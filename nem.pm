use strict;
use warnings;
use RRDs;
use POSIX;

use Net::SNMP;
use Math::BigInt;
use Time::HiRes;

package nem;

$nem::store_dir = "$nem::dir/store";
$nem::graph_dir = "$nem::dir/graph";
$nem::style = q[
    <!--
    input {
            font-family: 'Letter Gothic', Courier
    }
    -->
];
$nem::min_refresh = 60;
$nem::refresh_slop = 7;
$nem::step = 5 * 60;
$nem::hb = $nem::step * 2;

$nem::in1 = '#00CC00';        # light green
$nem::in2 = '#006600';        # dark green
$nem::ou1 = '#8470FF';        # light slate blue
$nem::ou2 = '#483D8B';        # dark slate blue
$nem::dis = '#FFA500';        # orange
$nem::err = '#FF0000';        # red

$nem::conf = { };

BEGIN {
    my @unused = ($nem::style, $nem::min_refresh, $nem::refresh_slop);
}

################################################################ nem::read_conf

sub find_ne {
    my ($nes, $find) = @_;

    foreach my $ne (@$nes) {
        if (defined $find) {
            next unless defined $ne->{session};
            if (!defined $ne->{session}->{-hostname}) {
                my ($p,$f,$l) = caller;
                &print_ne($ne);
                die "find_ne: no hostname ($p,$f,$l)";
            }
            return $ne if $find eq $ne->{session}->{-hostname};
        } else {
            return $ne if !defined $ne->{session};
        }
    }
    return undef;
}

sub find_or_make_ne {
    my ($nes, $find) = @_;
    my $ne = find_ne($nes, $find);

    return $ne if defined $ne;
    $ne = {
        targets => [ ]
    };
    push @$nes, $ne;
    return $ne;
}

sub read_conf {
    my ($conf) = @_;
    my $debug_conf = 0;
    my $nes = [ ];
    my $ne = undef;
    my $partial;
    local ($_);

    $_ = -M "$nem::dir/conf/$conf.nem";
    if (!defined $nem::conf->{$conf}) {
        $nem::conf->{$conf} = { };
    } else {
        return $nem::conf->{$conf}->{nes}
            if $nem::conf->{$conf}->{age} == $_;
    }
    $nem::conf->{$conf}->{age} = $_;

    return undef unless open('conf', "<$nem::dir/conf/$conf.nem");

    $partial = '';
    while (<conf>) {
        chop;
        s/#.*$//o;
        s/\s+/ /go;
        s/\s$//o;
        next unless length;

        $partial .= $_;
        if (/\\$/o) {
            chop $partial;
            next;
        }
        $_ = $partial;
        $partial = '';

        if ($_ eq '[]') {
            $ne = &find_or_make_ne($nes, undef);
            print STDERR "# []\n"
                if $debug_conf;
        } elsif (/^\[([\w\-\_\.]+)(\:(\d+)){0,1}\s+([^\s]+)\s+(1|2|X)\]$/) {
            my ($hostname, $port, $community, $version) =
                ($1, $3, $4, $5);
            $port = 161 unless defined $port;
            $ne = &find_or_make_ne($nes, $hostname);
            $ne->{session} = {
                -hostname => $hostname,
                -port => $port,
                -version => $version,
                -community => $community
            };
            print STDERR "# [$hostname:$port $community $version]\n"
                if $debug_conf;
        } elsif (/^\s+([\w\-\_\.]+)\s+([\w\-\_\.]+)/) {
            if (!defined $ne) {
                print STDERR "? <conf>:$#: ne-less target?\n";
                next;
            }
            my ($id, $type) = ($1, $2);
            $_ = $';
            my $target = {
                id => $id,
                vars => eval '\&'.$type.'_vars',
                proc => eval '\&'.$type.'_proc'
            };
            if ($type =~ /^(if|ifx|qfx|apn|cif|csi)$/) {
                if (/\s+(\d+)$/) {
                    my $inst = $1;
                    $target->{type} = 'port';
                    $target->{args} = [ $inst ];
                    print STDERR "#  $id $type $inst\n"
                        if $debug_conf;
                } else {
                    print STDERR "? <conf>:$#: ",
                        "bad inst in '$_'\n";
                    next;
                }
            } elsif ($type =~ /^(apnchas|cischas|extchas|naschas)$/) {
                my $model;
                if (/\s+(\d+)$/) {
                    $model = $1;
                } else {
                    print STDERR "? <conf>:$#: ",
                        "bad model '$1'\n";
                    next;
                }
                $target->{type} = 'chassis';
                $target->{args} = [ $model ];
                print STDERR "#  $id $type\n"
                    if $debug_conf;
            } elsif ($type =~ /^(sumport)$/ &&
                 !defined $ne->{session}) {
                $target->{type} = $type;
                $target->{args} = [ split ];
                print STDERR "#  $id $type $_\n"
                    if $debug_conf;
            } else {
                print STDERR "? <conf>:$#: ",
                    "bad type '$type'\n";
                next;
            }
            push @{$ne->{targets}}, $target;
        } else {
            print STDERR "? <conf>:$#: '$_'\n";
        }
    }
    close('conf');
    $nem::conf->{$conf}->{nes} = $nes;
    return $nes;
}

sub print_ne {
    my ($ne) = @_;

    printf STDERR "print_ne: {%s}\n", join(',', keys %$ne);
    foreach my $target (@{$ne->{targets}}) {
        printf STDERR "print_ne(target): {%s} %s %s %s\n",
            join(',', keys %$target),
            $target->{id}, $target->{type},
            join(',', @{$target->{args}});
    }
    if (defined $ne->{session}) {
        foreach my $key (keys %{$ne->{session}}) {
            printf STDERR "print_ne(session): %s=%s\n",
                $key, $ne->{session}->{$key};
        }
    }
}

sub print_conf {
    my ($nes, $tag) = @_;

    print STDERR "print_conf($tag): +++\n";
    foreach my $ne (@$nes) {
        &print_ne($ne);
    }
    print STDERR "print_conf($tag): ---\n";
}

############################################################ nem::find_target()

sub find_target {
    my ($id, $nes) = @_;

    foreach my $ne (@$nes) {
        foreach my $target (@{$ne->{targets}}) {
            return $target if $target->{id} eq $id;
        }
    }
    return undef;
}

################################################################ nem::collect()

sub collect {
    my ($n, $nes, $t) = @_;
    my $collect_debug = 0;

    my $nem_retries = defined($ENV{NEM_RETRIES}) ? $ENV{NEM_RETRIES} : 3;
    my $nem_timeout = defined($ENV{NEM_TIMEOUT}) ? $ENV{NEM_TIMEOUT} : 2.828;

N_E:
    foreach my $ne (@$nes) {
        next unless defined $ne->{session};
        next if ($ne->{session}->{-version} eq 'X');
        printf "# ne '%s'\n", $ne->{session}->{-hostname} if $collect_debug;
        my ($session, $error) = Net::SNMP->session(%{$ne->{session}});
        if (!defined $session) {
            printf STDERR "session() ERROR: %s.\n", $error;
            next N_E;
        }
        $session->translate(0);
        $session->retries($nem_retries);
        $session->timeout($nem_timeout);

        foreach my $target (@{$ne->{targets}}) {
            next unless $target->{type} =~ /^(port|chassis)$/;
            next if defined $t && $t ne $target->{id};
            my $time = time();
            my $args = $target->{args};
            printf "#\ttarg id '%s' type %s args (%s)\n",
                $target->{id}, $target->{type},
                join(' ', @$args)
                if $collect_debug;
            my $result = $session->get_request(
                -varbindlist => [ &{$target->{vars}}($args) ]
            );
            if (!defined $result) {
                if ($session->error !~ /No response from remote/) {
                    printf STDERR "get_request(%s %s) ERROR: %s\n",
                    $ne->{session}->{-hostname},
                    $target->{id},
                    $session->error;
                }
                $session->close;
                next N_E;
            }
            my $product = { };
            &{$target->{proc}}($args, $result, $product);
            printf "[%s %s %s %u]\n",
                $n, $target->{id}, $target->{type}, $time;
            foreach (sort keys %$product) {
                printf "\t%s %s\n", $_, $product->{$_};
            }
        }
        $session->close();
    }
}

################################################################ nem::store()

sub store {
    my ($meta, $cur) = @_;
    my $rrd = $nem::store_dir.'/'.$meta->{nem}.'/'.$meta->{id}.'.rrd';

    eval '&new_'.$meta->{type}.'($meta, $cur, $rrd)' unless -e $rrd;
    delete $cur->{speed} if $meta->{type} eq 'port';
    my @args = ($rrd, '-t', join(':', sort keys %$cur),
        join(':', $meta->{time}, @{$cur}{sort keys %$cur}));
    RRDs::update(@args);
    my $e = RRDs::error();
    print STDERR "$0: ERROR: Cannot update $meta->{type} $rrd:\n\t$e\n" if $e;
}

our @rra_list = (
    "RRA:AVERAGE:0.5:1:400",    # 5 min : ~1.3 days
    "RRA:AVERAGE:0.5:6:400",    # 30 min : ~8.3 days
    "RRA:AVERAGE:0.5:24:400",    # 2 hours : ~33.3 days
    "RRA:AVERAGE:0.5:288:400",    # 1 day : ~1.5 years
    "RRA:MAX:0.5:1:400",        # 5 min : ~1.3 days
    "RRA:MAX:0.5:6:400",        # 30 min : ~8.3 days
    "RRA:MAX:0.5:24:400",        # 2 hours : ~33.3 days
    "RRA:MAX:0.5:288:400"        # 1 day : ~1.5 years
);

sub new_port {
    my ($meta, $cur, $rrd) = @_;
    my $speed = $cur->{speed};
    $speed = 1000000000 unless defined $speed && $speed > 0;
    my $max_byte = $speed * 125000;
    my $max_pkt = $max_byte / 40;
    my @args = ($rrd, '-s', $nem::step, '-b', $meta->{time} - 10,
        "DS:in_dis:COUNTER:$nem::hb:0:$max_pkt",
        "DS:out_dis:COUNTER:$nem::hb:0:$max_pkt",
        "DS:in_err:COUNTER:$nem::hb:0:$max_pkt",
        "DS:out_err:COUNTER:$nem::hb:0:$max_pkt",
        "DS:in_uni:COUNTER:$nem::hb:0:$max_pkt",
        "DS:out_uni:COUNTER:$nem::hb:0:$max_pkt",
        "DS:in_nuni:COUNTER:$nem::hb:0:$max_pkt",
        "DS:out_nuni:COUNTER:$nem::hb:0:$max_pkt",
        "DS:in_oct:COUNTER:$nem::hb:0:$max_byte",
        "DS:out_oct:COUNTER:$nem::hb:0:$max_byte",
        @rra_list);
    &mkstoredir($meta->{nem});
    RRDs::create(@args);
    my $e = RRDs::error();
    die "ERROR: Cannot create logfile: $e\n" if $e;
    print STDERR "$0: new RRD '$rrd'\n";
}

sub new_chassis {
    my ($meta, $cur, $rrd) = @_;
    my @args = ($rrd, '-s', $nem::step, '-b', $meta->{time} - 10,
        "DS:cpu:GAUGE:$nem::hb:0:100",
        "DS:temp1:GAUGE:$nem::hb:0:212",
        "DS:temp2:GAUGE:$nem::hb:0:212",
        "DS:temp3:GAUGE:$nem::hb:0:212",
        "DS:temp4:GAUGE:$nem::hb:0:212",
        "DS:free1:GAUGE:$nem::hb:0:U",
        "DS:free2:GAUGE:$nem::hb:0:U",
        "DS:free3:GAUGE:$nem::hb:0:U",
        "DS:frag1:GAUGE:$nem::hb:0:U",
        "DS:frag2:GAUGE:$nem::hb:0:U",
        "DS:frag3:GAUGE:$nem::hb:0:U",
        @rra_list);
    &mkstoredir($meta->{nem});
    RRDs::create(@args);
    my $e = RRDs::error();
    die "ERROR: Cannot create logfile: $e\n" if $e;
    print STDERR "$0: new RRD '$rrd'\n";
}

################################################################ nem::graph_*

sub mkgraphdir {
    my ($n) = @_;

    mkdir $nem::graph_dir;
    mkdir $nem::graph_dir.'/'.$n;
}

sub mkstoredir {
    my ($n) = @_;

    mkdir $nem::store_dir;
    mkdir $nem::store_dir.'/'.$n;
}

sub graph_any {
    my ($n, $c, $r, $u, $t, $inout) = @_;
    my $ids;

    if ($t->{type} eq 'port') {
        $ids = [ $c ];
    } elsif ($t->{type} eq 'sumport') {
        $ids = $t->{args};
    } else {
        return { error => 'bad target type '.$t->{type} };
    }

    if (!defined $u) {
        return { error => 'no graph type' };
    } elsif ($u eq 'bits') {
        return &graph_bits($n, $c, $r, $ids, $inout);
    } elsif ($u eq 'pkts') {
        return &graph_pkts($n, $c, $r, $ids, $inout);
    } elsif ($u eq 'errs') {
        return &graph_errs($n, $c, $r, $ids, $inout);
    }
    return { error => 'bad graph units "'.$u.'"' };
}

sub mk_cdefvar {
    my ($var) = @_;

    return "$var,UN,0,$var,IF";
}

sub mk_graphvar {
    my ($n, $var, $ds, $cf, $ids) = @_;
    my @ret = ();
    my $x;

    $x = 0;
    foreach my $c (@$ids) {
        my $neg = 0;
        if ($c =~ /^\-/) {
            $neg = 1;
            $c = $';
        }
        my $rrd = $nem::store_dir.'/'.$n.'/'.$c.'.rrd';
        push @ret,
            sprintf("DEF:%s__%d=%s:%s:%s",
                $var, $x++, $rrd, $ds, $cf
            );
    }

    my $cdef = sprintf("CDEF:%s=0", $var);
    $x = 0;
    foreach my $c (@$ids) {
        my $op = ($c =~ /^\-/) ? '-' : '+';
        my $inst = sprintf("%s__%d", $var, $x++);
        $cdef .= sprintf(",%s,%s", &mk_cdefvar($inst), $op)
    }
    push @ret, $cdef;
    return @ret;
}

sub rrd_mtime {
    my ($n, $ids) = @_;
    my $ret = undef;

    foreach my $c (@$ids) {
        my $rrd = $nem::store_dir.'/'.$n.'/'.$c.'.rrd';
        my $mtime = $^T - (86400 * -M $rrd);

        $ret = $mtime if (!defined $ret) || $ret > $mtime;
    }
    return $ret;
}

sub graph_bits {
    my ($n, $c, $r, $ids, $inout) = @_;

    my $info = graph_info($r);
    return { error => $info->{error} } if defined $info->{error};

    my $curbits_in = $inout->{$ids->[0]}->{in_bit} || 0.0;
    my $curbits_out = $inout->{$ids->[0]}->{out_bit} || 0.0;

    my $cur_in = sprintf("%5sb", $curbits_in);
    my $cur_out = sprintf("%5sb", $curbits_out);

    my $neg;
    if ($c =~ /^\-/) {
        $neg = 1;
        $c = $';
    }
    my $png_file = $nem::graph_dir.'/'.$n.'/'.$c.'.'.$r.'.bits.png';

    my @args = ( $png_file, '--lazy',
        '--title', "$c traffic (~1 $r, bits)",
        '--font', 'TITLE:10:',
        '--font', 'AXIS:8:',
        '--font', 'UNIT:8:',
        '--font', 'LEGEND:7:',
        '--vertical-label', "bits per second",
        '--start', $info->{start},
#        '--end', 0 - $nem::hb,
#        '--logarithmic',
        '--imgformat', 'PNG',
        '--step', $info->{step},
        '--width', 400,
        '--height', 150,
        '--alt-autoscale',
        &mk_graphvar($n, 'in_oct_avg', 'in_oct', 'AVERAGE', $ids),
        &mk_graphvar($n, 'out_oct_avg', 'out_oct', 'AVERAGE', $ids),
        &mk_graphvar($n, 'in_oct_max', 'in_oct', 'MAX', $ids),
        &mk_graphvar($n, 'out_oct_max', 'out_oct', 'MAX', $ids),
        'CDEF:in_bit_avg=in_oct_avg,8,*',
        'CDEF:in_bit_navg=0,in_bit_avg,-',
        'CDEF:out_bit_avg=out_oct_avg,8,*',
        'CDEF:in_bit_max=in_oct_max,8,*',
        'CDEF:in_bit_nmax=0,in_bit_max,-',
        'CDEF:out_bit_max=out_oct_max,8,*',
        "AREA:in_bit_nmax$nem::in2:max input",
        "AREA:in_bit_navg$nem::in1:avg input",
        "AREA:out_bit_max$nem::ou2:max output",
        "AREA:out_bit_avg$nem::ou1:avg output",
        "COMMENT:\\c",
        "COMMENT:(as of ".POSIX::strftime("%a %b %e %H\\:%M\\:%S %Y", gmtime())." GMT)\\c",
        "GPRINT:in_bit_max:MAX: IN  Max(Max)=%6.2lf%sb",
        "GPRINT:in_bit_max:AVERAGE:Avg(Max)=%6.2lf%sb",
        "GPRINT:in_bit_avg:MAX:Max(Avg)=%6.2lf%sb",
        "COMMENT:Cur(Avg)=$cur_in\\l",
        "GPRINT:out_bit_max:MAX:OUT  Max(Max)=%6.2lf%sb",
        "GPRINT:out_bit_max:AVERAGE:Avg(Max)=%6.2lf%sb",
        "GPRINT:out_bit_avg:MAX:Max(Avg)=%6.2lf%sb",
        "COMMENT:Cur(Avg)=$cur_out\\l"
    );
    &mkgraphdir($n);
    my ($output, $width, $height, $e) = rrd_graph(@args);
    return { error => 'Cannot create bit PNG ('.$e.')' } if $e;
    return { error => "No PNG produced ($png_file)" }
        unless -e $png_file;
    my $length = -s _;
    my $last_modified = POSIX::strftime("%a, %e %b %Y %H:%M:%S GMT",
        gmtime($^T - (86400 * -M _)));
    my $rrd_mtime = &rrd_mtime($n, $ids);
    my $expires = POSIX::strftime("%a, %e %b %Y %H:%M:%S GMT",
        gmtime($rrd_mtime + $info->{step}));
    my $headers = {
        -type => 'image/png',
        -expires => $expires,
        -content_length => $length,
        -last_modified => $last_modified,
        -expires => $expires
    };
    return {
        headers => $headers,
        width => $width,
        height => $height,
        png_file => $png_file,
        rrd_mtime => $rrd_mtime,
        refresh => $rrd_mtime + $info->{step}
    };
}

sub inout_last {
    my ($n, $targets) = @_;

    my @args = ( '/dev/null',
        '--start', 0 - (2 * $nem::hb),
        '--step', 5 * 60
    );
    my $x = 0;
    foreach my $target (@$targets) {
        next unless $target->{type} eq 'port';
        $x++;
        my $c = $target->{id};
        my $rrd = $nem::store_dir.'/'.$n.'/'.$c.'.rrd';
        push @args,
             'DEF:in_oct_'.$x.'='.$rrd.':in_oct:AVERAGE',
             'DEF:out_oct_'.$x.'='.$rrd.':out_oct:AVERAGE',
             'DEF:in_dis_'.$x.'='.$rrd.':in_dis:AVERAGE',
             'DEF:out_dis_'.$x.'='.$rrd.':out_dis:AVERAGE',
             'DEF:in_err_'.$x.'='.$rrd.':in_err:AVERAGE',
             'DEF:out_err_'.$x.'='.$rrd.':out_err:AVERAGE',
             'DEF:in_uni_'.$x.'='.$rrd.':in_uni:AVERAGE',
             'DEF:out_uni_'.$x.'='.$rrd.':out_uni:AVERAGE',
             'DEF:in_nuni_'.$x.'='.$rrd.':in_nuni:AVERAGE',
             'DEF:out_nuni_'.$x.'='.$rrd.':out_nuni:AVERAGE',
             'CDEF:in_bit_'.$x.'=in_oct_'.$x.',8,*',
             'CDEF:out_bit_'.$x.'=out_oct_'.$x.',8,*',
             'CDEF:in_diserr_'.$x.'=in_err_'.$x.',in_dis_'.$x.',+',
             'CDEF:out_diserr_'.$x.'=out_err_'.$x.',out_dis_'.$x.',+',
             'PRINT:in_bit_'.$x.':LAST:'.$c.'/in_bit=%5.2lf%s',
             'PRINT:out_bit_'.$x.':LAST:'.$c.'/out_bit=%5.2lf%s',
             'PRINT:in_dis_'.$x.':LAST:'.$c.'/in_dis=%5.2lf%s',
             'PRINT:out_dis_'.$x.':LAST:'.$c.'/out_dis=%5.2lf%s',
             'PRINT:in_err_'.$x.':LAST:'.$c.'/in_err=%5.2lf%s',
             'PRINT:out_err_'.$x.':LAST:'.$c.'/out_err=%5.2lf%s',
             'PRINT:in_uni_'.$x.':LAST:'.$c.'/in_uni=%5.2lf%s',
             'PRINT:out_uni_'.$x.':LAST:'.$c.'/out_uni=%5.2lf%s',
             'PRINT:in_nuni_'.$x.':LAST:'.$c.'/in_nuni=%5.2lf%s',
             'PRINT:out_nuni_'.$x.':LAST:'.$c.'/out_nuni=%5.2lf%s',
             'PRINT:in_diserr_'.$x.':LAST:'.$c.'/in_diserr=%5.2lf%s',
             'PRINT:out_diserr_'.$x.':LAST:'.$c.'/out_diserr=%5.2lf%s';
    };
    return { } unless $x > 0;
    &mkgraphdir($n);
    my ($output, $width, $height, $e) = rrd_graph(@args);
    return { '.error' => join("\n",
        'Cannot get last inout bits ('.$e.')',
        @args
    ) } if $e;
    my $ret = { };
    foreach (@$output) {
        if (/^([\w\-\_\.]+)\/([\w\_]+)=(.*)$/) {
            $ret->{$1}->{$2} = $3;
        } else {
            print STDERR "RRDgraph ERROR#1: $_\n";
        }
    };
    return $ret;
}

sub graph_pkts {
    my ($n, $c, $r, $ids, $inout) = @_;

    my $info = graph_info($r);
    return { error => $info->{error} } if defined $info->{error};

    my $curuni_in = $inout->{$ids->[0]}->{in_uni} || 0.0;
    my $curuni_out = $inout->{$ids->[0]}->{out_uni} || 0.0;
    my $curnuni_in = $inout->{$ids->[0]}->{in_nuni} || 0.0;
    my $curnuni_out = $inout->{$ids->[0]}->{out_nuni} || 0.0;
    my $curdiserr_in = $inout->{$ids->[0]}->{in_diserr} || 0.0;
    my $curdiserr_out = $inout->{$ids->[0]}->{out_diserr} || 0.0;

    my $cur_uni_in = sprintf("%5s", $curuni_in);
    my $cur_uni_out = sprintf("%5s", $curuni_out);
    my $cur_nuni_in = sprintf("%5s", $curnuni_in);
    my $cur_nuni_out = sprintf("%5s", $curnuni_out);
    my $cur_diserr_in = sprintf("%5s", $curdiserr_in);
    my $cur_diserr_out = sprintf("%5s", $curdiserr_out);

    my $png_file = $nem::graph_dir.'/'.$n.'/'.$c.'.'.$r.'.pkts.png';

    my @args = ( $png_file, '--lazy',
        '--title', "$c traffic (~1 $r, pkts)",
        '--vertical-label', "pkts per second",
        '--start', $info->{start},
#        '--end', 0 - $nem::hb,
        '--imgformat', 'PNG',
        '--font', 'TITLE:10:',
        '--font', 'AXIS:8:',
        '--font', 'UNIT:8:',
        '--font', 'LEGEND:7:',
#        '--logarithmic',
        '--step', $info->{step},
        '--width', 400,
        '--height', 150,
        '--alt-autoscale',
        &mk_graphvar($n, 'in_dis_max', 'in_dis', 'MAX', $ids),
        &mk_graphvar($n, 'in_dis_avg', 'in_dis', 'AVERAGE', $ids),
        &mk_graphvar($n, 'in_err_max', 'in_err', 'MAX', $ids),
        &mk_graphvar($n, 'in_err_avg', 'in_err', 'AVERAGE', $ids),
        &mk_graphvar($n, 'in_uni_max', 'in_uni', 'MAX', $ids),
        &mk_graphvar($n, 'in_uni_avg', 'in_uni', 'AVERAGE', $ids),
        &mk_graphvar($n, 'in_nuni_max', 'in_nuni', 'MAX', $ids),
        &mk_graphvar($n, 'in_nuni_avg', 'in_nuni', 'AVERAGE', $ids),
        &mk_graphvar($n, 'out_dis_max', 'out_dis', 'MAX', $ids),
        &mk_graphvar($n, 'out_dis_avg', 'out_dis', 'AVERAGE', $ids),
        &mk_graphvar($n, 'out_err_max', 'out_err', 'MAX', $ids),
        &mk_graphvar($n, 'out_err_avg', 'out_err', 'AVERAGE', $ids),
        &mk_graphvar($n, 'out_uni_max', 'out_uni', 'MAX', $ids),
        &mk_graphvar($n, 'out_uni_avg', 'out_uni', 'AVERAGE', $ids),
        &mk_graphvar($n, 'out_nuni_max', 'out_nuni', 'MAX', $ids),
        &mk_graphvar($n, 'out_nuni_avg', 'out_nuni', 'AVERAGE', $ids),
        'CDEF:in_dis_nmax=0,in_dis_max,-',
        'CDEF:in_err_nmax=0,in_err_max,-',
        'CDEF:in_uni_nmax=0,in_uni_max,-',
        'CDEF:in_nuni_nmax=0,in_nuni_max,-',
        'CDEF:in_diserr_max=in_dis_max,in_err_max,+',
        'CDEF:out_diserr_max=out_dis_max,out_err_max,+',
        'CDEF:in_diserr_avg=in_dis_avg,in_err_avg,+',
        'CDEF:out_diserr_avg=out_dis_avg,out_err_avg,+',
        "COMMENT:INPUT",
        "AREA:in_uni_nmax$nem::in1:uni",
        "STACK:in_nuni_nmax$nem::in2:!uni",
        "STACK:in_dis_nmax$nem::dis:dis",
        "STACK:in_err_nmax$nem::err:err",
        "COMMENT:OUTPUT",
        "AREA:out_uni_max$nem::ou1:uni",
        "STACK:out_nuni_max$nem::ou2:!uni",
        "STACK:out_dis_max$nem::dis:dis",
        "STACK:out_err_max$nem::err:err",
        "COMMENT:\\c",
        "COMMENT:(as of ".POSIX::strftime("%a %b %e %H\\:%M\\:%S %Y", gmtime())." GMT)\\c",
        "COMMENT:(In\\:Out)  ---Max (Max)---   ---Avg (Max)--- ",
        "COMMENT:---Max (Avg)---   ---Cur (Avg)---\\l",

        "GPRINT:in_uni_max:MAX: Unicast  %6.2lf%s\\:\\g",
        "GPRINT:out_uni_max:MAX:%6.2lf%s ",
        "GPRINT:in_uni_max:AVERAGE:%6.2lf%s\\:\\g",
        "GPRINT:out_uni_max:AVERAGE:%6.2lf%s ",
        "GPRINT:in_uni_avg:MAX:%6.2lf%s\\:\\g",
        "GPRINT:out_uni_avg:MAX:%6.2lf%s ",
        "COMMENT:$cur_uni_in\\:\\g",
        "COMMENT:$cur_uni_out\\l",

        "GPRINT:in_nuni_max:MAX:!Unicast  %6.2lf%s\\:\\g",
        "GPRINT:out_nuni_max:MAX:%6.2lf%s ",
        "GPRINT:in_nuni_max:AVERAGE:%6.2lf%s\\:\\g",
        "GPRINT:out_nuni_max:AVERAGE:%6.2lf%s ",
        "GPRINT:in_nuni_avg:MAX:%6.2lf%s\\:\\g",
        "GPRINT:out_nuni_avg:MAX:%6.2lf%s ",
        "COMMENT:$cur_nuni_in\\:\\g",
        "COMMENT:$cur_nuni_out\\l",

        "GPRINT:in_diserr_max:MAX: Dis+Err  %6.2lf%s\\:\\g",
        "GPRINT:out_diserr_max:MAX:%6.2lf%s ",
        "GPRINT:in_diserr_max:AVERAGE:%6.2lf%s\\:\\g",
        "GPRINT:out_diserr_max:AVERAGE:%6.2lf%s ",
        "GPRINT:in_diserr_avg:MAX:%6.2lf%s\\:\\g",
        "GPRINT:out_diserr_avg:MAX:%6.2lf%s ",
        "COMMENT:$cur_diserr_in\\:\\g",
        "COMMENT:$cur_diserr_out\\l"
    );
    &mkgraphdir($n);
    my ($output, $width, $height, $e) = rrd_graph(@args);
    return { error => 'Cannot create pkt PNG ('.$e.')' } if $e;
    return { error => "No PNG produced ($png_file)" }
        unless -e $png_file;
    my $length = -s _;
    my $last_modified = POSIX::strftime("%a, %e %b %Y %H:%M:%S GMT",
        gmtime($^T - (86400 * -M _)));
    my $rrd_mtime = &rrd_mtime($n, $ids);
    my $expires = POSIX::strftime("%a, %e %b %Y %H:%M:%S GMT",
        gmtime($rrd_mtime + $info->{step}));
    my $headers = {
        -type => 'image/png',
        -expires => $expires,
        -content_length => $length,
        -last_modified => $last_modified,
        -expires => $expires
    };
    return {
        headers => $headers,
        width => $width,
        height => $height,
        png_file => $png_file,
        rrd_mtime => $rrd_mtime,
        refresh => $rrd_mtime + $info->{step}
    };
}

sub graph_errs {
    my ($n, $c, $r, $ids, $inout) = @_;

    my $info = graph_info($r);
    return { error => $info->{error} } if defined $info->{error};

    my $curerr_in = $inout->{$ids->[0]}->{in_err} || 0.0;
    my $curerr_out = $inout->{$ids->[0]}->{out_err} || 0.0;
    my $curdis_in = $inout->{$ids->[0]}->{in_dis} || 0.0;
    my $curdis_out = $inout->{$ids->[0]}->{out_dis} || 0.0;

    my $cur_err_in = sprintf("%5s", $curerr_in);
    my $cur_err_out = sprintf("%5s", $curerr_out);
    my $cur_dis_in = sprintf("%5s", $curdis_in);
    my $cur_dis_out = sprintf("%5s", $curdis_out);

    my $png_file = $nem::graph_dir.'/'.$n.'/'.$c.'.'.$r.'.errs.png';

    my @args = ( $png_file, '--lazy',
        '--title', "$c traffic (~1 $r, errs/discards)",
        '--vertical-label', "pkts per second",
        '--start', $info->{start},
#        '--end', 0 - $nem::hb,
        '--imgformat', 'PNG',
        '--font', 'TITLE:10:',
        '--font', 'AXIS:8:',
        '--font', 'UNIT:8:',
        '--font', 'LEGEND:7:',
#        '--logarithmic',
        '--step', $info->{step},
        '--width', 400,
        '--height', 150,
        '--alt-autoscale',
        &mk_graphvar($n, 'in_dis_max', 'in_dis', 'MAX', $ids),
        &mk_graphvar($n, 'in_dis_avg', 'in_dis', 'AVERAGE', $ids),
        &mk_graphvar($n, 'in_err_max', 'in_err', 'MAX', $ids),
        &mk_graphvar($n, 'in_err_avg', 'in_err', 'AVERAGE', $ids),
        &mk_graphvar($n, 'out_dis_max', 'out_dis', 'MAX', $ids),
        &mk_graphvar($n, 'out_dis_avg', 'out_dis', 'AVERAGE', $ids),
        &mk_graphvar($n, 'out_err_max', 'out_err', 'MAX', $ids),
        &mk_graphvar($n, 'out_err_avg', 'out_err', 'AVERAGE', $ids),
        'CDEF:in_dis_nmax=0,in_dis_max,-',
        'CDEF:in_err_nmax=0,in_err_max,-',
        "COMMENT:INPUT",
        "AREA:in_dis_nmax$nem::dis:dis",
        "STACK:in_err_nmax$nem::err:err",
        "COMMENT:OUTPUT",
        "AREA:out_dis_max$nem::dis:dis",
        "STACK:out_err_max$nem::err:err",
        "COMMENT:\\c",
        "COMMENT:(as of ".POSIX::strftime("%a %b %e %H\\:%M\\:%S %Y", gmtime())." GMT)\\c",
        "COMMENT:(In\\:Out)  ---Max (Max)---   ---Avg (Max)--- ",
        "COMMENT:---Max (Avg)---   ---Cur (Avg)---\\l",

        "GPRINT:in_err_max:MAX:     Err  %6.2lf%s\\:\\g",
        "GPRINT:out_err_max:MAX:%6.2lf%s ",
        "GPRINT:in_err_max:AVERAGE:%6.2lf%s\\:\\g",
        "GPRINT:out_err_max:AVERAGE:%6.2lf%s ",
        "GPRINT:in_err_avg:MAX:%6.2lf%s\\:\\g",
        "GPRINT:out_err_avg:MAX:%6.2lf%s ",
        "COMMENT:$cur_err_in\\:\\g",
        "COMMENT:$cur_err_out\\l",

        "GPRINT:in_dis_max:MAX:     Dis  %6.2lf%s\\:\\g",
        "GPRINT:out_dis_max:MAX:%6.2lf%s ",
        "GPRINT:in_dis_max:AVERAGE:%6.2lf%s\\:\\g",
        "GPRINT:out_dis_max:AVERAGE:%6.2lf%s ",
        "GPRINT:in_dis_avg:MAX:%6.2lf%s\\:\\g",
        "GPRINT:out_dis_avg:MAX:%6.2lf%s ",
        "COMMENT:$cur_dis_in\\:\\g",
        "COMMENT:$cur_dis_out\\l"
    );
    &mkgraphdir($n);
    my ($output, $width, $height, $e) = rrd_graph(@args);
    return { error => 'Cannot create err PNG ('.$e.')' } if $e;
    return { error => "No PNG produced ($png_file)" }
        unless -e $png_file;
    my $length = -s _;
    my $last_modified = POSIX::strftime("%a, %e %b %Y %H:%M:%S GMT",
        gmtime($^T - (86400 * -M _)));
    my $rrd_mtime = &rrd_mtime($n, $ids);
    my $expires = POSIX::strftime("%a, %e %b %Y %H:%M:%S GMT",
        gmtime($rrd_mtime + $info->{step}));
    my $headers = {
        -type => 'image/png',
        -expires => $expires,
        -content_length => $length,
        -last_modified => $last_modified,
        -expires => $expires
    };
    return {
        headers => $headers,
        width => $width,
        height => $height,
        png_file => $png_file,
        rrd_mtime => $rrd_mtime,
        refresh => $rrd_mtime + $info->{step}
    };
}

sub graph_info {
    my ($r) = @_;

    if ($r eq 'day') {
        # 5*1*400*60 (~1.3 days)
        return { start => -120000, step => 5*60 };
    } elsif ($r eq 'week') {
        # 5*6*400*60 (~8.3 days)
        return { start => -720000, step => 30*60 };
    } elsif ($r eq 'month') {
        # 5*24*400*60 (~33.3 days)
        return { start => -2880000, step => 2*60*60 };
    } elsif ($r eq 'year') {
        # 5*288*400*60 (~1.5 years)
        return { start => -34560000, step => 24*60*60 };
    } elsif (!defined $r) {
        return { error => "missing range" };
    }
    return { error => 'bad range ('.$r.')' };
}

sub rrd_graph {
    my @args = @_;

    my $old_tz = $ENV{TZ};
    $ENV{TZ} = 'GMT'; POSIX::tzset();
    my ($output, $width, $height) = RRDs::graph(@args);
    if (defined $old_tz) {
        $ENV{TZ} = $old_tz; POSIX::tzset();
    }
    my $e = RRDs::error();
    return ($output, $width, $height, $e);
}

################################################################ if_*
################################################################ SNMP V1

my $if_entry = '.1.3.6.1.2.1.2.2.1';
my $if_descr = "$if_entry.2";
my $if_speed = "$if_entry.5";
my $if_inoctets = "$if_entry.10";
my $if_inucast = "$if_entry.11";
my $if_innucast = "$if_entry.12";
my $if_indiscard = "$if_entry.13";
my $if_inerrors = "$if_entry.14";
my $if_outoctets = "$if_entry.16";
my $if_outucast = "$if_entry.17";
my $if_outnucast = "$if_entry.18";
my $if_outdiscard = "$if_entry.19";
my $if_outerrors = "$if_entry.20";

sub if_vars_descr {
    my ($inst) = @_;

    return (
        "$if_descr.$inst"
        );
}

sub if_vars_speed {
    my ($inst) = @_;

    return (
        "$if_speed.$inst"
    );
}

sub if_vars_diserr {
    my ($inst) = @_;

    return (
        "$if_indiscard.$inst",
        "$if_outdiscard.$inst",
        "$if_inerrors.$inst",
        "$if_outerrors.$inst",
    );
}

sub if_vars_nucast {
    my ($inst) = @_;

    return (
        "$if_innucast.$inst",
        "$if_outnucast.$inst"
    );
}

sub if_vars_ucast {
    my ($inst) = @_;

    return (
        "$if_inucast.$inst",
        "$if_outucast.$inst"
    );
}

sub if_vars_octet {
    my ($inst) = @_;

    return (
        "$if_inoctets.$inst",
        "$if_outoctets.$inst"
    );
}

sub if_vars {
    my ($args) = @_;
    my ($inst) = @$args;

    return (
        &if_vars_descr($inst),
        &if_vars_speed($inst),
        &if_vars_diserr($inst),
        &if_vars_nucast($inst),
        &if_vars_ucast($inst),
        &if_vars_octet($inst)
    );
}

sub if_proc_descr {
    my ($inst, $res, $prod) = @_;

    $prod->{descr} = $res->{"$if_descr.$inst"};
}

sub if_proc_speed {
    my ($inst, $res, $prod) = @_;

    $prod->{speed} = new Math::BigInt $res->{"$if_speed.$inst"};
    $prod->{speed}->badd( 999999);
    $prod->{speed}->bdiv(1000000);
}

sub if_proc_diserr {
    my ($inst, $res, $prod) = @_;

    $prod->{in_dis} = $res->{"$if_indiscard.$inst"};
    $prod->{out_dis} = $res->{"$if_outdiscard.$inst"};

    $prod->{in_err} = $res->{"$if_inerrors.$inst"};
    $prod->{out_err} = $res->{"$if_outerrors.$inst"};
}

sub if_proc_nucast {
    my ($inst, $res, $prod) = @_;

    $prod->{in_nuni} = $res->{"$if_innucast.$inst"};
    $prod->{out_nuni} = $res->{"$if_outnucast.$inst"};
}

sub if_proc_ucast {
    my ($inst, $res, $prod) = @_;

    $prod->{in_uni} = $res->{"$if_inucast.$inst"};
    $prod->{out_uni} = $res->{"$if_outucast.$inst"};
}

sub if_proc_octet {
    my ($inst, $res, $prod) = @_;

    $prod->{in_oct} = $res->{"$if_inoctets.$inst"};
    $prod->{out_oct} = $res->{"$if_outoctets.$inst"};
}

sub if_proc {
    my ($args, $res, $prod) = @_;
    my ($inst) = @$args;

    &if_proc_speed($inst, $res, $prod);
    &if_proc_diserr($inst, $res, $prod);
    &if_proc_nucast($inst, $res, $prod);
    &if_proc_ucast($inst, $res, $prod);
    &if_proc_octet($inst, $res, $prod);
}

################################################################ ifx_*
################################################################ SNMP V2

my $ifx_entry = '.1.3.6.1.2.1.31.1.1.1';
my $ifx_inoctets = "$ifx_entry.6";
my $ifx_inucast = "$ifx_entry.7";
my $ifx_inmulti = "$ifx_entry.8";
my $ifx_inbroad = "$ifx_entry.9";
my $ifx_outoctets = "$ifx_entry.10";
my $ifx_outucast = "$ifx_entry.11";
my $ifx_outmulti = "$ifx_entry.12";
my $ifx_outbroad = "$ifx_entry.13";
my $ifx_speed = "$ifx_entry.15";

sub ifx_vars_speed {
    my ($inst) = @_;

    return (
        "$ifx_speed.$inst"
    );
}

sub ifx_vars_ucast {
    my ($inst) = @_;

    return (
        "$ifx_inucast.$inst",
        "$ifx_outucast.$inst"
    );
}

sub ifx_vars_nucast {
    my ($inst) = @_;

    return (
        "$ifx_inmulti.$inst",
        "$ifx_outmulti.$inst",
        "$ifx_inbroad.$inst",
        "$ifx_outbroad.$inst"
    );
}

sub ifx_vars_octet {
    my ($inst) = @_;

    return (
        "$ifx_inoctets.$inst",
        "$ifx_outoctets.$inst",
    );
}

sub ifx_vars {
    my ($args) = @_;
    my ($inst) = @$args;

    return (
        &ifx_vars_speed($inst),
        &if_vars_diserr($inst),
        &ifx_vars_ucast($inst),
        &ifx_vars_nucast($inst),
        &ifx_vars_octet($inst)
    );
}

sub ifx_proc_speed {
    my ($inst, $res, $prod) = @_;

    $prod->{speed} = new Math::BigInt $res->{"$ifx_speed.$inst"};
}

sub ifx_proc_ucast {
    my ($inst, $res, $prod) = @_;

    $prod->{in_uni} = new Math::BigInt $res->{"$ifx_inucast.$inst"};
    $prod->{out_uni} = new Math::BigInt $res->{"$ifx_outucast.$inst"};
}

sub ifx_proc_nucast {
    my ($inst, $res, $prod) = @_;

    $prod->{in_nuni} = new Math::BigInt $res->{"$ifx_inbroad.$inst"};
    $prod->{in_nuni}->badd($res->{"$ifx_inmulti.$inst"});

    $prod->{out_nuni} = new Math::BigInt $res->{"$ifx_outbroad.$inst"};
    $prod->{out_nuni}->badd($res->{"$ifx_outbroad.$inst"});
}

sub ifx_proc_octet {
    my ($inst, $res, $prod) = @_;

    $prod->{in_oct} = new Math::BigInt $res->{"$ifx_inoctets.$inst"};
    $prod->{out_oct} = new Math::BigInt $res->{"$ifx_outoctets.$inst"};
}

sub ifx_proc {
    my ($args, $res, $prod) = @_;
    my ($inst) = @$args;

    &ifx_proc_speed($inst, $res, $prod);
    &if_proc_diserr($inst, $res, $prod);
    &ifx_proc_ucast($inst, $res, $prod);
    &ifx_proc_nucast($inst, $res, $prod);
    &ifx_proc_octet($inst, $res, $prod);
}

################################################################ qfx_*
################################################################ Juniper QFX

# use the jnxCosQstatTotalDropPkts counter from JUNIPER-COS-MIB:: as the
# number for ifOutDiscards

my $qfx_entry = '.1.3.6.1.4.1.2636.3.15.4.1';
my $qfx_outdiscard = "$qfx_entry.53";

sub qfx_vars_diserr {
    my ($inst) = @_;

    return (
        "$if_indiscard.$inst",
        "$qfx_outdiscard.$inst.0",
        "$if_inerrors.$inst",
        "$if_outerrors.$inst",
    );
}

sub qfx_vars {
    my ($args) = @_;
    my ($inst) = @$args;

    return (
        &ifx_vars_speed($inst),
        &qfx_vars_diserr($inst),
        &ifx_vars_ucast($inst),
        &ifx_vars_nucast($inst),
        &ifx_vars_octet($inst)
    );
}

sub qfx_proc_diserr {
    my ($inst, $res, $prod) = @_;

    $prod->{in_dis} = $res->{"$if_indiscard.$inst"};
    $prod->{out_dis} = new Math::BigInt $res->{"$qfx_outdiscard.$inst.0"};

    $prod->{in_err} = $res->{"$if_inerrors.$inst"};
    $prod->{out_err} = $res->{"$if_outerrors.$inst"};
}


sub qfx_proc {
    my ($args, $res, $prod) = @_;
    my ($inst) = @$args;

    &ifx_proc_speed($inst, $res, $prod);
    &qfx_proc_diserr($inst, $res, $prod);
    &ifx_proc_ucast($inst, $res, $prod);
    &ifx_proc_nucast($inst, $res, $prod);
    &ifx_proc_octet($inst, $res, $prod);
}


################################################################ apn_*
################################################################ Alcatel
################################################################ PowerNode

my $apn_extstats = '.1.3.6.1.4.1.3003.1.7.2.1';
my $apn_rxhigh = "$apn_extstats.2";
my $apn_rxlow = "$apn_extstats.3";
my $apn_txhigh = "$apn_extstats.4";
my $apn_txlow = "$apn_extstats.5";

sub apn_vars {
    my ($args) = @_;
    my ($inst) = @$args;

    return (
        &if_vars_speed($inst),
        &if_vars_diserr($inst),
        &if_vars_nucast($inst),
        &if_vars_ucast($inst),
        "$apn_rxhigh.$inst",
        "$apn_rxlow.$inst",
        "$apn_txhigh.$inst",
        "$apn_txlow.$inst"
    );
}

sub apn_proc {
    my ($args, $res, $prod) = @_;
    my ($inst) = @$args;

    &if_proc_speed($inst, $res, $prod);
    &if_proc_diserr($inst, $res, $prod);
    &if_proc_nucast($inst, $res, $prod);
    &if_proc_ucast($inst, $res, $prod);

    $prod->{in_oct} = new Math::BigInt $res->{"$apn_rxhigh.$inst"};
    $prod->{in_oct}->blsft(32);
    $prod->{in_oct}->bior($res->{"$apn_rxlow.$inst"});

    $prod->{out_oct} = new Math::BigInt $res->{"$apn_txhigh.$inst"};
    $prod->{out_oct}->blsft(32);
    $prod->{out_oct}->bior($res->{"$apn_txlow.$inst"});
}

my $apn_environ = '.1.3.6.1.4.1.3003.2.2.4.1.2.1.3';
my $apn_temp1 = "$apn_environ.1";
my $apn_temp2 = "$apn_environ.2";

sub apnchas_vars {
    my ($args) = @_;

    return (
        $apn_temp1,
        $apn_temp2
    );
}

sub apnchas_proc {
    my ($args, $res, $prod) = @_;

    $prod->{temp1} = $res->{$apn_temp1};
    $prod->{temp2} = $res->{$apn_temp2};
}

################################################################ extchas_*
################################################################ Purple Chassis

my $extchas_temp = '.1.3.6.1.4.1.1916.1.1.1.8.0';

sub extchas_vars {
    my ($args) = @_;

    return (
        $extchas_temp
    );
}

sub extchas_proc {
    my ($args, $res, $prod) = @_;

    my $f = (($res->{$extchas_temp} * 9) / 5) + 32;
    $prod->{temp1} = int($f + 0.999);
}

################################################################ naschas_*
################################################################ ReadyNAS

my $naschas_temp = '.1.3.6.1.4.1.4526.18.3.1.5';
my $naschas_temp1 = $naschas_temp.'.1';
my $naschas_temp2 = $naschas_temp.'.2';
my $naschas_temp3 = $naschas_temp.'.3';
my $naschas_temp4 = $naschas_temp.'.4';

sub naschas_vars {
    my ($args) = @_;

    return (
    $naschas_temp1,
    $naschas_temp2,
    $naschas_temp3,
    $naschas_temp4
    );
}

sub naschas_proc {
    my ($args, $res, $prod) = @_;

    if ($res->{$naschas_temp1} =~ /^\d+C\/(\d+)F$/) {
        $prod->{temp1} = $1;
        } elsif ($res->{$naschas_temp1} =~ /(\d+)$/) {
        $prod->{temp1} = $1;
    }
    if ($res->{$naschas_temp2} =~ /^\d+C\/(\d+)F$/) {
        $prod->{temp2} = $1;
        } elsif ($res->{$naschas_temp2} =~ /(\d+)$/) {
        $prod->{temp2} = $1;
    }
    if ($res->{$naschas_temp3} =~ /^\d+C\/(\d+)F$/) {
        $prod->{temp3} = $1;
        } elsif ($res->{$naschas_temp3} =~ /(\d+)$/) {
        $prod->{temp3} = $1;
    }
    if ($res->{$naschas_temp4} =~ /^\d+C\/(\d+)F$/) {
        $prod->{temp4} = $1;
        } elsif ($res->{$naschas_temp4} =~ /(\d+)$/) {
        $prod->{temp4} = $1;
    }
}

################################################################ cischas_*
################################################################ Cisco Chassis

my $cischas_temp = '.1.3.6.1.4.1.9.9.13.1.3.1.3';
my $cischas_cpu = '.1.3.6.1.4.1.9.2.1.58.0';
my $cischas_free = '.1.3.6.1.4.1.9.9.48.1.1.1.6';
my $cischas_frag = '.1.3.6.1.4.1.9.9.48.1.1.1.7';

sub cischas_info {
    my ($model) = @_;

    if ($model =~ /^72\d\d$/) {
        return { numtemp => 4, mempools => [ 1, 2, 8 ] };
    } elsif ($model =~ /^73\d\d$/) {
        return { numtemp => 2, mempools => [ 1, 2 ] };
    } elsif ($model =~ /^75\d\d$/) {
        return { numtemp => 3, mempools => [ 1, 4 ] };
    } else {
        return { numtemp => 0, mempools => [ ] };
    }
}

sub cischas_vars {
    my ($args) = @_;
    my ($model) = @$args;
    my $info = &cischas_info($model);
    my @ret;
    local ($_);

    @ret = ( $cischas_cpu );
    for ($_ = 1; $_ <= $info->{numtemp}; $_++) {
        push @ret, "$cischas_temp.$_";
    }
    foreach (@{$info->{mempools}}) {
        push @ret, "$cischas_free.$_";
        push @ret, "$cischas_frag.$_";
    }
    return @ret;
}

sub cischas_proc {
    my ($args, $res, $prod) = @_;
    my ($model) = @$args;
    my $info = &cischas_info($model);
    local ($_);

    $prod->{cpu} = $res->{$cischas_cpu};
    for ($_ = 1; $_ <= $info->{numtemp}; $_++) {
        next unless defined $res->{"$cischas_temp.$_"} &&
                $res->{"$cischas_temp.$_"} =~ /^\d+$/;
        my $f = (($res->{"$cischas_temp.$_"} * 9) / 5) + 32;
        $prod->{"temp$_"} = int($f + 0.999);
    }
    my $n = 1;
    foreach (sort { $a <=> $b } @{$info->{mempools}}) {
        next unless defined $res->{"$cischas_free.$_"} &&
                defined $res->{"$cischas_frag.$_"};
        $prod->{"free$n"} = $res->{"$cischas_free.$_"};
        $prod->{"frag$n"} = $res->{"$cischas_frag.$_"};
        $n++;
    }
}

################################################################ cif_*
################################################################ Cisco IF

sub cif_vars {
    my ($args) = @_;
    my ($inst) = @$args;

    return (
        &ifx_vars_speed($inst),
        &if_vars_diserr($inst),
        &ifx_vars_ucast($inst),
        &if_vars_nucast($inst),
        &ifx_vars_octet($inst)
    );
}

sub cif_proc {
    my ($args, $res, $prod) = @_;
    my ($inst) = @$args;

    &ifx_proc_speed($inst, $res, $prod);
    &if_proc_diserr($inst, $res, $prod);
    &ifx_proc_ucast($inst, $res, $prod);
    &if_proc_nucast($inst, $res, $prod);
    &ifx_proc_octet($inst, $res, $prod);
}

################################################################ csi_*
################################################################ Cisco SubIf

sub csi_vars {
    my ($args) = @_;
    my ($inst) = @$args;

    return (
        &ifx_vars_speed($inst),
        &ifx_vars_ucast($inst),
        &ifx_vars_nucast($inst),
        &ifx_vars_octet($inst)
    );
}

sub csi_proc {
    my ($args, $res, $prod) = @_;
    my ($inst) = @$args;

    &ifx_proc_speed($inst, $res, $prod);
    &ifx_proc_ucast($inst, $res, $prod);
    &ifx_proc_nucast($inst, $res, $prod);
    &ifx_proc_octet($inst, $res, $prod);
}

1;
