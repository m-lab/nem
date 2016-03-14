#!/usr/bin/perl

use strict;
use warnings;

use lib ($nem::dir = ($ENV{'nem'} || '/udir/netmon/nem'));
use nem;

my $nemdebug = ($ENV{'nemdebug'} || 0);
my $valid = 0;
my $dirty = 0;
my ($meta, $cur);

umask(0002);

while (<STDIN>) {
    if (/^\[([\w\-\_]+)\s+([\w\-\_\.]+)\s+([\w\-\_]+)\s+(\d+)\]$/) {
        print STDERR "<1< $_" if $nemdebug;
        &nem::store($meta, $cur) if $dirty;
        $meta = {
            nem => $1,
            id => $2,
            type => $3,
            time => $4
        };
        $cur = { };
        $dirty = 0;
        $valid = 1;
    } elsif ($valid && /^\s+([\w\_\-]+)\s+\+*(\d+)$/) {
        print STDERR "<2< $_" if $nemdebug;
        $cur->{$1} = $2;
        $dirty = 1;
    } else {
        print STDERR "<3< $_" if $nemdebug;
        # maybe signal an error somehow here..
    }
}
&nem::store($meta, $cur) if $dirty;
exit (0);
