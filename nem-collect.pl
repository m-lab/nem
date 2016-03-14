#! /usr/bin/perl

use strict;
use warnings;

use lib ($nem::dir = ($ENV{'nem'} || '/udir/netmon/nem'));
use nem;

my $n;
foreach $n (@ARGV) {
    my $t;
    die "usage: $0 <conf> [...]" unless defined $n and length $n;
    ($n, $t) = split(/:/, $n);
    die "bad conf $n ($!)" unless defined (my $nes = &nem::read_conf($n));
    &nem::collect($n, $nes, $t);
}
exit 0;
