#!perl
use strict;
use warnings;

print "This is Perl test\n";

sub add {
    $_[0] + $_[1];
}

print add(100, 200), "\n";
