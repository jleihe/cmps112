#!/usr/bin/perl
# $Id: segment.perl,v 1.1 2011-12-19 19:47:33-08 - - $
#
# NAME
#    segment - print out a segment of a file
#
# SYNOPSIS
#    segment size number
#
# DESCRIPTION
#    Prints out a segment of a file of size `size'.  The number
#    gives the number of segments to ignore before printing.
#

$size = shift;
$number = shift;
while (defined ($line = <>)) {
   print $line if $. > $size * $number
              and $. <= $size * ($number + 1);
}
