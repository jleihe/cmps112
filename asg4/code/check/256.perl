#!/bin/perl
# $Id: 256.perl,v 1.1 2010-11-15 19:13:58-08 - - $
for $char (0..255) {
   printf "%c", $char;
   print "\n" if $char % 16 == 15;
}
