#!/usr/bin/perl
# $Id: touchbydate.perl,v 1.1 2008-11-19 14:58:34-08 - - $
#
#
# NAME
#    touchbydate - touch a mail or news file by Date: header
#
# SYNOPSIS
#    touchbydate [filename...]
#
# DESCRIPTION
#    Scan each file for a Date: header and touch the file based
#    on the interpretation of that string.
#

use strict;
use warnings;
use HTTP::Date qw(str2time);
use POSIX qw (strftime);

$0 =~ s|^(.*/)?([^/]+)/*$|$2|;
my $EXITCODE = 0;
END{ exit $EXITCODE; }
sub note(@){ print STDERR "$0: @_"; };
$SIG{'__WARN__'} = sub{ note @_;; $EXITCODE = 1; };
$SIG{'__DIE__'} = sub{ warn @_; exit; };

delete $ENV{LC_TIME};
for my $fname (@ARGV) {
   open my $file, "<$fname" or do {warn "$fname: $!\n"; next};
   my $rawdate = (grep {m/^Date:\s/} <$file>)[0];
   do {warn "$fname: no ^Date: string\n"; next} unless $rawdate;
   $rawdate =~ s/^Date:\s+(.*?)\s+$/$1/;
   my $time = str2time ($rawdate);
   do {warn "$fname: $rawdate: unparseable\n"; next} unless $time;
   #printf "%s: %d %s\n", $fname, $time, strftime "%c", localtime $time;
   utime $time, $time, $fname;
}

