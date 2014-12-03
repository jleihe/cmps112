#!/usr/bin/perl
# $Id: str2time.perl,v 1.3 2009-02-25 14:56:07-08 - - $

use HTTP::Date qw (str2time);
use POSIX qw (strftime);

$input = "@ARGV";
$time = str2time $input;
$date = strftime "%a %b %e %H:%M:%S %Z %Y", localtime $time;

print "Input: $input\n";
print "Time:  $time\n";
print "Date:  $date\n";

