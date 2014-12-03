#!/usr/bin/perl
# $Id: nntpsave.perl,v 1.3 2009-09-24 16:20:04-07 - - $
#
# NAME
#    nntpsave - save newsgroup into a directory
#
# SYNOPSIS
#    nntpsave newsgroup directory [msgnumber]
#
# DESCRIPTION
#    Uses NNTP to log into the nntp server and extract all
#    articles starting with the given msgnumber.  If no
#    msgnumber is given, a file with the largest numeric
#    filename is found in the directory, and messages with
#    larger article numbers are saved.
#

use strict;
use warnings;
use Net::NNTP;
use HTTP::Date qw(str2time);

#################################################################
# Usage message.

sub usage() {
   print STDERR "Usage: $0 newsgroup directory [msgnumber]\n";
   exit 1;
}

#################################################################
# Boilerplate initialization.

$0 =~ s|^(.*/)?([^/]+)/*$|$2|;
sub note(@){ print STDERR "$0: @_"; };
$SIG{'__WARN__'} = sub{ note @_;; $! = 1; };
$SIG{'__DIE__'} = sub{ warn @_; exit; };

#################################################################
# Set default newshost and check on arguments.

my $newshost = "news.ucsc.edu";
usage unless @ARGV == 2 || @ARGV == 3 && $ARGV[2] =~ m/^\d+$/;

my $filepattern = '*.nn';
my $fileformat = '%05d.nn';
my $filesubst1 = '^.*/0*(.*)\.nn$';

#################################################################
# Find the most recent article number.

my ($newsgroup, $directory, $msgnumber) = @ARGV;
print "$0: $directory\n";
my $lastsaved = (sort {$b <=> $a}
                 grep {m/^\d+$/}
                 map {s|$filesubst1|$1|; $_}
                 glob "$directory/$filepattern")[0];
usage unless $lastsaved;

#################################################################
# Get the group article information.

my $nntp = Net::NNTP->new ($newshost, {Debug=> 1})
      or die "$0: $newshost unavailable to NNTP\n";
my ($nrarticles, $firstarticle, $lastarticle, $groupname) =
      $nntp->group ($newsgroup);
die "$0: $newsgroup not found" unless $groupname;
printf "%s: %s, last saved %d, last article %d\n",
       $0, $newsgroup, $lastsaved, $lastarticle;

#################################################################
# Loop over unsaved articles and save each one.

for my $mesgnr ($lastsaved + 1 .. $lastarticle) {
   my $article = $nntp->article ($mesgnr);
   unless ($article) {
      note "article $mesgnr of $newsgroup unavailable.\n";
      next;
   }
   my $filename = sprintf "%s/$fileformat", $directory, $mesgnr;
   open my $file, ">$filename" or die "open $filename: $!\n";
   print "$0: >$filename: OK\n";
   my $fromline = (grep {m/^From:\s/} @$article)[0];
   my $dateline = (grep {m/^Date:\s/} @$article)[0];
   map {s/^\S+:\s+//; chomp} $fromline, $dateline;
   print $file "From $fromline  $dateline\n",
               "Article: $mesgnr of $groupname\n",
               @$article;
   close $file;
   my $utime = str2time $dateline;
   utime $utime, $utime, $filename if $utime;
}

