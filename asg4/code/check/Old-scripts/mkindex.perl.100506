#!/bin/perl -w
# $Id: mkindex.perl,v 1.2 2010-05-06 17:47:48-07 - - $
#
# Make HEADER.html with Subjects for pointy-clicky types.
#

use strict;
use warnings;
$0 =~ s{^(.*/)?([^/]+)/*$}{$2};

use Cwd qw (realpath);
use HTTP::Date qw (str2time);
use POSIX qw (strftime);

sub printsystem(@){
   my( @command) = @_;
   print "$0: @command\n";
   system @command;
   print "========> status ", $? >> 8, ", signal ", $? & 127,
         ", core dumped" x ($? & 128), "\n" if $?;

};

sub formatdate($){
   my( $time) = @_;
   my $datefmt = abs( $time - time) / 86400 > 180
            ? "%b %e  %Y" : "%b %e %H:%M";
   return strftime $datefmt, localtime $time;
};

sub parsedate($){
   my ($date) = @_;
   return formatdate str2time $date;
};

sub filedate($){
   my ($file) = @_;
   my (@stat) = stat $file;
   return @stat ? formatdate $stat[9] : $!;
};

sub makeindex(){
   my $pwd = `pwd`;
   chomp $pwd;
   my $indexfile = ".index";
   my $tmpdir = $ENV{TMPDIR} || "/tmp";
   my $tmpindexfile = "$tmpdir/$indexfile";
   my @otherdirs;
   my @newsdirs;
   my @otherfiles;
   my @newsfiles;
   for my $file ("..", <*>) {
      my $filefmt = $file =~ m/^\d+$/ ? "%8s" : "%-8s";
      $filefmt = "<A HREF=%s>$filefmt  %-12s  %s</A>\n";
      my $filedate = filedate $file;
      next if $file eq $indexfile;
      if (-l $file) {
         push @otherfiles, sprintf $filefmt,
              $file, $file, $filedate, "-> " . readlink $file;
      }elsif (-d $file) {
         my @dirfiles = <$file/*>;
         my $dirfiles = sprintf "directory with %5d files",
                        scalar @dirfiles;
         my $fileline = sprintf $filefmt,
                        $file, "$file/", $filedate, $dirfiles;
         if ($file =~ m/^20\d{2}-(1w|2s|3u|4f)$/) {
            push @newsdirs, $fileline;
         }else{
            push @otherdirs, $fileline;
         };
      }elsif (-f $file) {
         my $subject = '';
         my $date = '';
         my $username = '';
         unless (open NEWS, "<$file") {
            $date = $!;
         }else{
            while (my $line = <NEWS>) {
               chomp $line;
               last if $. > 1 and $line =~ m/^\s*$/;
               $subject = $1 if ! $subject
                             and $line =~ m/^Subject:\s+(.*)/;
               $date = parsedate $1 if ! $date
                                    and $line =~ m/^Date:\s+(.*)/;
               $username = $1 if ! $username
                              and $line =~ m/^From:.*\s(\S+\@\S+)/;
               $username = $1 if ! $username
                              and $line =~ m/^From:\s+(.*)/;
               $username =~ s/[<>{}()\[\]]//g;
               $username =~ s/\.ucsc\.edu$//;
               $username =~ s/(\@ucsc)\.edu$/$1/;
            };
            close NEWS;
         };
         $date = $filedate unless $date;
         unless ($subject) {
            chomp ($subject = `file $file` );
            $subject =~ s/[^:]*:\s*//;
         };
         $subject =~ s/\s+/ /g;
         $subject =~ s/^\s*//;
         $subject =~ s/\s*$//;
         # my $descrip = $subject;
         $username =~ s/(.{14}).*/$1/;
         my $descrip = sprintf "%-14s  %s", $username, $subject;
         $descrip =~ s/(.{72}).*/$1/;
         my $fileline = sprintf $filefmt, $file, $file, $date, $descrip;
         if ($file =~ m/^\d+\.nn$/) {
            push @newsfiles, $fileline;
         }else{
            push @otherfiles, $fileline;
         };
      }else{
         print STDERR "$0: $file: file type not plain or directory\n";
      };
   };
   
   my $titlepwd = $pwd;
   $titlepwd =~ s{/afs/cats.ucsc.edu/courses/}{};
   my $newsfiles = @newsfiles;
   my $time = strftime "%Y-%m-%d %a %H:%M:%S %Z", localtime time;
   my $indexline = "$pwd -- $time -- $newsfiles news files";
   my $sepline = '=' x (length $indexline);
   #printsystem "mv $indexfile $tmpindexfile\n";
   open INDEX, ">$indexfile" or die "$0: $indexfile: $!";
   my $realpath = realpath ($indexfile);
   print "$0: open >$realpath: OK\n";
   print INDEX "<TITLE>$titlepwd</TITLE><PRE>\n",
               "<B>$sepline\n$indexline\n$sepline</B>\n",
               "\n",
               (sort {$b cmp $a} @newsfiles),
               (sort {$a cmp $b} @otherfiles),
               "\n",
               (sort {$b cmp $a} @newsdirs),
               (sort {$a cmp $b} @otherdirs),
               "\n",
               <DATA>,
               ;
   close INDEX;
   #printsystem "diff $indexfile $tmpindexfile | cut -c1-80";
};

sub renamenumberedfiles(){
   for my $file (grep m/^\d+$/, <*>) {
      my $newfile = sprintf "%05d", $file;
      next if $file eq $newfile;
      if (rename $file, $newfile) {
         print "$0: rename $file $newfile: OK\n";
      }else{
         die "$0: rename $file $newfile: $!";
      };
   };
};

renamenumberedfiles;
makeindex;

__DATA__

This directory contains archives of some articles posted to the class
newsgroup and most of the current quarter's postings.  However, there
is no automatic archiving, so new articles are only saved here when a
news saving program is explicitly run here.

To gain access to the lastest postings, use, for example:

.  trn -g ucsc.class.cmps012b

Note that if you have a News/.newsrc file already created, you can just
use the command rn.  Some people like to use other newsreaders, of
course, and you can feel free to use any.

DO NOT POST HTML OR MIME to the newsgroup.  It is obnoxious and will be
ignored.  Make sure your posting has the following attributes:

.  Content-Type: text/plain
.  Content-Transfer-Encoding: 8bit

When posting to a newsgroup, it is appropriate to post in text format
only and to make sure that all lines are at most 65 characters.
Paragraphs can be reformatted from inside vi using the command

.  {!}fmt -65

Lines you do not want formatted should being with a period.  Or don't
run fmt on those lines.

If you put the following commands

.  map ;f {!}fmt -c -65
.  set number

into your .exrc file, then the command ;f from inside vi will reformat
the paragraph with only two keystrokes.  The second command causes line
numbers to appear in the editor (not in your file) so that you can see
which lines you are working on.  Then you can perform a reformat with a
command such as:

.  :20,50!fmt -65

which will format lines 20 to 50.

