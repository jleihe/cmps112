/* $Id: pstatus.c,v 1.3 2010-02-11 14:10:04-08 - - $ */

/*
* Print out a status code or error message.
*/

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <ctype.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wait.h>

#define xfprintf (void) fprintf
#define xprintf (void) printf

void printsig (char *label, int signo, char *suffix) {
   char *sigstr = strsignal (signo);
   if (sigstr == NULL) sigstr = "Invalid Signal Number";
   xprintf ("%s %d: %s", label, signo, sigstr);
   if (suffix) xprintf (" (%s)", suffix);
}

long scanarg (int argc, char **argv) {
   char *endptr = NULL;
   long result;
   result = strtol (argc > 1 ? argv[1] : "", &endptr, 0);
   if (endptr == NULL || *endptr != '\0') {
      xfprintf (stderr, "Usage: %s status\n", basename (argv[0]));
      xfprintf (stderr, "argv[1] = \"%s\", bad chars = \"%s\"\n",
                argv[1], endptr);
   }
   return result;
}

int main (int argc, char **argv) {
   int argi;
   int status = scanarg (argc, argv);
   xprintf ("%s: 0x%04X ", basename (argv[0]), status);
   if (status < 0) {
      char *errstr = strerror (- status);
      if (errstr == NULL) errstr = "Invalid Error Number";
      xprintf ("ERROR %d: %s", - status, errstr);
   }else {
      if (WIFEXITED (status)) {
         xprintf ("EXIT STATUS = %d", WEXITSTATUS (status));
      }else if (WIFSIGNALED (status)) {
         printsig ("TERMINATED", WTERMSIG (status),
                   WCOREDUMP (status) ? "core dumped" : NULL);
      }else if (WIFSTOPPED (status)) {
         printsig ("STOPPED", WSTOPSIG (status), NULL);
      }else if (WIFCONTINUED (status)) {
         xprintf ("CONTINUED");
      }else {
         xprintf ("UNKNOWN EXIT STATUS");
      }
   }
   if (argc > 2) {
      xprintf (" --");
      for (argi = 2; argi < argc; ++argi) xprintf (" %s", argv[argi]);
   }
   xprintf ("\n");
   return EXIT_SUCCESS;
}

