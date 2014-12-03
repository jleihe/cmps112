// $Id: gnu-libc-version.c,v 1.1 2013-06-14 15:08:05-07 - - $

#include <stdio.h>
#include <gnu/libc-version.h>

int main (void) {
   printf ("%s\n", gnu_get_libc_version());
   return 0;
}

