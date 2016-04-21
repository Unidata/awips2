/*
 * error.c
 *
 *  Created on: Feb 6, 2009
 *      Author: jelkins
 */

#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>

/* Obtain a backtrace and print it to stdout.
 * from: http://www.gnu.org/software/libtool/manual/libc/Backtraces.html
 * */
void
print_trace (int startIndex)
{
  void *array[10];
  size_t size;
  char **strings;
  size_t i;

  size = backtrace (array, 10);
  strings = backtrace_symbols (array, size);

  for (i = startIndex; i < size; i++)
     fprintf (stderr, "\t at %s\n", strings[i]);

  free (strings);
}


void nolibError (char * libName) {
	fprintf(stderr, "Error: %s functions are not supported\n",libName);
	print_trace(2);
}
