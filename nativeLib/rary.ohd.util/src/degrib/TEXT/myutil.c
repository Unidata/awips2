/*****************************************************************************
 * myutil.c
 *
 * DESCRIPTION
 *    This file contains some simple utility functions.
 *
 * HISTORY
 *   12/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "degrib_inc/myutil.h"
#ifdef MEMWATCH
  #include "degrib_inc/memwatch.h"
#endif

/*****************************************************************************
 * GetIndexFromStr() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Looks through a list of strings (with a NULL value at the end) for a
 * given string.  Returns the index where it found it.
 *
 * ARGUMENTS
 *   arg = The string to look for. (Input)
 *   Opt = The list to look for arg in. (Input)
 * Index = The location of arg in Opt (or -1 if it couldn't find it) (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: int
 *  # = Where it found it.
 * -1 = Couldn't find it.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (TK,AC,TB,&MS): Code Review.
 *
 * NOTES
 *****************************************************************************
 */
int GetIndexFromStr (char *arg, char **Opt, int *Index)
{
   char **ptr = Opt;
   int cnt = 0;
   while (*ptr != NULL) {
      if (strcmp (arg, *ptr) == 0) {
         *Index = cnt;
         return cnt;
      }
      ptr++;
      cnt++;
   }
   return -1;
}

/*****************************************************************************
 * reallocFGets() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Read in data from file until a \n is read.  Reallocate memory as needed.
 * Similar to fgets, except we don't know ahead of time that the line is a
 * specific length.
 *   Assumes that Ptr is either NULL, or points to lenBuff memory.
 *   Responsibility of caller to free the memory.
 *
 * ARGUMENTS
 *     Ptr = An array of data that is of size LenBuff. (Input/Output)
 * LenBuff = The Allocated length of Ptr. (Input/Output)
 *      fp = Input file stream (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: int
 *   strlen (buffer)...
 *     0 if we Read only EOF.
 *     1 if we have "\nEOF" or "<char>EOF"
 *
 * HISTORY
 *  12/2002 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 *  1) Based on getline (see K&R C book (2nd edition) p 29)
 *     and on the behavior of Tcl's gets routine.
 *  2) Chose MIN_STEPSIZE = 80 because pages are usually 80 columns.
 *  3) Could switch lenBuff = i + 1 / lenBuff = i to always true.
 *     Rather not... Less allocs... This way code behaves almost the
 *     same as fgets except it can expand as needed.
 *****************************************************************************
 */
#define MIN_STEPSIZE 80
int reallocFGets (char **Ptr, int *LenBuff, FILE * fp)
{
   char *buffer = *Ptr; /* Local copy of Ptr. */
   int lenBuff = *LenBuff; /* Local copy of LenBuff. */
   int c;               /* Current char read from stream. */
   int i;               /* Where to store c. */

   for (i = 0; ((c = getc (fp)) != EOF) && (c != '\n'); ++i) {
      if (i >= lenBuff) {
         lenBuff += MIN_STEPSIZE;
         buffer = (char *) realloc ((void *) buffer,
                                    lenBuff * sizeof (char));
      }
      buffer[i] = (char) c;
   }
   if (c == '\n') {
      if (lenBuff <= i + 1) {
         lenBuff = i + 2; /* Make room for \n\0. */
         buffer = (char *) realloc ((void *) buffer,
                                    lenBuff * sizeof (char));
      }
      buffer[i] = (char) c;
      ++i;
      buffer[i] = '\0';
      *Ptr = buffer;
      *LenBuff = lenBuff;
      return i;
   } else {
      if (lenBuff <= i) {
         lenBuff = i + 1; /* Make room for \0. */
         buffer = (char *) realloc ((void *) buffer,
                                    lenBuff * sizeof (char));
      }
      buffer[i] = '\0';
      *Ptr = buffer;
      *LenBuff = lenBuff;
      return i;
   }
}

#undef MIN_STEPSIZE

/*****************************************************************************
 * myRound() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Round a number to a given number of decimal places.
 *
 * ARGUMENTS
 *  data = number to round (Input)
 * place = How many decimals to round to (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: double (rounded value)
 *
 * HISTORY
 *  5/2003 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 *  1) It is probably inadvisable to make a lot of calls to this routine,
 *     considering the fact that a context swap is made, so this is provided
 *     primarily as an example, but it can be used for some rounding.
 *****************************************************************************
 */
double POWERS_ONE[] = {
   1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
   1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17
};

double myRound (double data, signed char place)
{
   if (place > 17)
      place = 17;
   if (place < 0)
      place = 0;
   return (floor (data * POWERS_ONE[place] + .5)) / POWERS_ONE[place];

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/degrib/RCS/myutil.c,v $";
 static char rcs_id2[] = "$Id: myutil.c,v 1.1 2004/09/16 17:14:09 dsa Exp $";}
/*  ===================================================  */

}
