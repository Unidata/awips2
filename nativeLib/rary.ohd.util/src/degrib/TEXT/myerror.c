/*****************************************************************************
 * myerror.c
 *
 * DESCRIPTION
 *    This file contains the code to handle error messages.  Instead of simply
 * printing the error to stdio, it allocates some memory and stores the
 * message in it.  This is so that one can pass the error message back to
 * Tcl/Tk or another GUI program when there is no stdio.
 *    In addition a version of sprintf is provided which allocates memory for
 * the calling routine, so that one doesn't have to guess the maximum bounds
 * of the message.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL / RSIS): Created.
 *  12/2002 Rici Yu, Fangyu Chi, Mark Armstrong, & Tim Boyer
 *          (RY,FC,MA,&TB): Code Review 2.
 *
 * NOTES
 *   See Kernighan & Ritchie C book (2nd edition) page 156.
 *****************************************************************************
 */
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "degrib_inc/myerror.h"

/*****************************************************************************
 * AllocSprintf() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Based on minprintf (see K&R C book (2nd edition) page 156.  This code
 * tries to provide some of the functionality of sprintf, while at the same
 * time it handles the memory allocation.
 *   In addition, it provides a %S option, which allows one to pass in an
 * array of strings, and get back a comma delimited string.
 *
 * ARGUMENTS
 *     Ptr = An array of data that is of size LenBuff. (Input/Output)
 * LenBuff = The allocated length of Ptr. (Input/Output)
 *     fmt = Format similar to the one used by sprintf to define how to
 *           print the message (Input)
 *      ap = argument list initialized by a call to va_start.  Contains the
 *           data needed by fmt. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *  12/2002 AAT: Fixed the mallocSprintf ("") error.
 *   2/2003 AAT: increased bufpart[80] to bufpart[330] because the largest
 *          64 bit double is: +1.7E+308, and I want 20 "slots" for stuff
 *          after the decimal place. There is the possibility of "Long
 *          doubles" (80 bits) which would have a max of: +3.4E+4932, but
 *          that is excessive for now.
 *   2/2004 AAT: if lenBuff != 0, switch from ipos-- to strlen (buffer);
 *   3/2004 AAT: Added %c option.
 *
 * NOTES
 * Supported formats:
 *  %0.4f => float, double
 *  %03d %ld %10ld => int, long int.
 *  %s => Null terminated char string. (no range specification)
 *  %S => take a char ** and turn it into a comma delimited string.
 *
 * Assumes that no individual float or int will be more than 80 characters
 * Assumes that no % option is more than 20 char.
 *****************************************************************************
 */
static void AllocSprintf (char **Ptr, int *LenBuff, char *fmt, va_list ap)
{
   char *buffer = *Ptr; /* Local copy of Ptr. */
   int lenBuff = *LenBuff; /* Local copy of LenBuff. */
   char *p;             /* Points to % char in % option. */
   char *p1;            /* Points to end of % option. */
   char bufpart[330];   /* Used for formating the int / float options. */
   char format[20];     /* Used to store the % option. */
   char *sval;          /* For pulling strings off va_list. */
   char **Sval;         /* For pulling lists of strings off va_list. */
   int slen;            /* Length of used part of temp. */
   char f_inLoop;       /* Flag to state whether we got into %S , loop. */
   char flag;           /* If they have a l,L,h in string. */
   int ipos = *LenBuff; /* The current index to start storing data. */
   int c_type;          /* Used when handling %c option. */

   if ((fmt == NULL) || (strlen (fmt) == 0)) {
      return;
   }
   p = fmt;
   /* If lenBuff = 0, then make room for the '\0' character. */
   if (lenBuff == 0) {
      lenBuff++;
      buffer = (char *) realloc ((void *) buffer, lenBuff * sizeof (char));
   } else {
      ipos = strlen (buffer);
/*      ipos--;*/
   }
   while (p < fmt + strlen (fmt)) {
      p1 = p;
      p = strchr (p1, '%');
      /* Handle simple case when no more % in format string. */
      if (p == NULL) {
         /* No more format strings; copy rest of format and return */
         lenBuff += strlen (p1);
         buffer = (char *) realloc ((void *) buffer,
                                    lenBuff * sizeof (char));
         strcpy (buffer + ipos, p1);
         goto done;
      }
      /* Handle data up to the current % in format string. */
      lenBuff += p - p1;
      buffer = (char *) realloc ((void *) buffer, lenBuff * sizeof (char));
      strncpy (buffer + ipos, p1, p - p1);
      ipos = lenBuff - 1;
      /* Start dealing with % of format. */
      p1 = p + strspn (p + 1, "0123456789.");
      p1++;
      /* p1 points to first letter after %. */
      switch (*p1) {
         case 'h':
         case 'l':
         case 'L':
            flag = *p1;
            p1++;
            break;
         case '\0':
            /* Handle improper use of '%' for example: '%##' */
            lenBuff += p1 - p - 1;
            buffer = (char *) realloc ((void *) buffer,
                                       lenBuff * sizeof (char));
            strncpy (buffer + ipos, p + 1, p1 - p - 1);
            goto done;
         default:
            flag = ' ';
      }
      if ((p1 - p + 1) > (int) (sizeof (format)) - 1) {
         /* Protect against overflow of format string. */
         lenBuff += p1 - p + 1;
         buffer = (char *) realloc ((void *) buffer,
                                    lenBuff * sizeof (char));
         strncpy (buffer + ipos, p, p1 - p + 1);
         ipos = lenBuff - 1;
      } else {
         strncpy (format, p, p1 - p + 1);
         format[p1 - p + 1] = '\0';
         switch (*p1) {
            case 'd':
               switch (flag) {
                  case 'l':
                  case 'L':
                     sprintf (bufpart, format, va_arg (ap, long int));
                     break;
                     /* 
                      * gcc warning for 'h': "..." promotes short int to
                      * int.  Could get rid of 'h' option but decided to
                      * leave it in since we might have a different
                      * compiler.
                      */
/*
              case 'h':
                sprintf (bufpart, format, va_arg(ap, short int));
                break;
*/
                  default:
                     sprintf (bufpart, format, va_arg (ap, int));
               }
               slen = strlen (bufpart);
               lenBuff += slen;
               buffer = (char *) realloc ((void *) buffer,
                                          lenBuff * sizeof (char));
               strncpy (buffer + ipos, bufpart, slen);
               ipos = lenBuff - 1;
               break;
            case 'f':
               sprintf (bufpart, format, va_arg (ap, double));
               slen = strlen (bufpart);
               lenBuff += slen;
               buffer = (char *) realloc ((void *) buffer,
                                          lenBuff * sizeof (char));
               strncpy (buffer + ipos, bufpart, slen);
               ipos = lenBuff - 1;
               break;
            case 'c':
               c_type = va_arg (ap, int);
               lenBuff += 1;
               buffer = (char *) realloc ((void *) buffer,
                                          lenBuff * sizeof (char));
               buffer[ipos] = c_type;
               buffer[ipos + 1] = '\0';
               ipos = lenBuff - 1;
               break;
            case 's':
               if ((p1 - p) == 1) {
                  sval = va_arg (ap, char *);
                  slen = strlen (sval);
                  lenBuff += slen;
                  buffer = (char *) realloc ((void *) buffer,
                                             lenBuff * sizeof (char));
                  strncpy (buffer + ipos, sval, slen);
                  ipos = lenBuff - 1;
                  break;
               }
               /* Intentionally fall through. */
            case 'S':
               if ((p1 - p) == 1) {
                  f_inLoop = 0;
                  for (Sval = va_arg (ap, char **); *Sval; Sval++) {
                     slen = strlen (*Sval);
                     lenBuff += slen + 1;
                     buffer = (char *) realloc ((void *) buffer,
                                                lenBuff * sizeof (char));
                     strcpy (buffer + ipos, *Sval);
                     strcat (buffer + ipos + slen, ",");
                     ipos = lenBuff - 1;
                     f_inLoop = 1;
                  }
                  if (f_inLoop) {
                     lenBuff--;
                     buffer[lenBuff] = '\0';
                     ipos = lenBuff - 1;
                  }
                  break;
               }
               /* Intentionally fall through. */
            default:
               lenBuff += p1 - p;
               buffer = (char *) realloc ((void *) buffer,
                                          lenBuff * sizeof (char));
               strncpy (buffer + ipos, p + 1, p1 - p);
               ipos = lenBuff - 1;
         }
      }
      p = p1 + 1;
   }
 done:
   buffer[lenBuff - 1] = '\0';
   *Ptr = buffer;
   *LenBuff = lenBuff;
}

/*****************************************************************************
 * mallocSprintf() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   This is a front end for AllocSprintf, when you want to malloc memory.
 * In other words when the pointer is not pointing to anything in particular.
 * It allocates the memory, prints the message, and then sets Ptr to point to
 * it.
 *
 * ARGUMENTS
 * Ptr = Place to point to new memory which contains the message (Output)
 * fmt = Format similar to the one used by sprintf to define how to print the
 *       message (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 * Supported formats:  See AllocSprintf
 *****************************************************************************
 */
void mallocSprintf (char **Ptr, char *fmt, ...)
{
   va_list ap;          /* Contains the data needed by fmt. */
   int buff_len = 0;    /* Allocated length of buffer. */

   *Ptr = NULL;
   if (fmt != NULL) {
      va_start (ap, fmt); /* make ap point to 1st unnamed arg. */
      AllocSprintf (Ptr, &buff_len, fmt, ap);
      va_end (ap);      /* clean up when done. */
   }
}

/*****************************************************************************
 * reallocSprintf() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   This is a front end for AllocSprintf, when you want to realloc memory.
 * In other words, the pointer is pointing to NULL, or to some memory that
 * you want to tack a message onto the end of.  It allocates extra memory,
 * and prints the message.
 *
 *   KEY WORDS: "Tack a message onto the end of" 
 *
 * ARGUMENTS
 * Ptr = Pointer to memory to add the message to. (Input/Output)
 * fmt = Format similar to the one used by sprintf to define how to print the
 *       message (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 * Supported formats:  See AllocSprintf
 *****************************************************************************
 */
void reallocSprintf (char **Ptr, char *fmt, ...)
{
   va_list ap;          /* Contains the data needed by fmt. */
   int buff_len;        /* Allocated length of buffer. */

   if (fmt != NULL) {
      va_start (ap, fmt); /* make ap point to 1st unnamed arg. */
      if (*Ptr == NULL) {
         buff_len = 0;
      } else {
         buff_len = strlen (*Ptr) + 1;
      }
      AllocSprintf (Ptr, &buff_len, fmt, ap);
      va_end (ap);      /* clean up when done. */
   }
}

/*****************************************************************************
 * errSprintf() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   This uses AllocSprintf to generate a message, which it stores in a static
 * variable.  If it is called with a (NULL), it returns the built up message,
 * and resets its pointer to NULL.  The idea being that errors can be stacked
 * up, and you pop them off when you need to report them.  The reporting could
 * be done by printing them to stdio, or by passing them back to Tcl/Tk.
 *   Note: It is the caller's responsibility to free the memory, and it is
 * the caller's responsibility to make sure the last call to this is with
 * (NULL), or else the memory won't get freed.
 *
 * ARGUMENTS
 * fmt = Format similar to the one used by sprintf to define how to print the
 *       message (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: char *
 *   if (fmt == NULL) returns built up string
 *   else             returns NULL.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 * Supported formats:  See AllocSprintf
 *****************************************************************************
 */
/* Following 2 variables used in both errSprintf and preErrSprintf */
static char *errBuffer = NULL; /* Stores the current built up message. */
static int errBuff_len = 0; /* Allocated length of errBuffer. */

char *errSprintf (char *fmt, ...)
{
   va_list ap;          /* Contains the data needed by fmt. */
   char *ans;           /* Pointer to the final message while we reset
                         * buffer. */

   if (fmt == NULL) {
      ans = errBuffer;
      errBuffer = NULL;
      errBuff_len = 0;
      return ans;
   }
   va_start (ap, fmt);  /* make ap point to 1st unnamed arg. */
   AllocSprintf (&errBuffer, &errBuff_len, fmt, ap);
   va_end (ap);         /* clean up when done. */
   return NULL;
}

/*****************************************************************************
 * preErrSprintf() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   This uses AllocSprintf to generate a message, which it prepends to the
 * static variable used by errSprinf.  If it is called with a (NULL), it
 * does nothing... Use errSprintf (NULL) to get the message, and reset the
 * pointer to NULL.
 *   The idea here is that we want to prepend calling info when there was an
 * error.
 *   Note: It is the caller's responsibility to free the memory, by
 * eventually making one last call to errSprintf (NULL) and freeing the
 * returned memory.
 *
 * ARGUMENTS
 * fmt = Format similar to the one used by sprintf to define how to print the
 *       message (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *  12/2002 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 * Supported formats:  See AllocSprintf
 *****************************************************************************
 */
void preErrSprintf (char *fmt, ...)
{
   char *preBuffer = NULL; /* Stores the prepended message. */
   int preBuff_len = 0; /* Allocated length of preBuffer. */
   va_list ap;          /* Contains the data needed by fmt. */

   if (fmt == NULL) {
      return;
   }
   va_start (ap, fmt);  /* make ap point to 1st unnamed arg. */
   AllocSprintf (&preBuffer, &preBuff_len, fmt, ap);
   va_end (ap);         /* clean up when done. */

   if (errBuff_len != 0) {
      /* Increase preBuffer to have enough room for errBuffer */
      preBuff_len += errBuff_len;
      preBuffer = (char *) realloc ((void *) preBuffer,
                                    preBuff_len * sizeof (char));
      /* concat errBuffer to end of preBuffer, and free errBuffer */
      strcat (preBuffer, errBuffer);
      free (errBuffer);
   }
   /* Finally point errBuffer to preBuffer, and update errBuff_len. */
   errBuffer = preBuffer;
   errBuff_len = preBuff_len;
   return;
}

#ifdef TEST_MYERROR
/*****************************************************************************
 * The following 2 procedures are included only to test myerror.c, and only
 * if TEST_MYERROR is defined.
 *****************************************************************************
 */

/*****************************************************************************
 * checkAns() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To verify that a test gives the expected result.
 *
 * ARGUMENTS
 *  ptr = The results of the test. (Input)
 *  Ans = An array of correct answers. (Input)
 * test = Which test we are checking. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 *****************************************************************************
 */
static void checkAns (char *ptr, char **Ans, int test)
{
   if (ptr == NULL) {
      printf ("-----Check test (%d)--(ptr == NULL)-----\n", test);
      return;
   }
   if (strcmp (ptr, Ans[test]) != 0) {
      printf ("-----Failed test %d-------\n", test);
      printf ("%s %d =?= %s %d\n", ptr, strlen (ptr),
              Ans[test], strlen (Ans[test]));
   } else {
      printf ("passed test %d\n", test);
   }
}

/*****************************************************************************
 * main() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To test reallocSprint, mallocSprint, and errSprintf, to make sure that
 * they pass certain basic tests.  I will be adding more tests, as more bugs
 * are found, and features added.
 *
 * ARGUMENTS
 * argc = The number of arguments on the command line. (Input)
 * argv = The arguments on the command line. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: int
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 *****************************************************************************
 */
int main (int argc, char **argv)
{
   char *ptr;
   static char *Cmd[] = { "configure", "inquire", "convert", NULL };
   long int li_temp = 100000L;
   short int sect = 5;
   char varName[] = "Helium is a gas";
   long int lival = 22;
   char unit[] = "km", sval[] = "ans";
   double dval = 2.71828;

   char *buffer = NULL;
   short int ssect = 0;
   char vvarName[] = "DataType";
   long int llival = 0;
   char ssval[] = "Meteorological products";

   static char *Ans[] = { "S0 | DataType | 0 (Meteorological products)\n",
      "<testing>", "<05><3.1415><D><20>",
      "<configure,inquire,convert> ?options?",
      "100000", "25.123", "02s", "01234567890123456789012345",
      "25.123,05, hello world",
      "This is a test 5... Here I am\n",
      "Parse error Section 0\nErrorERROR: Problems opening c:--goober for "
         "write.Projection code requires Earth with Rad = 6367.47 not "
         "6400.010000",
      "ERROR IS1 not labeled correctly. 5000000\n"
         "Should be 1196575042 2 25\nERROR IS0 has unexpected values: "
         "100000 100000 100000\n",
      "S5 | Helium is a gas | 22 (ans)\nS5 | Helium is a gas | 22\n"
         "S5 | Helium is a gas | 22 (ans (km))\nS5 | Helium is a gas | ans\n"
         "S5 | Helium is a gas | 2.718280\nS5 | Helium is a gas | "
         "2.718280 (km)\n",
      "ERROR IS1 not labeled correctly. 5000000\n"
         "Should be 1196575042 2 25\nERROR IS0 has unexpected values: "
         "100000 100000 100000\n"
   };

/* Test -2. (See if it can handle blank). */
   mallocSprintf (&ptr, "");
   free (ptr);
   ptr = NULL;

   mallocSprintf (&ptr, " ");
   free (ptr);
   ptr = NULL;


/* Test -1. (see if checkAns is ok) */
   ptr = errSprintf (NULL);
   checkAns (ptr, Ans, -1);

/* Test 0 */
   reallocSprintf (&buffer, "S%d | %s | %ld (%s)\n", ssect, vvarName,
                   llival, ssval);
   checkAns (buffer, Ans, 0);
   free (buffer);

/* Test 1. */
   ptr = NULL;
   reallocSprintf (&ptr, "<testing>");
   checkAns (ptr, Ans, 1);
   free (ptr);

/* Test 2. */
   ptr = NULL;
   reallocSprintf (&ptr, "<%02d><%.4f><%D><%ld>", 5, 3.1415, 20, 24);
   checkAns (ptr, Ans, 2);
   free (ptr);

/* Test 3. */
   ptr = NULL;
   reallocSprintf (&ptr, "<%S> ?options?", Cmd);
   checkAns (ptr, Ans, 3);
   free (ptr);

/* Test 4. */
   ptr = NULL;
   reallocSprintf (&ptr, "%ld", li_temp);
   checkAns (ptr, Ans, 4);
   free (ptr);

/* Test 5. */
   ptr = NULL;
   reallocSprintf (&ptr, "%.3f", 25.1234);
   checkAns (ptr, Ans, 5);
   free (ptr);

/* Test 6. */
   ptr = NULL;
   reallocSprintf (&ptr, "%02s", 25.1234);
   checkAns (ptr, Ans, 6);
   free (ptr);

/* Test 7. */
   ptr = NULL;
   reallocSprintf (&ptr, "%01234567890123456789012345");
   checkAns (ptr, Ans, 7);
   free (ptr);

/* Test 8. */
   mallocSprintf (&ptr, "%.3f", 25.1234);
   reallocSprintf (&ptr, ",%02d", 5);
   reallocSprintf (&ptr, ", %s", "hello world");
   checkAns (ptr, Ans, 8);
   free (ptr);
   ptr = NULL;

/* Test 9. */
   errSprintf ("This is a test %d... ", 5);
   errSprintf ("Here I am\n");
   ptr = errSprintf (NULL);
   checkAns (ptr, Ans, 9);
   free (ptr);

/* Test 10. */
   errSprintf ("Parse error Section 0\n%s", "Error");
   errSprintf ("ERROR: Problems opening %s for write.", "c:--goober");
   errSprintf ("Projection code requires Earth with Rad = 6367.47 not %f",
               6400.01);
   ptr = errSprintf (NULL);
   checkAns (ptr, Ans, 10);
   free (ptr);

/* Test 11. */
   errSprintf ("ERROR IS1 not labeled correctly. %ld\n", 5000000L);
   errSprintf ("Should be %ld %d %ld\n", 1196575042L, 2, 25);
   errSprintf ("ERROR IS0 has unexpected values: %ld %ld %ld\n", li_temp,
               li_temp, li_temp);
   ptr = errSprintf (NULL);
   checkAns (ptr, Ans, 11);
   free (ptr);

/* Test 12. */
   ptr = NULL;
   reallocSprintf (&ptr, "S%d | %s | %ld (%s)\n", sect, varName, lival,
                   sval);
   reallocSprintf (&ptr, "S%d | %s | %ld\n", sect, varName, lival);
   reallocSprintf (&ptr, "S%d | %s | %ld (%s (%s))\n", sect, varName, lival,
                   sval, unit);
   reallocSprintf (&ptr, "S%d | %s | %s\n", sect, varName, sval);
   reallocSprintf (&ptr, "S%d | %s | %f\n", sect, varName, dval);
   reallocSprintf (&ptr, "S%d | %s | %f (%s)\n", sect, varName, dval, unit);
   checkAns (ptr, Ans, 12);
   free (ptr);

/* Test 13. */
   preErrSprintf ("Should be %ld %d %ld\n", 1196575042L, 2, 25);
   errSprintf ("ERROR IS0 has unexpected values: %ld %ld %ld\n", li_temp,
               li_temp, li_temp);
   preErrSprintf ("ERROR IS1 not labeled correctly. %ld\n", 5000000L);
   ptr = errSprintf (NULL);
   checkAns (ptr, Ans, 13);
   free (ptr);

   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/degrib/RCS/myerror.c,v $";
 static char rcs_id2[] = "$Id: myerror.c,v 1.1 2004/09/16 17:14:09 dsa Exp $";}
/*  ===================================================  */

}
#endif
