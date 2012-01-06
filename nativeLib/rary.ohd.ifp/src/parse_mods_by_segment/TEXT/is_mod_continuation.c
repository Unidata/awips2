/*
 *  int function is_mod_continuation.c
 *
 * This functions accepts a line from a run-time mod file
 *  and determines if a line expects a following continuation
 *  line.
 *
 * Originally written by G. Smith - HRL - 950305
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

#define MAX_SEGS_LINE 140

int getword();

int is_mod_continuation(char line[MAX_SEGS_LINE], int line_length)
{
 int   start_loc, loc_next;
 char  word[MAX_SEGS_LINE];
 
 start_loc = loc_next = 0;

 while (loc_next < line_length && loc_next != EOF)
   {
    start_loc = loc_next;
    loc_next = getword(line, start_loc, line_length, word);
    if(strlen(word) == 1 && word[0] == '&')
      {                  
       return (TRUE);	/* continuation field */
      }
   } /* end while ... */
 return (FALSE);	/* no continuation */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/parse_mods_by_segment/RCS/is_mod_continuation.c,v $";
 static char rcs_id2[] = "$Id: is_mod_continuation.c,v 1.1 1995/09/08 15:02:16 page Exp $";}
/*  ===================================================  */

}
