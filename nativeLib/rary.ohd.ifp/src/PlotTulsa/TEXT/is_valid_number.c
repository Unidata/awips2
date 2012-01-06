/* File: is_valid_number.c
 *
 * is_valid_number returns TRUE if the character string entered
 * contains a valid number.  The first character may be a minus
 * or plus sign.  One decimal point may appear anywhere in the
 * string.
 *
 * FALSE is returned if the character string is not valid.
 *
 *  Originally coded by George Smith, HRL, April 1991
 */
#include <stdio.h>
#include <ctype.h>

#ifndef TRUE
#define TRUE    1
#define FALSE   0
#endif

int is_valid_number(number)

  char          *number;        /* numeric character pointer */
{
  int           decimal_point_found = FALSE;
  int           i=0;            /* array index */

  while (number[i] != '\0')
       {
	if(isdigit(number[i]))   /* if digit is found, just continue */
	   ;
	else                                      /* non-digit found */
	  {
	   if(i == 0)                          /* at first character */
	     {
	      if(number[i] == '+' || number[i] == '-')
		 ;                  /* first character is + or -, OK */

	      else if(number[i] == '.')
		 decimal_point_found = TRUE;   /* first character is */
					       /* decimal point, OK  */
					       /* set flag           */
	      else
		 return (FALSE);           /* character is not valid */
	     }
	   else                             /* after first character */
	     {
	      if(number[i] == '.' && decimal_point_found == FALSE)
		 decimal_point_found = TRUE;
	      else
		 return (FALSE);         /* second decimal point or */
					 /* invalid character found */
	     }
	  }
	i++;           /* character passes tests, increment counter */
       }
  return (TRUE);   /* at end of string, only valid characters found */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/is_valid_number.c,v $";
 static char rcs_id2[] = "$Id: is_valid_number.c,v 1.1 1995/09/08 14:57:35 page Exp $";}
/*  ===================================================  */

}
