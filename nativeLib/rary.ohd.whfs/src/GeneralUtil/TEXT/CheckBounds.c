/*
	File:		CheckBounds.c
	Date:		July 1997
	Author:		Paul Taylor
	
	Purpose:	Provide support for performing bounds checking
			on a string (which is first converted to a double
			and compared against the lo & hi boundary conditions).
*/


#include <stdio.h>
#include <stdlib.h>

#include "GeneralUtil.h"


/****************************************************************************

   Checks the bounds on a string, by converting it to a double (using atof)
   and comparing it against the lo/hi conditions/values.  If the string
   passed in is NULL, it is treated as a (double) 0.0 value.

 ****************************************************************************/

int	CheckBounds(char* buf,
		    char* lo_cond, double* lo,
		    char* hi_cond, double* hi)
{
   double	value;
   
   char		lo_char = '\0';
   char		hi_char = '\0';
   
   int		pass_flag = 1; /* assume in bounds until proven otherwise */
   
   
   /* convert the buf to a value */
   
   value = atof(buf);
   

   
   /* assign limit characters */
   
   if (strcmp(lo_cond, GT) == 0)	lo_char = '(';
   if (strcmp(lo_cond, GE) == 0)	lo_char = '[';
   
   if (strcmp(hi_cond, LT) == 0)	hi_char = ')';
   if (strcmp(hi_cond, LE) == 0)	hi_char = ']';
   
   
   
   /* check to see if value falls outside of boundary conditions */
   
   if ((pass_flag) && (lo_char == '(') && (value <= *lo))   pass_flag = 0;
   if ((pass_flag) && (lo_char == '[') && (value <  *lo))   pass_flag = 0;
   
   if ((pass_flag) && (hi_char == ')') && (value >= *hi))   pass_flag = 0;
   if ((pass_flag) && (hi_char == ']') && (value >  *hi))   pass_flag = 0;
   
   

   return(pass_flag);
}



