#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void shleap(lyear, leap)

long int *lyear, *leap;
{
	long int ia, ib, ic;


	/*
      
	    RETURNS LEAP = 1 IF LYEAR IS A LEAP YEAR. LEAP = 0 OTHERWISE.
	    LYEAR IS THE 2 DIGIT YEAR.
    */

	*leap = *lyear + 1900L;	/* WHAT HAPPENS IN YEAR 2000 ? */
	ia = (*leap - (*leap/4L)*4L + 3L)/4L;
	ib = (*leap - (*leap/100L)*100L + 99L)/100L;
	ic = (*leap - (*leap/400L)*400L + 399L)/400L;
	*leap = 1L - ia + ib - ic;	/* LEAP IS 1 IF LEAP YEAR */

	return;

} 

