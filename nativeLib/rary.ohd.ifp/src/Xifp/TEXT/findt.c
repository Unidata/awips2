/* File: findt.c

	The findt function searches the T array and returns
	the location of the specified operation type.
	If no operation of the requested type is found,
	  return loct = 0

	function originally written by George Smith, HRL, March 1990
*/

#include <stdio.h>

/*      findt calls searcht which recursively looks through
	the t array for a requested operation type, return loct
*/
void    searcht(loct, numop, t, mt)

int     *loct;   /* pointer to the location of the beginning of the
		    parameter array. */
int     t[];     /* t array */
int     *numop;  /* number of the operation the operation looked for */
int     *mt;     /* maximum length of the t array */
{
if(t[*loct-1] == -1)
	{
	 /*  at end of t array - have not found operation       */
	 *loct = 0;
	 return;
	}
if(t[*loct-1] != *numop)
	{
	 /*  current operation is not the one wanted - call searcht     */
	 *loct = t[*loct-1 + 1];
	 searcht(loct, numop, t, mt);
	 return;
	}
/*  have found the operation - return           */
}

void findt(numop, loct, t, mt)

int     *numop, *loct, t[], *mt;
{
*loct = 1;
searcht(loct, numop, t, mt);
}
/*
 * findt_next function added to allow finding subsequent
 *  operations of the same type in an operations table.
 * Specifically, to find all PLOT-TUL operations in the
 *  ss_input.f code used to fill e19 data.
 * written by gfs, 2/19/92
 */
void findt_next(numop, loct, t, mt)

int     *numop, *loct, t[], *mt;
{
if(t[*loct-1] == -1)
	{
	 /*  at end of t array - have not found operation       */
	 *loct = 0;
	 return;
	}
/* move loct pointer to next operation in the T array */
*loct = t[*loct-1 + 1];
searcht(loct, numop, t, mt);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/findt.c,v $";
 static char rcs_id2[] = "$Id: findt.c,v 1.1 1995/09/08 15:00:17 page Exp $";}
/*  ===================================================  */

}
