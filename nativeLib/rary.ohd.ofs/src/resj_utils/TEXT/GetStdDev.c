/*------------------------------------------------------------------------------
** GetStdDev - calculate the standard deviation for a list of numbers
**------------------------------------------------------------------------------
** Copyright:   See the COPYRIGHT file.
**------------------------------------------------------------------------------
** History:
**
** 05-25-95	Steven A. Malers, RTi	Changed return status to an integer -
**					return stddev via the parameter list.
** 06 Sep 1996  Steven A. Malers, RTi   Split out of the HMStat.c file.
** 27 Jan 1997	Matthew J. Rutherford	Copied from libHMData.
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** n		I	Number of data values.
** routine	L	Name of this routine.
** s		O	Standard deviation.
** var		L	Variance of the values.
** x		I	Data values.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

int GetStdDev ( int n, float *x, float *s )
{	char	routine[] = "GetStdDev";
	float	var;

	*s = 0.0;
	if ( GetVar(n, x, &var) ) {
		PrintWarning ( 50, routine, "Trouble calculating variance" );
		return STATUS_FAILURE;
	}
	if ( var <= 0.0 ) {
		PrintWarning ( 50, routine,
		"Variance <= 0.  Cannot compute StdDev" );
		return STATUS_FAILURE;
	}
	*s	= sqrt ( var );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetStdDev.c,v $";
 static char rcs_id2[] = "$Id: GetStdDev.c,v 1.1 1999/02/18 15:16:49 dws Exp $";}
/*  ===================================================  */

}
