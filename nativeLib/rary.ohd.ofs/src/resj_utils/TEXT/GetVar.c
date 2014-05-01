/*------------------------------------------------------------------------------
** GetVar - calculate the sample variance for a list of numbers
**------------------------------------------------------------------------------
** History:
**
** 05-25-95	Steven A. Malers, RTi		Change so that the routine
**						returns an error code and the
**						mean is returned in the
**						parameter list.
** 27 Jan 1997	Matthew J. Rutherford, RTi	Copied into ESPUtil.
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** dif		L	Difference between a data value and the mean for the
**			set.
** mean		L	Mean of the sample set (zero of there is trouble).
** message	L	String for messages.
** n		I	Number of data values.
** routine	L	Name of this routine.
** var		O	Variance of the values.
** x		I	Data values.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

int GetVar ( int n, float *x, float *var )
{	int	i;
	float	dif, mean;
	char	message[256], routine[] = "GetVar";

	*var = 0.0;
	if ( n <= 1 ) {
		sprintf ( message, "Number of data values = %d <= 1", n );
		PrintWarning ( 50, routine, message );
		return STATUS_FAILURE;
	}
	if ( GetMean(n, x, &mean) ) {
		PrintWarning ( 50, routine, "Error calculating mean" );
		return STATUS_FAILURE;
	}
	for ( i = 0; i < n; i++ ) {
		dif = (x[i] - mean);
		*var += dif*dif;
	}
	*var /= (n - 1);
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetVar.c,v $";
 static char rcs_id2[] = "$Id: GetVar.c,v 1.1 1999/02/18 15:16:54 dws Exp $";}
/*  ===================================================  */

}
