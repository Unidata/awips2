/*------------------------------------------------------------------------------
** GetMeanFromInst - find the mean value of a instatenously equal time step
**                   series.
**------------------------------------------------------------------------------
** History:
**
** 08-01-96     Wentao Liu, Rti         Crecte function.
**------------------------------------------------------------------------------
** Variables    I/O     Description
**
** i            L       Loop counter for points.
** mean         O       Mean of the values.
** n            I       Number of data values.
** routine      L       Name of this routine.
** x            I       Data values.
**------------------------------------------------------------------------------
*/
#include "ResJ.h"

int GetMeanFromInst ( int n, float *x, float *mean)
{       int     i;
        char    routine[] = "GetMeanFromInst";
        float   sum=0.0;

        if ( n <= 0 ) {
                PrintWarning ( 50, routine, 
		"Number of points = %d <= 0", n );
                return STATUS_FAILURE;
        }

	if (n == 1){
		*mean=x[0];
		return STATUS_SUCCESS;
	}

        sum = 0.5 *( x[0] + x[n-1] );

        for ( i = 1; i < n-1; i++ ){
		sum += x[i];
	}

        *mean = sum / (float) (n-1);

        return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetMeanFromInst.c,v $";
 static char rcs_id2[] = "$Id: GetMeanFromInst.c,v 1.1 1999/02/18 15:16:47 dws Exp $";}
/*  ===================================================  */

}
