/*
** LinearRegression - calculate linear regression values for data
**------------------------------------------------------------------------------
** Copyright:   See the COPYRIGHT file.
**------------------------------------------------------------------------------
** History:
**
** 05-25-95	Steven A. Malers, RTi	Document routine.  Add checks for
**					statistics infeasible solutions (e.g.,
**					div by zero).
** 06 Sep 1996  Steven A. Malers, RTi   Split out of the HMStat.c file.
**    Jan 1997	Daniel J. Epstein	modified for inclusion into ESPADP
**					required removal of HM ref.
** 22 Jan 97	Matthew J. Rutherford, Rti
**					changed Sum to GetSum.
** 14 Oct 1997	DJE, RTi		Added the r2 correlation coefficient
**					as a return parameter.
** NOTES:
**		this fits equation y = a + bx 
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** a		O	Y-intercept of line of best fit.
** b		O	Slope of line of best fit.
** i		L	Loop counter for data.
** n		I	Number of data points.
** r2		O	Correlation coefficient (r^2).
** sumx		L	Sum of the x values.
** sumx2	L	Sum of the x^2 values.
** sumxy	L	Sum of the xy products.
** sumy		L	Sum of the y values.
** x		I	Independent variable data values.
** y		I	Dependent variable data values.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int LinearRegression ( int n, float *x, float *y, float *a, float *b ,float *r2)
{	float	sumx, sumx2, sumxy, sumy,sumy2;
	int	i;
	char	message[256], routine[] = "LinearRegression";

	if ( GetSum(n, x, &sumx) )
		return STATUS_FAILURE;
	if ( GetSum(n, y, &sumy) )
		return STATUS_FAILURE;

	if ( n <= 0 ) {
		sprintf ( message, "Number of points = %d <= 0", n );
		PrintWarning ( 2, routine, message );
		return STATUS_FAILURE;
	}

        sumx  = 0.0;
	sumx2 = 0.0;
	sumxy = 0.0;
	sumy  = 0.0;
	sumy2 = 0.0;
	
	for ( i = 0; i < n; i++ ) {
		sumx  += x[i];
		sumx2 += x[i]*x[i];
		sumxy += x[i]*y[i];
		sumy  += y[i];
		sumy2 += y[i]*y[i];
	}

	if ( (n*sumx2 - sumx*sumx) == 0.0 ) {
		PrintWarning ( 1, routine,
		"Denominator on regression equation is zero" );
		return STATUS_FAILURE;
	}
	

	*b = (n*sumxy - sumx*sumy)/(n*sumx2 - sumx*sumx);
	*a = (sumy - (*b)*sumx)/n;
	*r2= ((sumxy)-(sumx*sumy)/n)/ (  sqrt(sumx2-(sumx*sumx)/n)*sqrt(sumy2-(sumy*sumy)/n)  );

	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/LinearRegression.c,v $";
 static char rcs_id2[] = "$Id: LinearRegression.c,v 1.1 1999/02/18 15:17:00 dws Exp $";}
/*  ===================================================  */

}
