#include "ResJ.h"

/*------------------------------------------------------------------------------
** GetMax - find the maximum value in a list
**------------------------------------------------------------------------------
** History:
**
** 05-25-95	Steven A. Malers, RTi		Change to return status and
**						pass maximum via parameter list.
** 07-11-96 Mark S. Woodbury, RTi		Added argument to return the
**						number of values to reach max.
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** i		L	Loop counter for points.
** max		O	Maximum of the values.
** n		I	Number of data values.
** routine	L	Name of this routine.
** x		I	Data values.
** ndmx		O	Number of data points to reach max
**------------------------------------------------------------------------------
*/
int GetMax ( int n, float *x, float *max , int *ndmx)
{	int	i;
	char	routine[] = "Max";

	*max = 0.0;
	*ndmx = 0;

	if ( n <= 0 ) {
		PrintWarning ( 50, routine,
		"Number of points <= 0.  Returning max = 0.0" );
		return STATUS_FAILURE;
	}
	*max = x[0];
	for ( i = 1; i < n; i++ ) {
		if ( x[i] > *max ) {
			*max = x[i];
			*ndmx = i;
		}
	}
	return STATUS_SUCCESS;
}

/*------------------------------------------------------------------------------
** GetMean - calculate the mean for a list of numbers
**------------------------------------------------------------------------------
** History:
**
** 05-25-95	Steven A. Malers, RTi		Make function return a status
**						and pass mean in parameter list.
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** mean		O	Mean of the values.
** message	L	String for messages.
** n		I	Number of data values.
** routine	L	Name of this routine.
** sum		L	Sum of data values.
** x		I	Data values.
**------------------------------------------------------------------------------
*/
int GetMean ( int n, float *x, float *mean )
{	char	routine[] = "Mean", message[80];
	float	sum;
	
	*mean = 0.0;
	if ( n <= 0 ) {
		sprintf ( message, "Number of points = %d <= 0", n );
		PrintWarning ( 50, routine, message );
		return STATUS_FAILURE;
	}
	if ( GetSum(n, x, &sum) )
		return STATUS_FAILURE;
	*mean = sum/(float)(n);
	return STATUS_SUCCESS;
}


/*------------------------------------------------------------------------------
** GetMin - find the minimum value in a list
**------------------------------------------------------------------------------
** History:
**
** 05-25-95	Steven A. Malers, RTi		Return exit status and pass
**						minimum via parameter list.
** 07-11-96 Mark S. Woodbury, RTi		Added argument to return the
**						number of values to reach min.
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** i		L	Loop counter for data points.
** min		O	Minimum of the values.
** n		I	Number of data values.
** routine	L	Name of this routine.
** x		I	Data values.
** ndmn		O	Number of data points to reach min
**------------------------------------------------------------------------------
*/
int GetMin ( int n, float *x, float *min, int *ndmn )
{	int	i;
	char	routine[] = "Min";

	*min = 0.0;
	*ndmn = 0;

	if ( n <= 0 ) {
		PrintWarning ( 50, routine,
		"Number of points <= 0.  Returning min = 0.0" );
		return STATUS_FAILURE;
	}
	*min = x[0];
	for ( i = 1; i < n; i++ ) {
		if ( x[i] < *min ) {
		*min = x[i];
		*ndmn = i;
		}
	}
	return STATUS_SUCCESS;
}


/*------------------------------------------------------------------------------
** GetSum - calculate the sum of a list of numbers
**------------------------------------------------------------------------------
** History:
**
** 05-25-95	Steven A. Malers, RTi		Change so that the routine
**						returns and exit status and
**						passed sum via parameter list.
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** i		L	Loop counter for data values.
** n		I	Number of data values.
** x		I	Data values.
**------------------------------------------------------------------------------
*/
int GetSum ( int n, float *x, float *sum )
{	int	i;

	*sum = 0.0;

	for ( i = 0; i < n; i++ )	*sum += x[i];
	return STATUS_SUCCESS;
}

/*------------------------------------------------------------------------------
** GetNdto - calculate the number of data elements to reach a value
**------------------------------------------------------------------------------
** History:
**
** 07-11-96 Mark S. Woodbury, RTi		Initial version for ESPADP
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** i		L	Loop counter for data values.
** n		I	Number of data values.
** x		I	Data values.
** criteria	I	The criteria for testing data values.
** direction	I	Above = 1, below = -1;
** ndto     	I	Number of values before reaching criteria.
**------------------------------------------------------------------------------
*/
int GetNdto ( int n, float criteria, int direction, float *x, int *ndto )
{	int	i;
	char	routine[] = "Ndto";

	*ndto = 0;

	if ( n <= 0 ) {
		PrintWarning ( 50, routine,
		"Number of points <= 0.  Returning ndto = 0.0" );
		return STATUS_FAILURE;
	}

	for ( i = 0; i < n; i++ ) {
		if ( x[i] * direction >= criteria * direction ) {
			*ndto = i;
			return STATUS_SUCCESS;
		}
	}
	
	*ndto = n;
	return STATUS_SUCCESS;

}


/*------------------------------------------------------------------------------
** GetNdis - calculate the number of data elements above/below a value
**------------------------------------------------------------------------------
** History:
**
** 07-11-96 Mark S. Woodbury, RTi		Initial version for ESPADP
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** i		L	Loop counter for data values.
** n		I	Number of data values.
** x		I	Data values.
** criteria	I	The criteria for testing data values.
** direction	I	Above = 1, below = -1;
** ndis     	I	Number of values before reaching criteria.
**------------------------------------------------------------------------------
*/
int GetNdis ( int n, float criteria, int direction, float *x, int *ndis )
{	int	i, j;
	char	routine[] = "Ndis";


	if ( n <= 0 ) {
		PrintWarning ( 50, routine,
		"Number of points <= 0.  Returning ndto = 0.0" );
		return STATUS_FAILURE;
	}

	for ( i = 0, j = 0; i < n; i++ )
          {		
		if ( x[i] * direction >= criteria * direction ) 
			 j++;
	  }
	*ndis = j; 

	return STATUS_SUCCESS;
}

/*------------------------------------------------------------------------------
** Normalize - Adjust a list of numbers to sum to 1.0
**------------------------------------------------------------------------------
** History:
**
** 07-11-96 Mark S. Woodbury, RTi		Initial version for ESPADP
**------------------------------------------------------------------------------
** Variables	I/O	Description
**
** i		L	Loop counter for data values.
** data		I/O	Data values.
** n		I	Number of data values.
** sum     	L	Sum of the original data values.
**------------------------------------------------------------------------------
*/
int Normalize (int n, float *data)
{
	float sum;
	int i;
	char	routine[] = "Normalize";

	GetSum ( n, data, &sum);
	if ( sum == 0.0 ) {
		PrintWarning ( 50, routine,
		"Sum of weights is  0.  List cannot be normalized" );
		return STATUS_FAILURE;
	}
	for ( i = 0; i < n; i++)
		data[i] *= ( 1.0 / sum);
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/Stat.c,v $";
 static char rcs_id2[] = "$Id: Stat.c,v 1.1 1999/02/18 15:17:21 dws Exp $";}
/*  ===================================================  */

}
