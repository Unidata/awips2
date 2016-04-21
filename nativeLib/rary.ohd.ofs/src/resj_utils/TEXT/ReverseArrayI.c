#include "ResJ.h"

/*------------------------------------------------------------------------------
** ReverseArrayI - reverse an array of integers
**------------------------------------------------------------------------------
** History:
** 6/16/95	Peter T. Abplanalp, RTi		Created routine.
**------------------------------------------------------------------------------
** Notes:
**		(1)	Since we need to swap the top half of the array with
**			the bottom we only need to go from 0 to (ndata / 2).
**		(2)	The original array is overwritten by the new array.
**		(3)	Returns STATUS_SUCCESS for success and
**			STATUS_FAILURE for failure.
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** data		I/O	Incoming array of numbers
** half		L	Half of ndata
** message	L	Global message string.
** i		L	Loop index
** j		L	Array index
** ndata	I	Number of values in data
** routine	L	Routine name
** tempi	L	Temporary variable
**------------------------------------------------------------------------------
*/
int ReverseArrayI (	int *data, int ndata )
{	int	i, j, half, tempi;
	char	routine[] = "ReverseArrayI", message[MAXC];

	if ( ! data ) {
		PrintWarning ( 10, routine, "No array to reverse!" );
		return STATUS_FAILURE;
	}

	half	= ndata / 2;
	j	= ndata - 1;

	for ( i = 0; i < half; i++ ) {
/*		sprintf ( message, "Moving data[%i] (%i) to tempi.", i,
			data[i] );
		PrintDebug ( 50, routine, message );						*/
		tempi	= data[i];
/*		sprintf ( message, "Moving data[%i] (%i) to data[%i].", j,
			data[j], i );
		PrintDebug ( 50, routine, message );						*/
		data[i]	= data[j];
/*		sprintf ( message, "Moving tempi (%i) to data[%i].", tempi,
			j );
		PrintDebug ( 50, routine, message );						*/
		data[j]	= tempi;
		j--;
	}
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/ReverseArrayI.c,v $";
 static char rcs_id2[] = "$Id: ReverseArrayI.c,v 1.1 1999/02/18 15:17:10 dws Exp $";}
/*  ===================================================  */

}

