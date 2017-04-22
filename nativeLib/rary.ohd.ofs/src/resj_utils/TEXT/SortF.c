#include "ResJ.h"

/* ----------------------------------------------------------------------------
** SortF - sort a list of floating point numbers
** ----------------------------------------------------------------------------
** History:
**
** 06-15-95	Steven A. Malers, RTi		Add warning if method is not
**						allowed.  Clean up
**						documentation.  Add ability to
**						sort into descending order.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** data		I/O	Data values to sort.
** message	G	String for messages.
** i		L	Counter for data values.
** method	I	Method of sorting data.
** ndata	I	Number of data values.
** order	I	Order of sort (ascending or descending).
** sflag	I	Indicates whether "sort_order" is to be calculated.
** sort_order	O	Original locations of data after sort.
** ----------------------------------------------------------------------------
*/
int SortF (	float *data, int ndata, int method, int order, int *sort_order,
		int sflag )
{	char	routine[] = "SortF", message[256];
	int	i;

	/*
	** Initialize "sort_order" to sequential numbers...
	*/
	
	if ( sflag ) {
		for ( i = 0; i < ndata; i++ )
			sort_order[i] = i;
	}

	/*
	** Now sort into ascending order...
	*/

	if ( method != SORT_QUICK ) {
		/*
		** The only method supported now is the SORT_QUICK
		*/
		sprintf ( message,
		"Sort method %d not supported.  Using quick sort", method );
		PrintWarning ( 2, routine, message );
		if ( SortFQuick(data, ndata, sort_order, sflag) )
			return STATUS_FAILURE;
	}
	else {
		if ( SortFQuick(data, ndata, sort_order, sflag) )
			return STATUS_FAILURE;
	}

	/*
	** Now check to see if the arrays need to be reversed for descending
	** order...
	*/

	if ( order == SORT_DESCENDING ) {
		if ( ReverseArrayF(data, ndata) )
			return STATUS_FAILURE;
		if ( sflag ) {
			if ( ReverseArrayI(sort_order, ndata) )
				return STATUS_FAILURE;
		}
	}

	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SortF.c,v $";
 static char rcs_id2[] = "$Id: SortF.c,v 1.1 1999/02/18 15:17:18 dws Exp $";}
/*  ===================================================  */

}

