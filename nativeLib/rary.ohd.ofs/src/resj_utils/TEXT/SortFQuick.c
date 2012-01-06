#include "ResJ.h"
/* ----------------------------------------------------------------------------
** SortFQuick - quick sort on a list of floating point values
** ----------------------------------------------------------------------------
** Notes:	(1)	Default order is ascending (SORT_ASCENDING).
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** data		I	Data values to sort.
** ndata	I	Number of data values.
** sflag	I	Indicates whether the sort_order array should be
**			filled.
** sort_order	O	Original order of data after sort.
** ----------------------------------------------------------------------------
*/
int SortFQuick (	float *data, int ndata, int *sort_order, int sflag )
{	int	i, ia, ib, insertmax = 7, ir = ndata - 1, *istack, itemp, j,
		jstack = 0, k, l = 0, NSTACK = 500;
	float	a, temp;
	char	routine[] = "SortFHeap", string[MAXC];

	istack = (int *)(malloc(NSTACK*sizeof(int)));
	if ( !istack ) {
		sprintf ( string, "Unable to malloc for \"istack[%d]\"",
		NSTACK );
		PrintWarning ( 2, routine, string );
		return STATUS_FAILURE;
	}
	while ( 1 ) {
		if ( (ir - l) < insertmax ) {
			for ( j = (l + 1); j <= ir; j++ ) {
				a	= data[j];
				if ( sflag ) {
					ia	= sort_order[j];
				}
				for ( i = (j - 1); i >= 0; i-- ) {
					if ( data[i] <= a )
						break;
					data[i + 1] = data[i];
					if ( sflag ) {
						sort_order[i+1]	= sort_order[i];
					}
				}
				data[i + 1] = a;
				if ( sflag )
					sort_order[i + 1] = ia;
			}
			if ( !jstack )
				break;
			ir	= istack[jstack--];
			l	= istack[jstack--];
		}
		else {	k = (l + ir)/2;
			temp		= data[k];
			data[k]		= data[l + 1];
			data[l + 1]	= temp;
			if ( sflag ) {
				itemp		= sort_order[k];
				sort_order[k]	= sort_order[l + 1];
				sort_order[l+1]	= itemp;
			}
			if ( data[l + 1] > data[ir] ) {
				temp		= data[l + 1];
				data[l + 1]	= data[ir];
				data[ir]	= temp;
				if ( sflag ) {
					itemp		= sort_order[l + 1];
					sort_order[l+1]	= sort_order[ir];
					sort_order[ir]	= itemp;
				}
			}
			if ( data[l] > data[ir] ) {
				temp		= data[l];
				data[l]		= data[ir];
				data[ir]	= temp;
				if ( sflag ) {
					itemp		= sort_order[l];
					sort_order[l]	= sort_order[ir];
					sort_order[ir]	= itemp;
				}
			}
			if ( data[l + 1] > data[l] ) {
				temp		= data[l + 1];
				data[l + 1]	= data[l];
				data[l]		= temp;
				if ( sflag ) {
					itemp		= sort_order[l + 1];
					sort_order[l+1]	= sort_order[l];
					sort_order[l]	= itemp;
				}
			}
			i	= l + 1;
			j	= ir;
			a	= data[l];
			if ( sflag ) {
				ia	= sort_order[l];
			}
			while ( 1 ) {
				do {	i++;
				} while ( data[i] < a );
				do {	j--;
				} while ( data[j] > a );
				if ( j < i )
					break;
				temp		= data[i];
				data[i]		= data[j];
				data[j]		= temp;
				if ( sflag ) {
					itemp		= sort_order[i];
					sort_order[i]	= sort_order[j];
					sort_order[j]	= itemp;
				}
			}
			data[l]	= data[j];
			data[j] = a;
			if ( sflag ) {
				sort_order[l] = sort_order[j];
				sort_order[j] = ia;
			}
			jstack	+= 2;
			if ( jstack > (NSTACK - 1) ) {
				sprintf ( string,
				"NSTACK (%d) too small in sort", NSTACK );
				if ( istack )
					free ( istack );
				PrintWarning ( 2, routine, string );
				return STATUS_FAILURE;
			}
			if ( (ir - i + 1) >= (j - l) ) {
				istack[jstack]		= ir;
				istack[jstack - 1]	= i;
				ir			= j - 1;
			}
			else {	istack[jstack]		= j - 1;
				istack[jstack - 1]	= l;
				l			= i;
			}
		}
	}
	if ( istack )
		free ( istack );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SortFQuick.c,v $";
 static char rcs_id2[] = "$Id: SortFQuick.c,v 1.1 1999/02/18 15:17:19 dws Exp $";}
/*  ===================================================  */

}

