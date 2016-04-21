#include "ResJ.h"
/*
** UnsortF - unsort an array given the original position of the data
**
** notes:	(1)	"sort_order" contains the original position of the
**			values before the sort.  The unsorting is carried out
**			by skipping around the data until all of the original
**			positions are restored.
**
** data		.... data array
** i		.... loop for data
** j		.... loop for data
** ndata	.... number of data
** sort_order	.... original data positions (0 to "ndata - 1")
** temp		.... temporary storage for value
*/
int UnsortF ( float *data, int ndata, int *sort_order )
{	int	i, itemp, j;
	float	temp;

	for ( i = 0; i < ndata; i++ ) {
		for ( j = (i + 1); j < ndata; j++ ) {
			if ( sort_order[j] == i ) {
				temp		= data[j];
				data[j]		= data[i];
				data[i]		= temp;
				itemp		= sort_order[j];
				sort_order[j]	= sort_order[i];
				sort_order[i]	= itemp;
				break;
			}
		}
	}

	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/UnsortF.c,v $";
 static char rcs_id2[] = "$Id: UnsortF.c,v 1.1 1999/02/18 15:17:27 dws Exp $";}
/*  ===================================================  */

}
