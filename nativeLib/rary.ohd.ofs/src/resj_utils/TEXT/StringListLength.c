/*------------------------------------------------------------------------------
** StringListLength - get the number of items in a string list
**------------------------------------------------------------------------------
** Notes:	(1)	The list is assumed to be terminated by a NULL string.
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** i		L	Counter for strings.
** list		I	List of broken out strings.
** routine	L	Name of this routine.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

int StringListLength ( char **list, int *nlist )
{	int	i;
	char	message[256], routine[] = "StringListLength";

	*nlist = 0;
	if ( list == (char **)NULL ) {
		PrintWarning ( 5, routine, "NULL string list" );
		return STATUS_FAILURE;
	}
	else {	for ( i = 0; list[i] != (char *)NULL; i++ ) {
			++(*nlist);
		}
	}
	sprintf ( message, "List has %d strings + trailer", *nlist );
	PrintDebug ( 50, routine, message );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/StringListLength.c,v $";
 static char rcs_id2[] = "$Id: StringListLength.c,v 1.1 1999/02/18 15:17:22 dws Exp $";}
/*  ===================================================  */

}
