/*------------------------------------------------------------------------------
** FreeStringList - free memory from a char ** list
**------------------------------------------------------------------------------
** Notes:	(1)	The list is assumed to be terminated by a NULL string.
**------------------------------------------------------------------------------
** History:
**
** 14 May 1996	SAM, RTi		Update so that (char **)NULL is
**					returned.  This makes it so that a
**					statement like:
**						list = FreeStringList (list);
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** i		L	Counter for strings.
** list		I	List of broken out strings.
** nlist	L	Number of items in list (without trailer).
** routine	L	Name of this routine.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

char **FreeStringList ( char **list )
{	int	i, nlist = 0;
	char	message[256], routine[] = "FreeStringList";

	if ( list != (char **)NULL ) {
		for ( i = 0; list[i] != (char *)NULL; i++ ) {
			free ( list[i] );
			++nlist;
		}
		/* next line for non NULL string list end */
		/*free ( list[nlist - 1] );*/
		free ( list );		/* head */
	}
	sprintf ( message, "Freed list for %d strings + trailer", nlist );
	PrintDebug ( 50, routine, message );
	return (char **)NULL;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/FreeStringList.c,v $";
 static char rcs_id2[] = "$Id: FreeStringList.c,v 1.1 1999/02/18 15:16:40 dws Exp $";}
/*  ===================================================  */

}
