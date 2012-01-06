/*------------------------------------------------------------------------------
** AddListToStringList - combine two string lists
**------------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
**------------------------------------------------------------------------------
** Notes:	(1)	The list is assumed to be terminated by a NULL string.
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** i		L	Counter for strings.
** list		I	List of broken out strings.
** routine	L	Name of this routine.
**------------------------------------------------------------------------------
** History:
**
** 05 Sep 96	Steven A. Malers, RTi	Break code out of HMUtil.c.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

char **AddListToStringList ( char **list, char **listtoadd, int *nlist )
{	int	i;
	char	**newlist, routine[] = "AddListToStringList";

	if ( listtoadd == (char **)NULL ) {
		/*
		** Null list to add.  Return the original list...
		*/
		PrintWarning ( 5, routine, "NULL list to add" );
		StringListLength ( list, nlist );
		return list;
	}
	newlist = list;
	for ( i = 0; listtoadd[i] != (char *)NULL; i++ ) {
		newlist = AddToStringList ( newlist, listtoadd[i], nlist );
		if ( newlist == (char **)NULL ) {
			PrintWarning ( 5, routine,
			"Unable to add string item %d in list", i );
			return newlist;
		}
	}
	return newlist;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/AddListToStringList.c,v $";
 static char rcs_id2[] = "$Id: AddListToStringList.c,v 1.1 1999/02/18 15:16:34 dws Exp $";}
/*  ===================================================  */

}
