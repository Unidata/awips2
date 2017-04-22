/*------------------------------------------------------------------------------
** ArrayToStringList - create a list of strings from an array of strings
**------------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
**------------------------------------------------------------------------------
** Notes:	(1)	The list is of type char **.  The input is a char *[],
**			like "argv".
**		(2)	Call "HMFreeStringList" when done with the list.
**		(3)	The list always has one NULL element at the end so that
**			we know how to free the memory.  However, "nlist" does
**			not include this element.
**------------------------------------------------------------------------------
** History:
**
** 06-21-95	Steven A. Malers, RTi	Created routine.
** 04 Oct 1995	SAM, RTi		Use HMAddToStringList to do the work.
** 05 Sep 1996	SAM, RTi		Break out of the HMUtil.c file.
** 27 Sep 1996	Catherine E. Nutting,RTi	Copied from HMData
**------------------------------------------------------------------------------
** Variable	I/O	Description
**
** array	I	Array of strings.
** i		L	Loop counter for strings.
** len		L	Length of each string.
** list		L	List of strings.
** message	L	String for message.
** nlist	I	Number of strings in the array.
** nlist2	L	Number of strings in string list.
** routine	L	Name of this routine.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"

char **ArrayToStringList ( int nlist, char *array[] )
{	int	i, len, nlist2;
	char	**list = (char **)NULL, message[256],
		routine[] = "ArrayToStringList";

	if ( nlist <= 0 ) {
		PrintWarning ( 5, routine, "<= 0 items in list" );
		return (char **)NULL;
	}
	for ( i = 0; i < nlist; i++ ) {
		list = AddToStringList ( list, array[i], &nlist2 );
	}
	if ( list == (char **)NULL ) {
		PrintWarning ( 5, routine,
		"Unable to convert array to string list" );
		return (char **)NULL;
	}
	sprintf ( message,
	"Created list for %d strings + trailer", nlist );
	PrintDebug ( 5, routine, message );
	return list;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/ArrayToStringList.c,v $";
 static char rcs_id2[] = "$Id: ArrayToStringList.c,v 1.1 1999/02/18 15:16:36 dws Exp $";}
/*  ===================================================  */

}
