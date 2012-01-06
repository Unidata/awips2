/*-----------------------------------------------------------------------------
** AddToStringList - Add a string to the end of an existing string list
**-----------------------------------------------------------------------------
** Notes:	(1)	This routine adds a single string to the end of a string
**			list.  If the list is NULL, then it is initialized.
**-----------------------------------------------------------------------------
** History:
** 6/26/95	Peter T. Abplanalp, RTi	Created routine.
** 25 Sep 1995	SAM, RTi		Get the list length by calling
**					StringListLength.
** 26 Sep 1995	SAM, RTi		Change so that a NULL pointer as input
**					causes the string list to be intialized.
** 04 Oct 1995	SAM, RTi		Make end a true NULL pointer.
**-----------------------------------------------------------------------------
** Variable	I/O	Description
**
** message	G	Global message string.
** list		I/O	Stringlist that is added to.
** listlen	O	At first, current length of list.  On return, the length
**			of the list.
** routine	L	Name of this routine.
** string	I	String to add.
**-----------------------------------------------------------------------------
*/

#include "ResJ.h"

char **AddToStringList ( char **list, char *string, int *listlen )
{	char	message[256], **newlist, routine[] = "AddToStringList";

	if ( list == (char **)NULL ) {
		/*
		** Allocate an initial list of one item plus the trailer...
		*/
		*listlen = 0;
		newlist = (char **)(malloc((sizeof(char *)*(*listlen + 2))));
	}
	else {	/*
		** Try to get the list length and then expand the list by one
		** (the trailer is not included in the list length and therefore
		** needs to be added in)...
		*/
		if ( StringListLength(list, listlen) ) {
			PrintWarning ( 2, routine,
			"Trouble getting length of string list" );
			return (char **)NULL;
		}
		newlist = (char **)(realloc(list,(sizeof(char *)*(*listlen + 2))));
	}
	if ( newlist == (char **)NULL ) {
		sprintf ( message, 
		"Could not allocate memory for %d items in list.",
		(*listlen + 2) );
		PrintWarning ( 2, routine, message );
		return (char **)NULL;
	}

	/*
	** Now add the specified string...
	*/

	/*
	sprintf ( message, "Adding \"%s\" to list[%d].", string, *listlen );
	PrintDebug ( 50, routine, message );
	*/

	newlist[*listlen] =
		(char *)(malloc((size_t)((strlen(string)+1)*sizeof(char))));
	if ( newlist[*listlen] == (char *)NULL ) {
		sprintf ( message,
		"Could not allocate memory for list[%d].", *listlen );
		PrintWarning ( 2, routine, message );
		return (char **)NULL;
	}
	/*
	sprintf ( message, "Copying \"%s\" as list[%d].", string, *listlen );
	PrintDebug ( 50, routine, message );
	*/
	strcpy ( newlist[*listlen], string );
	++(*listlen);

	/*
	** Add a NULL at the end...
	*/

	newlist[*listlen] = (char *)NULL;

	return newlist;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/AddToStringList.c,v $";
 static char rcs_id2[] = "$Id: AddToStringList.c,v 1.1 1999/02/18 15:16:35 dws Exp $";}
/*  ===================================================  */

}
