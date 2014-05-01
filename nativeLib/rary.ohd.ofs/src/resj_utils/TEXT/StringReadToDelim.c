/* ----------------------------------------------------------------------------
** HMStringReadToDelim - read until a delimiting character has been found
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	Everything read is copied to "string".
**			HMSTATUS_SUCCESS is returned if the end of string is
**			encountered. HMSTATUS_FAILURE is returned if everything
**			goes well.  The delimiter is not included in the string.
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996	Steven A. Malers, RTi	Split out of the HMUtil.c file.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** c		L	Single character.
** delim	I	Delimiter character to indicate end of read.
** string0	I	Pointer to string being read.
** string	O	String to contain characters read.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int StringReadToDelim ( char *string0, char delim, char *string )
{	int	c, i = 0;

	*string = '\0';
	do {	c = string0[i];
		if ( c == delim ) {
			*string = '\0';
			return STATUS_SUCCESS;
		}
		else	*string++ = c;
		i++;
	} while ( c != '\0' );
	return STATUS_FAILURE;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/StringReadToDelim.c,v $";
 static char rcs_id2[] = "$Id: StringReadToDelim.c,v 1.1 1999/02/18 15:17:23 dws Exp $";}
/*  ===================================================  */

}
