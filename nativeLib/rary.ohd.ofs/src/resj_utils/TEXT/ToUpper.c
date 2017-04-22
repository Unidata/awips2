/* ----------------------------------------------------------------------------
** HMToUpper - convert a string to uppercase
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	The original string is destroyed.
** ----------------------------------------------------------------------------
** History:
**
** 04 Dec 95	Steven A. Malers, RTi	Original version of routine.
** 06 Sep 96	SAM, RTi		Split out of the HMUtil.c file.
** 07 Sep 96	SAM, RTi		Add <ctype.h> to prototype functions.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** pt		L	Pointer to string.
** string	I/O	String to convert.
** ----------------------------------------------------------------------------
*/

#include <ctype.h>

#include "ResJ.h"

int ToUpper ( char *string )
{	char	*pt;

	/*
	** Check for NULL string...
	*/

	if ( string == (char *)NULL ) {
		return STATUS_FAILURE;
	}

	pt = string;
	while ( *pt != '\0' ) {
		*pt = toupper ( *pt );
		++pt;
	}

	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/ToUpper.c,v $";
 static char rcs_id2[] = "$Id: ToUpper.c,v 1.1 1999/02/18 15:17:25 dws Exp $";}
/*  ===================================================  */

}
