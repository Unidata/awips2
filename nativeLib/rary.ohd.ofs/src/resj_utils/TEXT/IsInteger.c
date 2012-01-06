/* ----------------------------------------------------------------------------
** HMIsInteger - determine whether a string is an integer
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	Currently, only check to see whether characters are
**			appropriate, but don't worry about order of characters.
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996	Steven A. Malers, RTi	Split code out of HMUtil.c file.
** 07 Oct 1996	SAM, RTi		Add <string.h> to prototype functions.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** pt		L	Pointer to string.
** string	I	String to be checked.
** ----------------------------------------------------------------------------
*/

#include <string.h>

#include "ResJ.h"

int IsInteger ( char *string )
{	char	*pt = string;

	while ( *pt ) {
		if ( !strchr(" -0123456789", *pt) )
			return 0;
		++pt;
	}
	return 1;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/IsInteger.c,v $";
 static char rcs_id2[] = "$Id: IsInteger.c,v 1.1 1999/02/18 15:16:58 dws Exp $";}
/*  ===================================================  */

}
