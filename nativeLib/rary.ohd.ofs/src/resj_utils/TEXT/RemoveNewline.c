/* ----------------------------------------------------------------------------
** RemoveNewline - remove newline from string
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996	Steven A. Malers, RTi	Split code out of HMUtil.c file.
** 27 Sep 1996	Catherine E. Nutting, Rti	Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** pt		L	Pointer to string.
** pt2		L	Second pointer to string.
** string	I/O	String to manipulate.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int RemoveNewline ( char *string )
{	char	*pt = string, *pt2;

	while ( *pt ) {
		if ( *pt == '\n' ) {
			/*
			** See if the character after newline(s) is the end
			** of the string...
			*/
			pt2 = pt;
			while ( *pt2 && (*pt2 == '\n') )
				++pt2;
			if ( *pt2 == '\0' ) {
				/*
				** Nothing after the newline(s)...
				*/
				*pt = '\0';
				return STATUS_SUCCESS;
			}
			else {	/*
				** Something after the newline(s)...
				*/
			 	*pt = ' ';
			}
		}
		++pt;
	}
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/RemoveNewline.c,v $";
 static char rcs_id2[] = "$Id: RemoveNewline.c,v 1.1 1999/02/18 15:17:08 dws Exp $";}
/*  ===================================================  */

}
