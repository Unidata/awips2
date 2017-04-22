/* ----------------------------------------------------------------------------
** 
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	Currently, only check to see whether characters are
**			appropriate, but don't worry about order of characters.
** ----------------------------------------------------------------------------
** History:
**
** 29 Jun 2001	James R. VanShaar, RTi	Copied and modified IsInteger.c from
**					Res-J code.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** pt		L	Pointer to string.
** string	I	String to be checked.
** ----------------------------------------------------------------------------
*/

#include <string.h>

#include "ResJ.h"

int IsDouble ( char *string )
{	char	*pt = string;

	while ( *pt ) {
		if ( !strchr(" -0123456789.eE", *pt) )
			return 0;
		++pt;
	}
	return 1;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/IsDouble.c,v $";
 static char rcs_id2[] = "$Id: IsDouble.c,v 1.2 2002/02/13 15:46:54 dws Exp $";}
/*  ===================================================  */

}
