/*
*******************************************************************************
** ReplaceChar - replaces all instances of a character in a string.
*******************************************************************************
** Copyright:	See the COPYRIGHT file.
*******************************************************************************
** Notes:
**
*******************************************************************************
** History:
** 
** 17 Feb 1998	Matthew J. Rutherford, RTi	Created initial routine.
*******************************************************************************
** Variables:	I/O	Description		
**
** fc		I	Character to convert from.
** string	I/O	String for perform replacements on.
** tc		I	Character to convert to.
*******************************************************************************
*/
#include "ResJ.h"

int ReplaceChar( char* string, char fc, char tc )
{
	char	*pt=NULL, routine[]="ReplaceChar";

	if( string == NULL ){
		PrintWarning( 1, routine,
		"Incoming string is NULL." );
		return( STATUS_FAILURE );
	}

	pt = string;

	while( *(pt) != '\0' ){
		if( *pt == fc ){
			*pt = tc;
		}
		++pt;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/ReplaceChar.c,v $";
 static char rcs_id2[] = "$Id: ReplaceChar.c,v 1.1 1999/02/18 15:17:08 dws Exp $";}
/*  ===================================================  */

}
