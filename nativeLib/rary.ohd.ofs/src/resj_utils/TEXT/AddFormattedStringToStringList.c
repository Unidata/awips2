/*
*******************************************************************************
** AddFormattedStringToStringList - formats a string and adds it to a list.
*******************************************************************************
** Copyright:	See the COPYRIGHT file.
*******************************************************************************
** Notes:
**
*******************************************************************************
** History:
** 
** 20 Jan 1997	Matthew J. Rutherford, RTi	Created initial version.
*******************************************************************************
** Variables:	I/O	Description		
**
**
*******************************************************************************
*/
#include "ResJ.h"

#ifndef ESPBIGSTRING
#define ESPBIGSTRING 2000
#endif

char** AddFormattedStringToStringList( char** list, int* nlist, 
	char* format, ... )
{
	char	message[ESPBIGSTRING],
		routine[]="AddFormattedStringToStringList";
	va_list	ap;

	va_start( ap, format );

	vsprintf( message, format, ap );

	va_end( ap );

	return( AddToStringList( list, message, nlist ) );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/AddFormattedStringToStringList.c,v $";
 static char rcs_id2[] = "$Id: AddFormattedStringToStringList.c,v 1.1 1999/02/18 15:16:33 dws Exp $";}
/*  ===================================================  */

}
