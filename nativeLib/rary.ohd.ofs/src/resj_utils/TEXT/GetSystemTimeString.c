/* ----------------------------------------------------------------------------
** GetSystemTimeString - get current time from system clock (as string)
** ----------------------------------------------------------------------------
** Copyright:   See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine calls the "strftime" C library routine
**			using "format".
** ----------------------------------------------------------------------------
** History:
**
** 29 Nov 1995	Steven A. Malers, RTi	Add return status.
** 01 Dec 1995	SAM, RTi		Add special format "datum_seconds" to
**					get the number of seconds since 1970
**					(or whatever the standard datum is).
** 06 Sep 1996  Steven A. Malers, RTi   Split out of the HMTD.c file.
** 27 Sep 1996	Catherine E. Nutting, RTi	Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** format0	I	Time format as requested by calling program.
** format	L	Time format for "strftime".
** stime	O	Time as string.
** t		L	Time data structure.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"
#include <time.h>

int GetSystemTimeString ( char *stime, char *format0 )
{	time_t	t;
	char	default_format[] = "%a %b %d %H:%M:%S %Z %Y", format[256];

	if ( format0 == (char *)NULL ) {
		strcpy ( format, default_format );
	}
	else if ( *format0 != '\0' ) {
		strcpy ( format, format0 );
	}
	else {	strcpy ( format, default_format );
	}
	t = time ( NULL );
	if ( !strcmp(format, "datum_seconds") ) {
		/*
		** Want the number of seconds since the standard time datum	
		*/
		sprintf ( stime, "%u", t );
	}
	else {	/*
		** Convert format to string...
		*/
		strftime ( stime, 256, format, localtime(&t) );
	}

	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetSystemTimeString.c,v $";
 static char rcs_id2[] = "$Id: GetSystemTimeString.c,v 1.1 1999/02/18 15:16:50 dws Exp $";}
/*  ===================================================  */

}
