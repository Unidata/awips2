/* ----------------------------------------------------------------------------
** PrintDebug - print debug message
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-06-95	Steven A. Malers, RTi	Add multiple output files.
** 14 Nov 95	SAM, RTi		Add HMmsgsuffix.
** 03 Sep 96	SAM, RTi		Split code out of HMMsg.c and make
**					more stand-alone.
** 11 Sep 96	SAM, RTi		Change to accept variable argument
**					list.  This should help reduce some of
**					the overhead associated with processing
**					messages.  Also add the option of
**					showing the debug level in the message.
** 17 Sep 96	CEN, RTi		Copied from HMData to ESPUtil.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** ap		L	Pointer to argument list.
** dl		I	Debug level for message.
** dlstring	L	Debug level string.
** format	I	Format to use for the message.
** debug	G	Global debug data.
** msgprefix	G	Message prefix.
** showdebuglevel G	Indicates whether debug level should be displayed.
** i		L	Counter for output files.
** message	L	Free-format message after formatting.
** message2	L	Entire message after formatting with prefix and suffix.
** routine	I	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

#ifndef ESPBIGSTRING
#define ESPBIGSTRING 2000
#endif

void PrintDebug ( int dl, char *routine, char *format, ... )
{	int	i;
	char	dlstring[32], message[ESPBIGSTRING], message2[ESPBIGSTRING];
	va_list	ap;

	InitFILEs ();

	va_start ( ap, format );
	for ( i = 0; i < MAX_MESSAGE_FILE; i++ ) {
		if ( ESPdebug[i].level >= dl ) {
			/*
			** First convert the free-format string...
			*/
			vsprintf ( message, format, ap );
			/*
			** Get the debug level as a string...
			*/
			if ( showdebuglevel ) {
				sprintf ( dlstring, "[%3d]", dl );
			}
			else {	dlstring[0] = '\0';
			}
			/*
			** Now we have the free-format string and can format
			** with our normal prefix and suffix...
			*/
			if ( *routine ) {
				sprintf ( message2,
				"%sDebug%s (%s) -> %s%s",
				msgprefix, dlstring, routine, message,
				msgsuffix );
			}
			else {	sprintf ( message2, "%sDebug%s   -> %s%s",
				msgprefix, dlstring, message, msgsuffix );
			}
			/*
			** If output is to a file pointer, print it...
			*/
			if ( ESPdebug[i].fp ) {
				fprintf ( ESPdebug[i].fp, "%s\n", message2 );
			}
			/*
			** If output is to a function, pass it along...
			*/
			if ( ESPdebug[i].func ) {
				(ESPdebug[i].func)( dl, routine, message2 );
			}
		}
	}
	va_end ( ap );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/PrintDebug.c,v $";
 static char rcs_id2[] = "$Id: PrintDebug.c,v 1.2 2000/05/18 13:08:33 dws Exp $";}
/*  ===================================================  */

}
