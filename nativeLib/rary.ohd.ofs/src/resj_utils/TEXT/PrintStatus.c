/* ----------------------------------------------------------------------------
** PrintStatus - print status message
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-06-95	Steven A. Malers, RTi	Add multiple output files.
** 06-28-95	SAM, RTi		Change so that printing the routine
**					name is tied to the current status
**					level, not the message level.
** 16 Nov 95	SAM, RTi		Add message suffix.
** 11 Sep 96	SAM, RTi		Change to accept variable argument
**					list.  This should help reduce some of
**					the overhead associated with processing
**					messages.  Also add the option of
**					showing the debug level in the message.
** 17 Sep 96	CEN, RTi		Copied from HMData to ESPUtil
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** ap		L	Pointer to argument list.
** format	I	Format to use for the message.
** msgprefix	G	Prefix for messages.
** msgsuffix	G	Suffix for messages.
** showstatuslevel G	Indicates whether status level should be displayed.
** status	G	Global status data.
** i		L	Counter for output files.
** message	L	Free-format message after formatting.
** message2	L	Entire message after formatting with prefix and suffix.
** routine	I	Name of routine printing message.
** sl		I	Status level for message.
** slstring	L	Status level string.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

#ifndef ESPBIGSTRING
#define ESPBIGSTRING 2000
#endif

void PrintStatus ( int sl, char *routine, char *format, ... )
{	int	i;
	char	message[ESPBIGSTRING], message2[ESPBIGSTRING], slstring[32];
	va_list	ap;

	InitFILEs ();

	va_start ( ap, format );
	for ( i = 0; i < MAX_MESSAGE_FILE; i++ ) {
		if ( ESPstatus[i].level >= sl ) {
			/*
			** First convert the free-format string...
			*/
			vsprintf ( message, format, ap );
			/*
			** Get the debug level as a string...
			*/
			if ( showstatuslevel ) {
				sprintf ( slstring, "[%3d]", sl );
			}
			else {	slstring[0] = '\0';
			}
			/*
			** Now we have the free-format string and can format
			** with our normal prefix and suffix...
			*/
			if ( *routine ) {
				if ( ESPstatus[i].level == 1 ) {
					sprintf ( message2,
					"%sStatus%s  -> %s%s",
					msgprefix, slstring, message,
					msgsuffix );
				}
				else {	sprintf ( message2,
					"%sStatus%s (%s) -> %s%s",
					msgprefix, slstring, routine, message,
					msgsuffix );
				}
			}
			else {	sprintf ( message2, "%sStatus%s  -> %s%s",
				msgprefix, slstring, message, msgsuffix );
			}
			/*
			** If output is to a file pointer, print it...
			*/
			if ( ESPstatus[i].fp ) {
				fprintf ( ESPstatus[i].fp, "%s\n", message2 );
			}
			/*
			** If output is to a function, pass it along...
			*/
			if ( ESPstatus[i].func ) {
				(ESPstatus[i].func)( sl, routine, message2 );
			}
		}
	}
	va_end ( ap );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/PrintStatus.c,v $";
 static char rcs_id2[] = "$Id: PrintStatus.c,v 1.2 2000/05/18 13:08:33 dws Exp $";}
/*  ===================================================  */

}
