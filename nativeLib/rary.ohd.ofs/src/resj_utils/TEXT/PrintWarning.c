/* ----------------------------------------------------------------------------
** PrintWarning - print warning message
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-06-95	Steven A. Malers, RTi	Add multiple output files.
** 06-28-95	SAM, RTi		Change so that printing the routine
**					name is tied to the current debug level,
**					not the message level.
** 16 Nov 95	SAM, RTi		Add message suffix.
** 03 Sep 96	SAM, RTi		Split code out of HMMsg.c and make more
**					stand-alone.
** 11 Sep 96	SAM, RTi		Change to accept variable argument
**					list.  This should help reduce some of
**					the overhead associated with processing
**					messages.  Also add the option of
**					showing the debug level in the message.
** 17 Sep 96	CEN, RTi		Copied from HMData to ESPUtil
** 09 May 2001	James R. VanShaar, RTi	Reformatted warning string to match
**					standard warning appearance in NWSRFS
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** ap		L	Pointer to argument list.
** format	I	Format to use for the message.
** msgprefix	G	Message prefix.
** msgsuffix	G	Message suffix.
** warning	G	Global warning data.
** warning_count G	Global warning count.
** i		L	Counter for output files.
** message	I	Free-format message after formatting.
** message2	L	Entire message after formatting with prefix and suffix.
** routine	I	Name of routine printing message.
** wl		I	Warning level for message.
** wlstring	L	Warning level string.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

#ifndef ESPBIGSTRING
#define ESPBIGSTRING 2000
#endif

void PrintWarning ( int wl, char *routine, char *format, ... )
{	int	i;
	char	message[ESPBIGSTRING], message2[ESPBIGSTRING], wlstring[32];
	va_list	ap;

	InitFILEs ();

	++warning_count;

	va_start ( ap, format );
	for ( i = 0; i < MAX_MESSAGE_FILE; i++ ) {
		if ( ESPwarning[i].level >= wl ) {
			/*
			** First convert the free-format string...
			*/
			vsprintf ( message, format, ap );
			/*
			** Get the warning level as a string...
			*/
			if ( showwarninglevel ) {
				sprintf ( wlstring, "[%3d]", wl );
			}
			else {	wlstring[0] = '\0';
			}
			/*
			** Now we have the free-format string and can format
			** with our normal prefix and suffix...
			*/
			if ( *routine ) {
				if ( ESPwarning[i].level == 1 ) {
					sprintf ( message2,
					// "0**WARNING** %s <- %sWarning%s%s",
					// message, msgprefix, wlstring,
					"0**WARNING** %s %s%s",
					message, wlstring,
					msgsuffix );
				}
				else {	sprintf ( message2,
					// "0**WARNING** %s <- %sWarning%s (%s)%s",
					// message, msgprefix, wlstring, routine,
					"0**WARNING** %s %s (%s)%s",
					message, wlstring, routine,
					msgsuffix );
				}
			}
			else {	sprintf ( message2,
				// "0**WARNING** %s <- %sWarning%s (%s)%s",
				// message, msgprefix, wlstring, msgsuffix );
				"0**WARNING** %s %s (%s)%s",
				message, wlstring, msgsuffix );
			}
			/**** Old format
			if ( *routine ) {
				if ( ESPwarning[i].level == 1 ) {
					sprintf ( message2,
					"%sWarning%s -> %s%s",
					msgprefix, wlstring, message,
					msgsuffix );
				}
				else {	sprintf ( message2,
					"%sWarning%s (%s) -> %s%s",
					msgprefix, wlstring, routine, message,
					msgsuffix );
				}
			}
			else {	sprintf ( message2,
				"%sWarning%s -> %s%s", msgprefix, wlstring,
				 message, msgsuffix );
			}
			****/

			/*
			** If output is to a file pointer, print it...
			*/
			if ( ESPwarning[i].fp ) {
				fprintf ( ESPwarning[i].fp, "%s\n", message2 );
			}
			/*
			** If output is to a function, pass it along...
			*/
			if ( ESPwarning[i].func ) {
				(ESPwarning[i].func)( wl, routine, message2 );
			}
		}
	}
	va_end ( ap );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/PrintWarning.c,v $";
 static char rcs_id2[] = "$Id: PrintWarning.c,v 1.4 2005/06/29 17:29:06 wkwock Exp $";}
/*  ===================================================  */

}
