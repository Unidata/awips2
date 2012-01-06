/* ----------------------------------------------------------------------------
** PrintError - print error message
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-06-95	Steven A. Malers, RTi	Add multiple output files.
** 16 Nov 95	SAM, RTi		Add message suffix.
** 03 Sep 96	SAM, RTi		Split code out of HMMsg.c and make more
**					stand-alone.
** 11 Sep 96	SAM, RTi		Change to accept variable argument
**					list.  This should help reduce some of
**					the overhead associated with processing
**					messages.
** 09 May 2001	James R. VanShaar, RTi	Reformatted error string to match
**					standard error appearance in NWSRFS
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** ap		L	Pointer to argument list.
** format	I	Format to use for the message.
** ESPerror	G	Global error data.
** msgprefix	G	Message prefix.
** msgsuffix	G	Message suffix.
** i		L	Counter for output files.
** message	L	Free-format message after formatting.
** message2	L	Entire message after formatting with prefix and suffix.
** routine	I	Name of routine printing message.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"
#include "resj/Extern.h"

#ifndef ESPBIGSTRING
#define ESPBIGSTRING 2000
#endif

void PrintError ( char *routine, char *format, ... )
{	int	i;
	char	message[ESPBIGSTRING], message2[ESPBIGSTRING];
	va_list	ap;

	InitFILEs ();

	va_start ( ap, format );
	/*
	** There are no error levels so go ahead and convert the free-format
	** string...
	*/
	vsprintf ( message, format, ap );
	for ( i = 0; i < MAX_MESSAGE_FILE; i++ ) {
		/*
		** Now add the suffix and prefix...
		*/
		if ( *routine ) {
			sprintf ( message2, 
			"0          **ERROR** %s <- %sError (%s)%s",
			message, msgprefix, routine, msgsuffix );
		}
		else {	sprintf ( message2, 
			"0          **ERROR** %s <- %sError%s",
			message, msgprefix, msgsuffix );
		}
		/*** OLD format
		if ( *routine ) {
			sprintf ( message2, "%sError (%s) -> %s%s",
			msgprefix, routine, message, msgsuffix );
		}
		else {	sprintf ( message2, "%sError   -> %s%s",
			msgprefix, message, msgsuffix );
		}
		***/

		/*
		** If output is to a file pointer, print it...
		*/
		if ( ESPerror[i].fp ) {
			fprintf ( ESPerror[i].fp, "%s\n", message2 );
		}
		/*
		** If output is to a function, pass it along...
		*/
		if ( ESPerror[i].func) {
			(ESPerror[i].func)(0, routine, message2 );
		}
	}
	va_end ( ap );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/PrintError.c,v $";
 static char rcs_id2[] = "$Id: PrintError.c,v 1.3 2002/02/15 16:44:35 dws Exp $";}
/*  ===================================================  */

}
