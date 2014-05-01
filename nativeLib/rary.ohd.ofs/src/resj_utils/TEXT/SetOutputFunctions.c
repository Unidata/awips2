/* ----------------------------------------------------------------------------
** SetOutputFunctions - set function pointer for all messages
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine sets the output pointers to the specified
**			function.
** ----------------------------------------------------------------------------
** History:
**
** 06-12-95	Steven A. Malers, RTi	Add routine.
** 05 Sep 96	SAM, RTi		Break code out of Msg.c and make more
**					stand-alone.
** 17 Sep 96	CEN, RTi		Copied from HMData to ESPUtil
** 09 Apr 01	James R. VanShaar, RTi	Modified input parameters and algorithm
**					to assign functions to individul ESP 
**					types
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** filenum	I	Output file number.
** message	L	String for messages.
** func		I	Pointer to output function.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetOutputFunctions ( int (*func)(int, char *, char *), int filenum,
	char *type )
{	char	message[256], routine[] = "SetOutputFunctions";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
			"Request to set output function %d denied.  %d is "
			"maximum function #", (MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	/***
	//SetDebugFILE ( oFILE, filenum );
	//SetErrorFILE ( oFILE, filenum );
	//SetStatusFILE ( oFILE, filenum );
	//SetWarningFILE ( oFILE, filenum );
	***/

	/***
	// The following 'type' logic triggers based on the unique values
	// the first character of char *type will take.  The full string is
	// not considered.
	// 	Valid type descriptions (1st letter is case insensitive trigger)
	//		DEBUG, ERROR, STATUS, WARNING
	***/
	switch ( type[0] ) {
	   case 'D': case 'd':
		ESPdebug[filenum].func   = func;
		break;
	   case 'E': case 'e':
		ESPerror[filenum].func   = func;
		break;
	   case 'S': case 's':
		ESPstatus[filenum].func  = func;
		break;
	   case 'W': case 'w':
		ESPwarning[filenum].func = func;
		break;
	   default:
		return STATUS_FAILURE;
		break;
	}

	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetOutputFunctions.c,v $";
 static char rcs_id2[] = "$Id: SetOutputFunctions.c,v 1.3 2002/02/13 14:26:56 dws Exp $";}
/*  ===================================================  */

}
