/* ----------------------------------------------------------------------------
** SetDebugFILE - set file pointer for debug messages
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-06-95	Steven A. Malers, RTi	Add flag for file pointer #.
** 03 Sep 96	SAM, RTi		Pull code out of HMMsg.c and make it
**					more stand-alone.
** 24 Sep 96	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
**
** Variable	I/O	Description
**
** dbFILE	I	Pointer to open debug FILE.
** filenum	I	Debug file number.
** message	L	String for messages.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetDebugFILE ( FILE *dbFILE, int filenum )
{	char		message[256], routine[] = "SetDebugFILE";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
		"Request to set debug FILE %d denied.  %d is maximum file #",
		filenum, (MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	ESPdebug[filenum].fp = dbFILE;
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetDebugFILE.c,v $";
 static char rcs_id2[] = "$Id: SetDebugFILE.c,v 1.2 2000/05/18 13:08:33 dws Exp $";}
/*  ===================================================  */

}
