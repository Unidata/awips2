/* ----------------------------------------------------------------------------
** SetWarningFILE - set file pointer for warning messages
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-06-95	Steven A. Malers, RTi	Add flag for file pointer #.
** 05 Sep 96	SAM, RTi		Break out of HMMsg.c and make more
**					stand-alone.
** 24 Sep 96	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** waFILE	I	Pointer to open warning FILE.
** filenum	I	Warning file number.
** message	L	String for messages.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetWarningFILE ( FILE *waFILE, int filenum )
{	char	message[256], routine[] = "SetWarningFILE";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
		"Request to set warning FILE %d denied.  %d is maximum file #",
		filenum, (MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	ESPwarning[filenum].fp = waFILE;
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetWarningFILE.c,v $";
 static char rcs_id2[] = "$Id: SetWarningFILE.c,v 1.2 2000/05/18 13:08:35 dws Exp $";}
/*  ===================================================  */

}
