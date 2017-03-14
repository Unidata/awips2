/* ----------------------------------------------------------------------------
** SetStatusFILE - set file pointer for status messages
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
** stFILE	I	Pointer to open error FILE.
** filenum	I	Status file number.
** message	L	String for messages.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetStatusFILE ( FILE *stFILE, int filenum )
{	char	message[256], routine[] = "SetStatusFILE";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
		"Request to set status FILE %d denied.  %d is maximum file #",
		filenum, (MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	ESPstatus[filenum].fp = stFILE;
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetStatusFILE.c,v $";
 static char rcs_id2[] = "$Id: SetStatusFILE.c,v 1.2 2000/05/18 13:08:34 dws Exp $";}
/*  ===================================================  */

}
