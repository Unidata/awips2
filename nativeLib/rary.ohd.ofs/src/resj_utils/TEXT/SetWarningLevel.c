/* ----------------------------------------------------------------------------
** SetWarningLevel - set warning level for ESP routines
** ----------------------------------------------------------------------------
** Copyright:	Set the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-07-95	Steven A. Malers, RTi	Add file number argument.
** 03 Sep 96	SAM, RTi		Split out of HMMsg.c and make more
**					stand-alone.
** 18 Sep 96	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** sl		I	Warning level (>= 0).
** filenum	I	Warning file number.
** HMwarning	G	Global warning data.
** message	L	String for messages.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetWarningLevel ( int wl, int filenum )
{	char	message[256], routine[] = "SetWarningLevel";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
		"Unable to set warning FILE %d to level %d.  %d is max file #",
		filenum, wl, (MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	ESPwarning[filenum].level = wl;
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetWarningLevel.c,v $";
 static char rcs_id2[] = "$Id: SetWarningLevel.c,v 1.2 2000/05/18 13:08:35 dws Exp $";}
/*  ===================================================  */

}
