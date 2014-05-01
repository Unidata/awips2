/* ----------------------------------------------------------------------------
** SetStatusLevel - set status level for ESP routines
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-07-95	Steven A. Malers, RTi	Add file number argument.
** 05 Sep 96	SAM, RTi		Break out of HMMsg.c and make more
**					stand-alone.
** 18 Sep 96	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** sl		I	Status level (>= 0).
** filenum	I	Status file number.
** status	G	Global status data.
** message	L	String for messages.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetStatusLevel ( int sl, int filenum )
{	char	message[256], routine[] = "SetStatusLevel";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
		"Unable to set status FILE %d to level %d.  %d is max file #",
		filenum, sl, (MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	ESPstatus[filenum].level = sl;
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetStatusLevel.c,v $";
 static char rcs_id2[] = "$Id: SetStatusLevel.c,v 1.2 2000/05/18 13:08:34 dws Exp $";}
/*  ===================================================  */

}
