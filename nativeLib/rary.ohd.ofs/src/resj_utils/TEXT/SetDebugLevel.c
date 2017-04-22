/* ----------------------------------------------------------------------------
** SetDebugLevel - set debug level for ESP routines
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-07-95	Steven A. Malers, RTi	Add file number argument.
** 03 Sep 96	SAM, RTi		Split code out of HMMsg.c and make more
**					stand-alone.
** 18 Sep 96	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** dl		I	Debug level (>= 0).
** filenum	I	Debug file number.
** HMdebug	G	Global debug data.
** message	L	String for messages.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetDebugLevel ( int dl, int filenum )
{	char	message[256], routine[] = "SetDebugLevel";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
		"Unable to set debug FILE %d to level %d.  %d is max file #",
		filenum, dl, (MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	ESPdebug[filenum].level = dl;
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetDebugLevel.c,v $";
 static char rcs_id2[] = "$Id: SetDebugLevel.c,v 1.2 2000/05/18 13:08:33 dws Exp $";}
/*  ===================================================  */

}
