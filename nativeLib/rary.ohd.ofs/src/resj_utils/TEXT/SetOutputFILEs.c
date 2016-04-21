/* ----------------------------------------------------------------------------
** SetOutputFILEs - set file pointer for all messages
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine makes calls to the SetDebugFILE,
**			SetErrorFILE, SetStatusFILE, and SetWarningFILE
**			routines.  It can be used in place of individual calls
**			if output is going to common destinations.
** ----------------------------------------------------------------------------
** History:
**
** 06-07-95	Steven A. Malers, RTi	Add routine.
** 05 Sep 96	SAM, RTi		Break code out of HMMsg.c and make more
**					stand-alone.
** 24 Sep 96	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** filenum	I	Output file number.
** message	L	String for messages.
** oFILE	I	Pointer to open FILE.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetOutputFILEs ( FILE *oFILE, int filenum )
{	char	message[256], routine[] = "SetOutputFILEs";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
		"Request to set output FILE %d denied.  %d is maximum file #",
		(MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	SetDebugFILE ( oFILE, filenum );
	SetErrorFILE ( oFILE, filenum );
	SetStatusFILE ( oFILE, filenum );
	SetWarningFILE ( oFILE, filenum );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetOutputFILEs.c,v $";
 static char rcs_id2[] = "$Id: SetOutputFILEs.c,v 1.2 2000/05/18 13:08:34 dws Exp $";}
/*  ===================================================  */

}
