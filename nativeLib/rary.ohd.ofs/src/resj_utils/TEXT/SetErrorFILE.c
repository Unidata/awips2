/* ----------------------------------------------------------------------------
** SetErrorFILE - set file pointer for error messages
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06-06-95	Steven A. Malers, RTi	Add flag for file pointer #.
** 05 Sep 96	SAM, RTi		Break code out of HMMsg.c and make more
**					stand-alone.
** 24 Sep 96	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** erFILE	I	Pointer to open error FILE.
** filenum	I	Error file number.
** message	L	String for messages.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int SetErrorFILE ( FILE *erFILE, int filenum )
{	char	message[256], routine[] = "SetErrorFILE";

	InitFILEs ();
	if ( (filenum < 0) || (filenum >= MAX_MESSAGE_FILE) ) {
		sprintf ( message,
		"Request to set error FILE %d denied.  %d is maximum file #",
		filenum, (MAX_MESSAGE_FILE - 1) );
		PrintWarning ( 1, routine, message );
		return STATUS_FAILURE;
	}
	ESPerror[filenum].fp = erFILE;
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetErrorFILE.c,v $";
 static char rcs_id2[] = "$Id: SetErrorFILE.c,v 1.2 2000/05/18 13:08:34 dws Exp $";}
/*  ===================================================  */

}
