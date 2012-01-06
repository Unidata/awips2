/* ----------------------------------------------------------------------------
** GetProgramData - get the program name and program version
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine requires that the HMSetProgramData
**			routine has been called to set the informtion.
** ----------------------------------------------------------------------------
** History:
**
** 05-19-95	Steven A. Malers, RTi	Created function.
** 06 Sep 96	SAM, RTi		Break out of the HMUtil.c file.
** 27 Sep 96	Catherine E. Nutting, RTi	Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** progname	O	Program name.
** progver	O	Program version.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int GetProgramData ( char *progname, char *progver )
{	if ( program_initialized ) {
		strcpy ( progname, ESPprogname );
		strcpy ( progver, ESPprogver );
	}
	else {	*progname	= '\0';
		*progver	= '\0';
	}

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetProgramData.c,v $";
 static char rcs_id2[] = "$Id: GetProgramData.c,v 1.2 2000/05/18 13:08:32 dws Exp $";}
/*  ===================================================  */

}
