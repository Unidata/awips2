/* ----------------------------------------------------------------------------
** SetProgramCommandFile - set the program command file
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine copies the program command file into
**			global variables so that they can be used in routines
**			outside of the main program (e.g.,
**			PrintCreatorHeader).
** ----------------------------------------------------------------------------
** History:
**
** 26 Sep 1995	Steven A. Malers, RTi	Create routine.
** 06 Sep 1996	SAM, RTi		Split out of HMUtil.c file.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** commandfile	G	Program command file.
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"
#include "resj/Extern.h"

int SetProgramCommandFile ( char *commandfile )
{	char	routine[] = "SetProgramCommandFile";

	if ( commandfile ) {
		strcpy ( ESPcommandfile, commandfile );
	}
	
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/SetProgramCommandFile.c,v $";
 static char rcs_id2[] = "$Id: SetProgramCommandFile.c,v 1.2 2000/05/18 13:08:34 dws Exp $";}
/*  ===================================================  */

}
