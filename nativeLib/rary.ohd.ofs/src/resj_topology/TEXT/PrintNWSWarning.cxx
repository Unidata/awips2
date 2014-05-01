/* ----------------------------------------------------------------------------
** PrintNWSWarning - print warning message, including a call to NWS WARN.
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 08 May 2001	James R. VanShaar, RTi	Initial version
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"
#include "ResJSys.h" 
/*** #include "resj/Extern.h" ***/

/*****
// wl is warning level (generally will come in as 0)
// routine is a string naming the source of the error by routine
// message is the error message to be output, will have been prepared
//	by PrintError.
*****/

int PrintNWSWarning ( int wl, char *routine, char* message )
{	char function[] = "PrintNWSWarning";

	int unitNum;
	unitNum = ResJSys::getIPR();

	int length = strlen(message);

	// pass the warning along for output
	ResJ_ccwrite( &length, message, &unitNum );

	// call NWS 'WARN' 
	WARN();

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/PrintNWSWarning.cxx,v $";
 static char rcs_id2[] = "$Id: PrintNWSWarning.cxx,v 1.3 2006/10/26 15:29:13 hsu Exp $";}
/*  ===================================================  */

}
