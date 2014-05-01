//------------------------------------------------------------------------------
// TS.setLocation - set the location part of TSIdent
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 09 Jan 1998	Steven A. Malers,	Port from Java.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setLocation ( char *location )
{	char	routine[]="TS_setLocation";

	return _id.setLocation ( location );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setLocation.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setLocation.cxx,v 1.2 2000/05/19 13:39:51 dws Exp $";}
/*  ===================================================  */

}
