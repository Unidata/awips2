//------------------------------------------------------------------------------
// TS.setInstantaneous - sets the _is_instantaneous to INSTANTANEOUS 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 16 Apr 1998	Daniel Weiler,			Created intial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setInstantaneous()
{
	_is_instantaneous = INSTANTANEOUS;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setInstantaneous.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setInstantaneous.cxx,v 1.2 2000/05/19 13:39:51 dws Exp $";}
/*  ===================================================  */

}
