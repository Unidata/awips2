//------------------------------------------------------------------------------
// TS.getDataLimits - return the limits of the TS data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 09 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSLimits TS :: getDataLimits( void )
{
	// Make sure that the limits have been set...
	refresh();
	return( _data_limits );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getDataLimits.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getDataLimits.cxx,v 1.2 2000/05/19 13:36:20 dws Exp $";}
/*  ===================================================  */

}
