//------------------------------------------------------------------------------
// TSInterval::getBaseString
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the interval base as a string.
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers, RTi	Port to C++ from Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "TSInterval.h"

char * TSInterval :: getBaseString( void )
{
	return _interval_base_string;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_getBaseString.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_getBaseString.cxx,v 1.1 1999/02/18 15:19:47 dws Exp $";}
/*  ===================================================  */

}
