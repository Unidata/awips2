//------------------------------------------------------------------------------
// TSInterval.getBase
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the interval base as an int.
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers, RTi	Port to C++ from Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "TSInterval.h"

int TSInterval :: getBase( void )
{
	return _interval_base;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_getBase.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_getBase.cxx,v 1.1 1999/02/18 15:19:46 dws Exp $";}
/*  ===================================================  */

}
