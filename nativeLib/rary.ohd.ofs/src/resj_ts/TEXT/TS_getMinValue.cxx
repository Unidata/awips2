//------------------------------------------------------------------------------
// TS.getMinValue
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

double TS :: getMinValue( void )
{
	return( _data_limits.getMinValue() );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getMinValue.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getMinValue.cxx,v 1.2 2000/05/19 13:37:37 dws Exp $";}
/*  ===================================================  */

}
