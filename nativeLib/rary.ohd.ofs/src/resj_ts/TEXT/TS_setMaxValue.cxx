//------------------------------------------------------------------------------
// TS.setMaxValue
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

int TS :: setMaxValue( double max )
{
	_data_limits.setMaxValue ( max );

	return( 0 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setMaxValue.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setMaxValue.cxx,v 1.2 2000/05/19 13:39:51 dws Exp $";}
/*  ===================================================  */

}
