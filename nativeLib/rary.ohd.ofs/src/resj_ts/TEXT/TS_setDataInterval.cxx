//------------------------------------------------------------------------------
// TS.setDataInterval - set the data interval
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

int TS :: setDataInterval( int base, int mult )
{
	_data_interval_base = base;

	_data_interval_mult = mult;

	return( 0 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setDataInterval.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setDataInterval.cxx,v 1.2 2000/05/19 13:38:44 dws Exp $";}
/*  ===================================================  */

}
