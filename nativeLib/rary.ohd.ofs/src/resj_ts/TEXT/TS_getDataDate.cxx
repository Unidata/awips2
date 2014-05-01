//------------------------------------------------------------------------------
// TS::getDataDate - get the date relative to a starting date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function is meant to be implemented by regular and
//			irregular interval time series.  Because most time
//			series are regular interval, we implement here and let
//			the IrregularTS code overrule this.
//------------------------------------------------------------------------------
// History:
// 
// 30 Sep 1997	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSDate TS :: getDataDate ( TSDate &date0, int increment )
{	char	routine[] = "TS::getDataDate";
	TSDate	t;

	// For now, allow the date to go past the end of the time series...

	t = date0;
	t.addInterval ( _data_interval_base,
			increment*_data_interval_mult );

	return t;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getDataDate.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getDataDate.cxx,v 1.2 2000/05/19 13:35:21 dws Exp $";}
/*  ===================================================  */

}
