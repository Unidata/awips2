//-----------------------------------------------------------------------------
// TSUtil.getValidPeriod -	get a valid period to work on given two
//				suggested dates
//-----------------------------------------------------------------------------
// Notes:	(1)	This routine takes two dates which are generally the
//			start and end dates for an iteration.  If they are
//			specified, they are used (even if they are outside the
//			range of the time series).  If a date is not specified,
//			then the appropriate date from the time series is
//			used.  This routine may require logic at some point to
//			handle special cases.  For example, the incoming
//			arguments may specify a start date but no end date.
//			If the start date from the time series is later than
//			the specified end date, then what?
//-----------------------------------------------------------------------------
// History:
//
// ?		Daniel K. Weiler, RTi	Initial version.
// 09 Jan 1998	Steven A. Malers, RTi	Enhance to return TSLimits.  Port to
//					C++.
//-----------------------------------------------------------------------------

#include "TSUtil.h"
#include "TSLimits.h"

TSLimits TSUtil :: getValidPeriod (	TS *ts, TSDate suggested_start,
					TSDate suggested_end )
{	TSLimits dates;

	if ( suggested_start.getYear() == 0 ) {
		dates.setDate1( ts->getDate1() );	
	}
	else {	dates.setDate1 ( suggested_start );
	}

	if ( suggested_end.getYear() == 0 ) {
		dates.setDate2( ts->getDate2() );
	}
	else {	dates.setDate2 ( suggested_end );
	}

	return dates;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSUtil_getValidPeriod.cxx,v $";
 static char rcs_id2[] = "$Id: TSUtil_getValidPeriod.cxx,v 1.1 1999/02/18 15:20:27 dws Exp $";}
/*  ===================================================  */

}
