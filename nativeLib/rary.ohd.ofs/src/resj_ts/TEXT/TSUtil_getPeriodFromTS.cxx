//-----------------------------------------------------------------------------
// TSUtil.getPeriodFromTS -	finds the maximum or minimum POR referenced by
//				all entries in a list of time series TS
//-----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//-----------------------------------------------------------------------------
// Notes:	(1)	For example:
//
//			------------               TS1
//			      -------------------- TS2
//			         ------            TS3
//
//			-------------------------- max
//			         ---               min
//		(2)	Currently, input is an array of time series pointers.
//			It is likely that this will be overloaded to provide
//			more flexibility.
//-----------------------------------------------------------------------------
// History:
//
// ?		Catherine E. Nutting, RTi	Created routine.
// 01/11/96	Peter T. Abplanalp, RTi		Updated to use TSSetTSPt.
//						Changed to include TSLib.h
//						instead of TSDefs.h.
// 01/26/96	CEN, RTi			Added flag: MaxMin (TSPOR_MAX,
//						or TSPOR_MIN ).
// 03 Oct 96	Steven A. Malers, RTi		Clean up code while debugging.
// 08 Jan 1998	SAM, RTi			Copy TSGetPeriodFromTS and put
//						into this TSUtil class.
//-----------------------------------------------------------------------------
// Variable	I/O	Description
//
// am1, am2	L	Absolute months for start and end of window.
// nstats	I	Number of time series entries in TS[].
// m1, y1	O	Months/years which mark earliest absolute months.
// m2, y2	O	Months/years which mark latest absolute months.
// ts		I	Time series list.
//-----------------------------------------------------------------------------

#include <stdio.h>

#include "TSUtil.h"
#include "ResJ.h"

TSLimits TSUtil :: getPeriodFromTS ( int nstats, TS *ts[], int flag )
{	char		rtn[] = "TSUtil.getPeriodFromTS";
	TS		*tspt;
	int		i = 0;
	TSDate		end(TSDate::DATE_STRICT),
			tsptr_end(TSDate::DATE_STRICT),
			tsptr_start(TSDate::DATE_STRICT),
			start(TSDate::DATE_STRICT);
	TSLimits	limits;
		
	PrintDebug ( 10, rtn, "Checking %d TS to find POR limits", nstats );

	// Check for NULL list...

	if ( nstats == 0 ) {
		PrintWarning ( 2, rtn, "Zero time series list" );
		return limits;
	}

	// Set first guess to the limits of the first time series...

	if ( !ts[0] ) {
		PrintWarning ( 2, rtn, "Null first time series" );
		return limits;
	}

	tspt = ts[0];
	start	= tspt->getDate1();
	end	= tspt->getDate2();

	// Now loop through the remaining time series.  Start with i=1 because
	// TS is used to set initial am1, am2 values...

	for (i=1; i<nstats; i++) {
		tspt = ts[i];
		if ( tspt == (TS *)NULL ) {
			PrintWarning ( 2, rtn,
			"Null TS for [%d] time series", i );
			continue;
		}

		tsptr_start = tspt->getDate1();
		tsptr_start = tspt->getDate2();

		if ( flag == MAX_POR ) {
			if ( tsptr_start < start )
				start = tsptr_start;
			if ( tsptr_end > end ) 
				end = tsptr_end;
		}
		else if ( flag == MIN_POR ) {
			if ( tsptr_start > start )
				start = tsptr_start;
			if ( tsptr_end < end ) 
				end = tsptr_end;
		}
		else {	PrintWarning ( 2, rtn, "Unknown flag option %u.",
			flag );
			return limits;
		}
	}

	if ( flag == MIN_POR ) {
		PrintDebug ( 10, rtn,
		"Found minimum POR limits to be %s to %s",
		(char*)start, (char*)end );
	}
	else if ( flag == MAX_POR ) {
		PrintDebug ( 10, rtn,
		"Found maximum POR limits to be %s to %s",
		(char*)start, (char*)end );
	}

	limits.setDate1 ( start );
	limits.setDate2 ( end );
	return limits;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSUtil_getPeriodFromTS.cxx,v $";
 static char rcs_id2[] = "$Id: TSUtil_getPeriodFromTS.cxx,v 1.1 1999/02/18 15:20:26 dws Exp $";}
/*  ===================================================  */

}
