// ----------------------------------------------------------------------------
// TSUtil.changeInterval - change time series from one interval to another
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	This is a placeholder.  Need to combine the ESPADP
//			and Java TSUtil code here.
// ----------------------------------------------------------------------------
// History:
// 
// 05 Jan 1998	Steven A. Malers, RTi	Initial version in C++.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// routine	L	name of routine.
//
// ----------------------------------------------------------------------------

#include "TSUtil.h"
#include "resj/TSDate.h"
#include "resj/TS.h"

TS TSUtil :: changeInterval (	TS *ts0, int interval_base, int interval_mult,
				unsigned int flags ) 
{	char routine[] = "TSUtil.changeInterval";

	// Put these in so we don't get irritating compiler messages...

	int base = interval_base;
	int mult = interval_mult;
	int f = flags;
	TS * ts = ts0;

	PrintWarning ( 1, routine, "NOT IMPLEMENTED" );
	return (TS)NULL;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSUtil_changeInterval.cxx,v $";
 static char rcs_id2[] = "$Id: TSUtil_changeInterval.cxx,v 1.3 2000/05/19 13:35:20 dws Exp $";}
/*  ===================================================  */

}
