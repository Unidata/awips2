// ----------------------------------------------------------------------------
// TSInterval.init - initialize the data
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSInterval.h"

int TSInterval :: init ()
{
	_interval_base = 0;
	_interval_base_string = (char *)NULL;
	setBaseString ( "" );
	_interval_mult = 0;
	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_init.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_init.cxx,v 1.1 1999/02/18 15:19:49 dws Exp $";}
/*  ===================================================  */

}
