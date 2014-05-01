//------------------------------------------------------------------------------
// TS::setNonMissingDataDate2 -	set the date corresponding to the last
//				non-missing data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 27 May 1996	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
// 09 Jan 1998	SAM, RTi		Update to use _data_limits.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"

int TS :: setNonMissingDataDate2( TSDate t )
{
	_data_limits.setNonMissingDataDate2 ( t );

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setNonMissingDataDate2.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setNonMissingDataDate2.cxx,v 1.3 2000/05/19 13:39:52 dws Exp $";}
/*  ===================================================  */

}
