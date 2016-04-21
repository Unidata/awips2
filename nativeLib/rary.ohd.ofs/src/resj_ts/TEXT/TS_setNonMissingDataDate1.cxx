//------------------------------------------------------------------------------
// TS::setNonMissingDataDate1 -	set the date corresponding to the first
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
//		Riverside Technology,	Change from setDataDate1.
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"

int TS :: setNonMissingDataDate1( TSDate t )
{
	_data_limits.setNonMissingDataDate1 ( t );

	return( 0 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setNonMissingDataDate1.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setNonMissingDataDate1.cxx,v 1.3 2000/05/19 13:39:52 dws Exp $";}
/*  ===================================================  */

}
