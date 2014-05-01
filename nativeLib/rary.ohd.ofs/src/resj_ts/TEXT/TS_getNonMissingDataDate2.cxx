//------------------------------------------------------------------------------
// TS.getNonMissingDataDate2 -	get the date corresponding to the last
//				non-missing data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 27 May 1997	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"

TSDate TS :: getNonMissingDataDate2( void )
{
	return( _data_limits.getNonMissingDataDate2() );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getNonMissingDataDate2.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getNonMissingDataDate2.cxx,v 1.3 2000/05/19 13:37:38 dws Exp $";}
/*  ===================================================  */

}
