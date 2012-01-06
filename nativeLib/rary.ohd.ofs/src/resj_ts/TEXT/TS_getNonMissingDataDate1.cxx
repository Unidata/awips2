//------------------------------------------------------------------------------
// TS.getNonMissingDataDate1 -	get the date corresponding to the first
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
// 09 Jan 1998	SAM, RTi		Move this code from TS.getDataDate1.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"

TSDate TS :: getNonMissingDataDate1( void )
{
	return( _data_limits.getNonMissingDataDate1() );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getNonMissingDataDate1.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getNonMissingDataDate1.cxx,v 1.3 2000/05/19 13:37:38 dws Exp $";}
/*  ===================================================  */

}
