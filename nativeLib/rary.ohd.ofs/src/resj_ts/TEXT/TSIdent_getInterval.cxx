//------------------------------------------------------------------------------
// TSIdent::getInterval - get the full interval string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the full interval, which is set
//			from its parts.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Split code out of SetGet.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent :: getInterval ( void )
{
	return( _interval_string );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getInterval.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getInterval.cxx,v 1.2 2000/05/19 13:06:33 dws Exp $";}
/*  ===================================================  */

}
