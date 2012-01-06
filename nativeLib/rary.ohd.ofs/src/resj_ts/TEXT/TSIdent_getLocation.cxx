//------------------------------------------------------------------------------
// TSIdent::getLocation - get the full location string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The full location is returned and is based on the
//			current value controlled by _behavior_mask.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Split out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent :: getLocation( void )
{
	return( _full_location );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getLocation.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getLocation.cxx,v 1.2 2000/05/19 13:06:34 dws Exp $";}
/*  ===================================================  */

}
