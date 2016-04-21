//------------------------------------------------------------------------------
// TSIdent::getBehaviorMask - get the behavior mask
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The full location is returned and is based on the
//			current value controlled by _behavior_mask.
//------------------------------------------------------------------------------
// History:
// 
// 22 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

unsigned int TSIdent :: getBehaviorMask( void )
{
	return( _behavior_mask );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getBehaviorMask.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getBehaviorMask.cxx,v 1.2 2000/05/19 13:06:33 dws Exp $";}
/*  ===================================================  */

}
