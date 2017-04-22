//------------------------------------------------------------------------------
// TSIdent::setBehaviorMask - set the behavior mask for the identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The behavior mask controls how identifier sub-parts
//			are joined into the full identifier.   Currently this
//			routine does a full reset (not bit-wise).
//------------------------------------------------------------------------------
// History:
// 
// 18 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// behavior_mask I	Behavior mask for identifier.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setBehaviorMask( unsigned int behavior_mask )
{	char	routine[] = "TSIdent::setBehaviorMask";

	_behavior_mask = behavior_mask;
	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setBehaviorMask.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setBehaviorMask.cxx,v 1.2 2000/05/19 13:08:20 dws Exp $";}
/*  ===================================================  */

}
