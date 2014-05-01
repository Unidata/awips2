//------------------------------------------------------------------------------
// TSIdent::getSubSource - get the sub-source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the sub-source, which will be
//			an empty string if _behavior_mask has
//			TSIDENT_NO_SUB_SOURCE set.
//------------------------------------------------------------------------------
// History:
// 
// 16 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent :: getSubSource( void )
{
	return( _sub_source );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getSubSource.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getSubSource.cxx,v 1.2 2000/05/19 13:06:36 dws Exp $";}
/*  ===================================================  */

}
