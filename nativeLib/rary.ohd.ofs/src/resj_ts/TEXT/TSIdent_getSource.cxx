//------------------------------------------------------------------------------
// TSIdent::getSource - get the full source string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the full source string as
//			controlled by the _behavior_mask flag.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent :: getSource( void )
{
	return( _full_source );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getSource.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getSource.cxx,v 1.2 2000/05/19 13:06:35 dws Exp $";}
/*  ===================================================  */

}
