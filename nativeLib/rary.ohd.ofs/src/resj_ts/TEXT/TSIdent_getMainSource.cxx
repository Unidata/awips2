//------------------------------------------------------------------------------
// TSIdent::getMainSource - get the main source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the main source.
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

char * TSIdent :: getMainSource( void )
{
	return( _main_source );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getMainSource.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getMainSource.cxx,v 1.2 2000/05/19 13:06:35 dws Exp $";}
/*  ===================================================  */

}
