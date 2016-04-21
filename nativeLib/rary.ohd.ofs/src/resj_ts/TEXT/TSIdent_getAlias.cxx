//------------------------------------------------------------------------------
// TSIdent::getAlias - get the time series alias
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The time series alias is returned.
//------------------------------------------------------------------------------
// History:
// 
// 07 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent :: getAlias ( void )
{
	return( _alias );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getAlias.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getAlias.cxx,v 1.2 2000/05/19 13:06:32 dws Exp $";}
/*  ===================================================  */

}
