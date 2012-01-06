//------------------------------------------------------------------------------
// TSIdent::getType - get the data type
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	Return the data type.
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

char * TSIdent :: getType( void )
{
	return( _type );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getType.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getType.cxx,v 1.2 2000/05/19 13:06:36 dws Exp $";}
/*  ===================================================  */

}
