//------------------------------------------------------------------------------
// TSIdent::getIdentifier - get the full identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The full identifier is based on _behavior_mask and is
//			set when identifier components are set.
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

char * TSIdent :: getIdentifier( void )
{
	return( _identifier );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_getIdentifier.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_getIdentifier.cxx,v 1.2 2000/05/19 13:06:33 dws Exp $";}
/*  ===================================================  */

}
