//------------------------------------------------------------------------------
// TS.getIdentifierString - get the identifier as a string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char * TS :: getIdentifierString()
{
	return( _id.getIdentifier() );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getIdentifierString.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getIdentifierString.cxx,v 1.2 2000/05/19 13:37:37 dws Exp $";}
/*  ===================================================  */

}
