//------------------------------------------------------------------------------
// TS::allocateDataSpace
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 05 Feb 1997	Steven A. Malers, RTi	Created member function.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: allocateDataSpace ( void )
{	char	routine[] = "TS::allocateDataSpace";

	PrintWarning( 1, routine, 
	"This is a virtual function, redefine in lower classes" );

	return 1;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_allocateDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: TS_allocateDataSpace.cxx,v 1.2 2000/05/19 13:35:21 dws Exp $";}
/*  ===================================================  */

}
