//------------------------------------------------------------------------------
// TS::freeDataSpace - free memory used by the data array
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 06 Feb 1997	Steven A. Malers, RTi	Created member function.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: freeDataSpace ( void )
{	char	routine[] = "TS::freeDataSpace";

	PrintWarning( 1, routine, 
	"This is a virtual function, redefine in lower classes" );

	return 1;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: TS_freeDataSpace.cxx,v 1.2 2000/05/19 13:35:21 dws Exp $";}
/*  ===================================================  */

}
