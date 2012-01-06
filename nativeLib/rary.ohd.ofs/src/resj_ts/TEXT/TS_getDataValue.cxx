//------------------------------------------------------------------------------
// TS::getDataValue
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford, RTi	created member function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

double TS :: getDataValue( TSDate& date )
{
	char	routine[]="TS::getDataValue";

	PrintWarning( 1, routine, 
	"This is a virtual function, redefine in lower classes" );

	return( -999.0 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getDataValue.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getDataValue.cxx,v 1.2 2000/05/19 13:36:21 dws Exp $";}
/*  ===================================================  */

}
