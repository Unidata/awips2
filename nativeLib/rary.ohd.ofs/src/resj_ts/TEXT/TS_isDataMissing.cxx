//------------------------------------------------------------------------------
// TS::isDataMissing - determines if a data value is missing
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.  created funct
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: isDataMissing( double value )
{
	char	routine[]="TS::isDataMissing";

	if( 	value >= _missingl &&
		value <= _missingu ) {

		return( 1 );
	}

	return( 0 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_isDataMissing.cxx,v $";
 static char rcs_id2[] = "$Id: TS_isDataMissing.cxx,v 1.2 2000/05/19 13:38:43 dws Exp $";}
/*  ===================================================  */

}
