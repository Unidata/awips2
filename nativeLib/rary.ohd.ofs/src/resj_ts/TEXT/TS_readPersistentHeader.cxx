//------------------------------------------------------------------------------
// TS::readPersistentHeader
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be 
//		redefined.
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Nov 1996	Matthew J. Rutherford, RTi	Created function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: readPersistentHeader( istream& in )
{
	char	routine[]="TS::readPersistentHeader(istream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}

int TS :: readPersistentHeader( char *fname )
{
	char	routine[]="TS::readPersistentHeader(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_readPersistentHeader.cxx,v $";
 static char rcs_id2[] = "$Id: TS_readPersistentHeader.cxx,v 1.2 2000/05/19 13:38:44 dws Exp $";}
/*  ===================================================  */

}
