//------------------------------------------------------------------------------
// TS::readPersistent
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be 
//		redefined.
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford, RTi	Created function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: readPersistent( istream& in )
{
	char	routine[]="TS::readPersistent(istream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}

int TS :: readPersistent( char *file_name )
{
	char	routine[]="TS::readPersistent(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_readPersistent.cxx,v $";
 static char rcs_id2[] = "$Id: TS_readPersistent.cxx,v 1.2 2000/05/19 13:38:43 dws Exp $";}
/*  ===================================================  */

}
