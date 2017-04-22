//------------------------------------------------------------------------------
// TS::writePersistentHeader
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is a virtual function at this level and should be 
//			redefined in the derived class.
//------------------------------------------------------------------------------
// History:
// 
// 26 May 1997	Steven A. Malers, RTi	Created function
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: writePersistentHeader( ostream& in )
{
	char	routine[]="TS::writePersistentHeader(ostream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined in the derived class" );

	return( STATUS_SUCCESS );
}

int TS :: writePersistentHeader( char *fname )
{
	char	routine[]="TS::writePersistentHeader(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined in the derived class" );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_writePersistentHeader.cxx,v $";
 static char rcs_id2[] = "$Id: TS_writePersistentHeader.cxx,v 1.2 2000/05/19 13:39:53 dws Exp $";}
/*  ===================================================  */

}
