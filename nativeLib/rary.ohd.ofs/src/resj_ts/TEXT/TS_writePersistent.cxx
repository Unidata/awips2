//------------------------------------------------------------------------------
// writePersistent - 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is used to write a time series to a DATACARD
//			format file.
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::writePersistent ( ostream& out )
{	
	char	routine[]="TS::writePersistent(ostream&)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in lower classes" );

	return( STATUS_SUCCESS );
}

int TS::writePersistent ( char *fname )
{	
	char	routine[]="TS::writePersistent(char*)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in lower classes" );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_writePersistent.cxx,v $";
 static char rcs_id2[] = "$Id: TS_writePersistent.cxx,v 1.2 2000/05/19 13:39:52 dws Exp $";}
/*  ===================================================  */

}
