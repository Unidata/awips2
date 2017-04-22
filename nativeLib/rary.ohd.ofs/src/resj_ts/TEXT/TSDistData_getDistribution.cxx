//-----------------------------------------------------------------------------
// TSDistData :: getDistribution - get distribution values 
//-----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// History:
// 
// 05 Mar 1998	Daniel Weiler,	Created initial version.
//		Riverside Technology, inc.
//-----------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// routine	L	name of routine.
//-----------------------------------------------------------------------------

#include "TSDistData.h"

float TSDistData :: getDistribution ( int index ) 
{	
	char	routine[] = "TSDistData::getDistribution";

	// Lookup the _dsitrib[index] value	
	return( _distrib[index] );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSDistData_getDistribution.cxx,v $";
 static char rcs_id2[] = "$Id: TSDistData_getDistribution.cxx,v 1.1 1999/02/18 15:19:16 dws Exp $";}
/*  ===================================================  */

}
