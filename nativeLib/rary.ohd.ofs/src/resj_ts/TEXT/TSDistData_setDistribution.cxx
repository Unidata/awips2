//-----------------------------------------------------------------------------
// TSDistData :: setDistribuition - sets the _distrib array 
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

int TSDistData :: setDistribution ( float* dist, int n_dist ) 
{	
	char	routine[] = "TSDistData::setDistribution";
	int i;

	// Set the private data members...
	_n_dist = n_dist;
	if( _distrib ) {
		delete [] _distrib;
	}
	_distrib = new float [ n_dist ];

	if( dist == NULL ) {
		PrintDebug( 10, routine, "Distribution is not set, using "
			"default uniform distribution for %d values.", n_dist );
		for( i = 0; i < _n_dist; i++ ) {
			_distrib[i] =  1.0 / (float)n_dist;
		}
	}
	else {
		for( i = 0; i < _n_dist; i++ ) {
			_distrib[i] = dist[i];
		}
	}
	
	return (STATUS_SUCCESS);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSDistData_setDistribution.cxx,v $";
 static char rcs_id2[] = "$Id: TSDistData_setDistribution.cxx,v 1.1 1999/02/18 15:19:18 dws Exp $";}
/*  ===================================================  */

}
