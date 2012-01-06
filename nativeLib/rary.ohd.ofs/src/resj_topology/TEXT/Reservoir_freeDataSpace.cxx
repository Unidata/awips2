//------------------------------------------------------------------------------
// Reservoir :: freeDataSpace - deletes the whole tree.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reservoir.h"

int Reservoir :: freeDataSpace()
{
	char	routine[]="Reservoir :: freeDataSpace";

	if( _pool_ts ) {
		delete _pool_ts;
	}
	if( _release_ts ) {
		delete _release_ts;
	}
	if( _spill_ts ) {
		delete _spill_ts;
	}
	if( _withdraw_ts ) {
		delete _withdraw_ts;
	}
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reservoir_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_freeDataSpace.cxx,v 1.4 2006/10/26 15:32:28 hsu Exp $";}
/*  ===================================================  */

}
