//------------------------------------------------------------------------------
// CalcInflow::freeDataSpace - delete dynamically allocated memory.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Feb 2004	James R. VanShaar, RTi	Created initial version.
// 19 Apr 2004	JRV, RTi	Adjusted for proper handling of _inflow_calc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "CalcInflow.h"

int CalcInflow::freeDataSpace()
{
	_maxIncrease.freeDataSpace();
	_maxDecrease.freeDataSpace();


	if( _isCopy ) {
		if( _release_obs != NULL ) {
			delete _release_obs;
		}
		if( _withdraw_obs != NULL ) {
			delete _withdraw_obs;
		}
		if( _pool_obs != NULL ) {
			delete _pool_obs;
		}
	}

	// delete _pool_obs;
	if( _inflow_calc != NULL ) {
		delete _inflow_calc;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_freeDataSpace.cxx,v 1.2 2006/10/26 15:11:45 hsu Exp $";}
/*  ===================================================  */

}
