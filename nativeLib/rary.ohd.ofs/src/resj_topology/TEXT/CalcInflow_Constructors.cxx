//------------------------------------------------------------------------------
// CalcInflow::CalcInflow - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 12 Feb 2004	James R. VanShaar, RTi	Created initial version.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "CalcInflow.h"

CalcInflow::CalcInflow( Reservoir* owner ) : ReservoirMethod( owner )
{
	initialize();
}

CalcInflow::CalcInflow( const CalcInflow& meth, Reservoir* owner ) : 
	ReservoirMethod( meth, owner )
{
	char	routine[]="CalcInflow::CalcInflow";

	initialize();

	// Copy integers first...
	_outputCalcTS = meth._outputCalcTS;
	_useForSim = meth._useForSim;
	_constrainChange = meth._constrainChange;
	_useLoss = meth._useLoss;

	// Copy doubles...
	_minInflow = meth._minInflow;
	_remainingVol = meth._remainingVol;
	_startPool = meth._startPool;
	_startRelease = meth._startRelease;
	_startWithdrawal = meth._startWithdrawal;
	_startInflow = meth._startInflow;

	// Copy tables...
	if( _constrainChange ) {
		_maxIncrease = meth._maxIncrease;
		_maxDecrease = meth._maxDecrease;
	}

	// time series
	// These are pointers.  We create a new HourTS, populate it by copying
	// the information from the timeseries on meth, then point to it with
	// the timeseries pointer.
	if( meth._release_obs != NULL ) {
		delete _release_obs;
		_release_obs = new HourTS( *(meth._release_obs) );
	}
	if( meth._withdraw_obs != NULL ) {
		delete _withdraw_obs;
		_withdraw_obs = new HourTS( *(meth._withdraw_obs) );
	}
	if( meth._pool_obs != NULL ) {
		delete _pool_obs;
		_pool_obs = new HourTS( *(meth._pool_obs) );
	}
	if( meth._inflow_calc != NULL ) {
		delete _inflow_calc;
		_inflow_calc = new HourTS( *(meth._inflow_calc) );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_Constructors.cxx,v 1.2 2006/10/26 15:11:27 hsu Exp $";}
/*  ===================================================  */

}
