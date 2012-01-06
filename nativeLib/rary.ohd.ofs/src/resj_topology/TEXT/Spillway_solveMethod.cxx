//------------------------------------------------------------------------------
// Spillway :: solveMethod - Algorithm that solves the Spillway method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Dec 2002	James R. VanShaar , Riverside Technology, inc
//					Created initial version.
// 14 Jan 2003	JRV, RTi	Introduced use of _myValue in revising the 
// 				startRelease from the Reservoir to eliminate the
// 				previous spill.
// 12 Jul 2003	JRV, RTi	Added handling of _owner->_release if it is
// 				MISSING for bug associated with Spillway as the
// 				only method.
// 21 Jul 2003	KSH, OHD	Use dummy variable and flow in pseudo-implicit
//                              integration
// 19 Feb 2004        JRV, RTi        Replaced calls to sumInflow with calls to 
//                            getTotalInflow.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Spillway.h"
#include "resj/Table.h"

int Spillway :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[]="Spillway :: solveMethod";
	int 	i;
	double	startInflow, startRelease, startWithdraw, startStorage, spill,
		endInflow, endRelease, endWithdraw, endStorage, endPool, storage,
		timestepSec, deltaInflowV, deltaReleaseV, deltaWithdrawV,
		timestepSub, deltaInflow, deltaRelease, deltaWithdraw, ddq;

	// Identify the spill required at the end of the last timestep by this 
	// method.  It is contained in _myValue, unless the method has been
	// inactive.
	if( !_Active ) {
		// This method prescribed a spill of nothing
		_myValue = 0;
	}

	// Set _Active to 1
	_Active = 1;

	startRelease = _owner->_startRelease;
	if( startRelease == MISSING ) {
		startRelease = 0;
	}
	else {
		startRelease = startRelease - _myValue;
	}
	endRelease = _owner->_release;
	if( endRelease == MISSING ) {
	// This will happen, probably, only if Spillway is the only
	// method.
	// In which case, we are going to set _owner->_release to zero
	// to ensure Reservoir::finalizeSolution can solve the water 
	// balance, if necessary.  Any issues with _owner->_min_release
	// will also be resolved there.
		endRelease = 0;
		_owner->_release = 0;
	}
	// Ensure that endRelease is consistent with the owner's minRelease.
	// If not, we'll increment it to minRelease before continuing.
	if( endRelease < _owner->_min_release ) {
		endRelease = _owner->_min_release;
	}

	// Prepare previous values, if not immediately available, for reference
	//	and expected water balance calculation below
	startInflow = _owner->_startInflow;
	startWithdraw = _owner->_startWithdraw;
	if( startWithdraw == MISSING ) {
		startWithdraw = 0;
	}
	startStorage = _owner->_storage;

	// prepare current water balance terms
	endInflow = _owner->getTotalInflow( cur_date );
	endWithdraw = _owner->_withdraw;
	if( endWithdraw == MISSING ) {
		endWithdraw = 0;
	}

	// Check to see if spillway may be active before going overboard.
	if( startStorage < _minSpillStorage ) {
		// We don't start spilling, check on the likelihood of spilling
		// at the end of the timestep
		// Waterbalance based on change in storage and average flows 
		// 	over the timestep.  (losses are already removed)
		timestepSec = Method::getTimeStepSec();
		endStorage =  startStorage + 
			(startInflow+endInflow)/2 * timestepSec -
			(startRelease+endRelease)/2 * timestepSec -
			(startWithdraw+endWithdraw)/2 * timestepSec;

		if( endStorage < _minSpillStorage ) {
			// Deactivate the spillway, since it does no work.
			_Active = 0;
			_myValue = 0;
//			if( group_val ) {

			if( !group_val ) {
				_owner->_spill = _myValue; 
			}
			else {

				*group_val[0] = _myValue;
			}

			// We are not going to spill.
			// Let Reservoir::finalizeSolution do its work
			return( STATUS_SUCCESS );
		}
	}

	// If we get to this point, we are going to spill.
	
	timestepSub = Method::getTimeStepSec() / _intervals;
	deltaInflow = ( endInflow - startInflow ) * 0.5 / _intervals;
	deltaRelease = ( endRelease - startRelease ) * 0.5 / _intervals; 
	deltaWithdraw = ( endWithdraw - startWithdraw ) * 0.5 / _intervals;
	storage = startStorage;
	spill = _stor_spill_tbl->lookup( storage, GETCOLUMN_2, 
		ALLOW_BOUNDS );
/*
	// We will work with volumes
	timestepSec = Method::getTimeStepSec() / _intervals;
	deltaInflowV = ( endInflow - startInflow ) / _intervals * timestepSec;
	deltaReleaseV = ( endRelease - startRelease ) / _intervals * 
		timestepSec;
	deltaWithdrawV = ( endWithdraw - startWithdraw ) / _intervals * 
		timestepSec;

	// Prepare the various timeseries, interpolating the values throughout
	_inflVol_ts[0] = startInflow * timestepSec;
	_relVol_ts[0] = startRelease * timestepSec;
	_withVol_ts[0] = startWithdraw * timestepSec;
	_stor_ts[0] = startStorage;
	_spillVol_ts[0] = _stor_spill_tbl->lookup( _stor_ts[0], GETCOLUMN_2, 
		ALLOW_BOUNDS ) * timestepSec;

	for( i = 1; i <= _intervals; i++ ) {
		_inflVol_ts[i] = _inflVol_ts[i-1] + deltaInflowV;
		_relVol_ts[i] = _relVol_ts[i-1] + deltaReleaseV;
		_withVol_ts[i] = _withVol_ts[i-1] + deltaWithdrawV;
		_spillVol_ts[i] = 0;
	}

	// Run the water balance through the intervals
	for( i = 1; i <= _intervals; i++ ) {
		// We will assume that the spill through the end of the timestep
		// is defined by the storage at the beginning of the timestep 
		// and that the spill is constant over the timestep.
		// True, there is error here, but wise parameterization will
		// keep it minimal.  It also allows us to more accurately
		// define the volume spilling from the reservoir.

		_stor_ts[i] =  _stor_ts[i-1] + 
			(_inflVol_ts[i-1] + _inflVol_ts[i]) / 2 -
			(_relVol_ts[i-1] + _relVol_ts[i]) / 2 -
			(_withVol_ts[i-1] + _withVol_ts[i]) / 2 -
			_spillVol_ts[i-1];
		if( _stor_ts[i-1] > _minSpillStorage ) {
			// Determine the corresponding spill to be applied
			 _spillVol_ts[i] = timestepSec * _stor_spill_tbl->
				lookup( _stor_ts[i-1], GETCOLUMN_2, 
				ALLOW_BOUNDS );
		}
	}
*/
	// Run the water balance through the intervals
	for( i = 1; i <= _intervals; i++ ) {
		
		ddq = (
			(startInflow + deltaInflow * i)-
			(startRelease + deltaRelease * i)-
			(startWithdraw + deltaWithdraw * i) -
			spill
		      );
		storage =  storage + ddq  * timestepSub;

		if( storage > _minSpillStorage ) {
			// Determine the corresponding spill to be applied
			 spill = _stor_spill_tbl->
				lookup( storage, GETCOLUMN_2, 
				ALLOW_BOUNDS );
		}
	}

	// Set the _myValue, in case it is needed for _SpecialTie work
	_myValue = spill;
	endStorage = storage;
//	if( group_val ) {

	if( !group_val ) {
		_owner->_spill = _myValue; 
	}
	else {

		*group_val[0] = _myValue;
	}

	// We've completed the water balance through the full timestep.
	// Now we modify the values on the reservoir to preserve instantaneous
	// release, pool and storage values.
	_owner->_release = endRelease + _myValue;
//	endPool = _owner->_elev_stor_tbl.lookup( _stor_ts[_intervals], 
	endPool = _owner->_elev_stor_tbl.lookup( endStorage, 
		GETCOLUMN_1, ALLOW_BOUNDS );
/*
printf( " !!!SP _mon= %2d _day= %2d _hour= %2d \n", 
         cur_date.getMonth(), cur_date.getDay(), cur_date.getHour());
printf(" !!!SP _s0= %10f _s1= %10f _h1= %10f _i0= %10f _i1= %10f \n"
         "_r0= %10f _r1= %10f _my= %10f \n",
         startStorage, endStorage, endPool, startInflow, endInflow, 
	 startRelease, endRelease, _myValue);
*/
	// Check to see if we are concerned with minpool.
	if( endPool <= _owner->_min_pool ) {
		// Let the user know that system parameterization should be 
		// examined closely and probably revised.
		PrintWarning( 1, routine, "Spillway solution and Reservoir "
			"minimum pool are conflicting.  Please review the "
			"paramterization as this is VERY LIKELY to be "
			"UNREALISTIC.  (Reservoir %s, Spillway Method %s)", 
			_owner->_id, _id );

		// Set the values
		_owner->_pool = _owner->_min_pool;
	}
	else {
		_owner->_pool = endPool;
	}
	// NOTE: The storage will be determined from the pool as part of the 
	// Reservoir::finalizeSolution, even if we do not recalculate the mass 
	// balance therein.
	

	// Set the "don't recalc' water balance flag
	_owner->_conserve_mass = 0;
	
	// Unlike other methods, we don't need to worry about combo methods.
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_solveMethod.cxx,v 1.5 2006/10/26 15:36:23 hsu Exp $";}
/*  ===================================================  */

}
