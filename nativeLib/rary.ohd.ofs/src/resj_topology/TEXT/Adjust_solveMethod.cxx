//------------------------------------------------------------------------------
// Adjust :: solveMethod - Algorithm that solves the Adjust method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 24 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 28 Aug 1998	DKW, RTi		Redid blending logic
// 24 Sep 1998	DKW, RTi	Redid data retrieval based on Method-centric
//				input TS storage.
// 28 Nov 2001	James R. VanShaar, RTi	Revamped algorithm for BLEND to be 
//					consistent with other method's blends.
// 14 Oct 2002	JRV, RTi	Added handling with _adjsim.
// 11 Dec 2002	JRV, RTi	Added assignment of _Active.
// 10 Mar 2004	JRV, RTi	Added reinitialization of 
// 				_owner->_conserve_mass to address a bug related
// 				to the Spillway method's use of the same 
// 				variable.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Adjust.h"

int Adjust :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	if ( !_adjsim ) {
		// Don't bother setting states or returning values in the 
		// arrays, just carry on.
		_Active = 1;
		return( STATUS_SUCCESS );
	}

	char routine[]="Adjust :: solveMethod";
	double pool = MISSING, release = MISSING, deviate, mean_rel, 
		prev_release = 0;
	int i, reset_obs = 0;
	TSDate prev_date;

	// Set _Active to 1
	_Active = 1;

	// Reinitialize the reservoir's trigger to calculate the mass balance.
	// We may turn this off later, but we need to ensure that if we don't,
	// it will be calculated despite any previous instruction to avoid the
	// mass balance (such as Spillway method's instruction).
	_owner->_conserve_mass = 1;

	// Prepare previous values, if not immediately available
	prev_date = _owner->_prev_date;
	if ( prev_date < Method::getForecastDate1() ) {
		// Need to access from carryover
		// prev_inflow
		prev_release = _owner->_releaseCO;
	}
	else {
		// access from timeseries
		HourTS *releaseTS;
		releaseTS = _owner->_release_ts;
		prev_release = releaseTS->getDataValue( prev_date );
	}

	// Check to see that we have some observed data to adjust to. If not, 
	//	exit gracefully without resetting the reservoir
	// 	states.
	if( !_pool_obs && !_release_obs ) {
		PrintStatus( 1, routine, "Adjust method %s has no observed "
			"data - not adjusting reservoir\"%s\" states.", _id,
			_owner->_id );
		return( STATUS_SUCCESS );
	}

	// Once we are here, determine which combination of observed data
	// we have.
	// Options:
	//	A) Observed pool and release
	//	B) Observed pool
	//	C) Observed release

	if( _pool_obs != NULL && _release_obs != NULL ) {
		// We are in option A (11/01 II.4-RES-J-Adjust type #4)
		release = _release_obs->getDataValue( cur_date );
		pool = _pool_obs->getDataValue(	cur_date );
		if( pool == MISSING ) {
			pool = _owner->_pool;
		}
		else {
			_owner->_conserve_mass = 0;
		}

		// Ensure compliance with pool MinimumPool
		if( pool < _owner->_min_pool ) {
			PrintWarning( 1, routine, "Observed pool for %s on "
				"%s is less than the Minimum pool (%f < %f). "
				"Setting to Minimum pool.", _id, _owner->_id,
				pool, _owner->_min_pool );
			pool = _owner->_min_pool;
		}

		if( release == MISSING && ( _n_tstep <= _n_blend ) ) {
			if( prev_release != MISSING ) {
				deviate = _owner->_release - prev_release;
				release = prev_release + deviate /
					( _n_blend - _n_tstep +1 );
				_n_tstep++;
			}
			else {
				// Otherwise, calculate the adjusted discharge
				// as the simulated discharge.
				release = _owner->_release;
			}
		}
		else if( release == MISSING ) {
			release = _owner->_release;
		}
		else {
			reset_obs = 1;
			/*****	What is this all about?! JRV:11/28/01
			//if( _release_obs->getInstantaneous() ) {
				//release = 2.0 * release - 
					//_owner->_release;
			//}
			*****/
			// NOTE: We do not handle mean discharge!
		}

		// Clean up here - set the state variables or the incoming
		// double pointers
		if( !group_val ) {
			_owner->_release = release;
			_owner->_pool = pool;
		}
		else {
			// A combo method called the Adjust method
			*group_val[0] = release;
			*group_val[1] = pool;
			return( STATUS_SUCCESS );
		}
	}
	else if( _pool_obs != NULL ) {
		// We are in option B (11/01 II.4-RES-J-Adjust type #1)

		// Set the reservoir state if the observed value is not missing.
		pool = _pool_obs->getDataValue( cur_date );
		if( pool == MISSING ) {
			pool = _owner->_pool;
		}
		else {
			_owner->_conserve_mass = 0;
		}

		// Ensure compliance with pool MinimumPool
		if( pool < _owner->_min_pool ) {
			PrintWarning( 1, routine, "Observed pool elevation is "
				"below the minimum pool on %s (%f < %f). "
				"Setting pool to minimum.", _owner->_id, pool, 
				_owner->_min_pool );
			pool = _owner->_min_pool;
		}

		if( !group_val ) {
			_owner->_pool = pool;
		}
		else {
			// Adjust does not adjust the release in the 
			// case of observed pool, so if a ComboMethod
			// is calling this then set the release to th
			// unadjusted release. The ComboMethod will require it
			// for consideration with / against other releases.
			*group_val[0] = _owner->_release;
			*group_val[1] = pool;
		}

		// Storage and mean discharge will be defined under normal 
		// procedures in Reservoir::finalizeSolution.  Instantaneous 
		// discharge will remain as calculated by other methods (or at 
		// last time step if this is the only method--it better not be.)

		return( STATUS_SUCCESS );
	}
	else if( _release_obs != NULL ) {
		// We are in option C (11/01 II.4-RES-J-Adjust type #2)

		// find the observed release...
		release = _release_obs->getDataValue( cur_date );
		if( release == MISSING && ( _n_tstep <= _n_blend ) ) {
			if( prev_release != MISSING ) {
				deviate = _owner->_release - prev_release;
				release = prev_release + deviate /
					( _n_blend - _n_tstep +1 );
				_n_tstep++;
			}
			else {
				release = _owner->_release;
			}
		}
			// A combo method called the Adjust method
		else if( release == MISSING ) {
			release = _owner->_release;
		}
		else {
			// The release is known
			reset_obs = 1;
			/*****	What is this all about?! JRV:11/28/01
			//if( _release_obs->getInstantaneous() ) {
				//release = 2.0 * release - _owner->_release;
			//}
			*****/
			// NOTE: We do not handle mean discharge!
		}

		// Set the appropriate variables depending on calling 
		// function... 
		if( !group_val ) {
			_owner->_release = release;
		}
		else {
			// A combo method called the Adjust method
			*group_val[0] = release;
			return( STATUS_SUCCESS );
		}	
	}

	// If we get here, then the calling function was not a Combo method or
	// a non-primary solution. Check to see if we need to reset the last
	// observed release value. (We don't want to do this if the calling 
	// function is a Combo or non-primary solution)
	if( reset_obs ) {
		_n_tstep = 1;
		// _last_obs_rel = release;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Adjust_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: Adjust_solveMethod.cxx,v 1.6 2006/10/26 15:08:31 hsu Exp $";}
/*  ===================================================  */

}
