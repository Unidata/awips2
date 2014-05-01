//------------------------------------------------------------------------------
// Reservoir::finalizeSolution - does solution cleanup at the end of a 
//				   time-step.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 15 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 24 Sep 1998	DKW, RTi	Retooled to fit Method-centric input TS storage.
// 07 Oct 1998  DKW, RTi	Configured for non-mass-conservation solution	
// 18 Apr 2001  James R. VanShaar, RTi	Corrected calcluation of release if
//					pool <= min_pool
// 25 Apr 2001  JRV, RTi	Included special instructions for Panama
//				Trinidad reservoir investigation
// 24 May 2001	JRV, RTi	Modified water balance to use average flows over
//				the timestep
// 05 Jun 2001	JRV, RTi	Added Check for possible missing value of 
//				_release
// 13 Nov 2001	JRV, RTi	Moved carry over writing to Component::solve
// 06 Jun 2002	JRV, RTi	Converted from cur_inflow to _endInflow use
//				and prev_* to start* where appropriate
// 07 Jun 2002	JRV, RTi	Moved handling of updating steps to the
//				setEndOfTimestepStates function.
// 12 Jun 2002	JRV, RTi	Added _endStorage.
// 12 Dec 2002	JRV, RTi	Added _specialTieOut.
// 27 Mar 2006  JRV, RTi   Added recalculation of cur_stor from _pool following
//                         release reduction anti-oscillation algorithm.
//                         Revised valueToMethod calls to use new method.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reservoir.h"
#include "ResJSys.h"

int Reservoir::finalizeSolution( TSDate& cur_date )
{
	char routine[] = "Reservoir::finalizeSolution";
	int i;
	double	cur_stor = 0.0, mean_flow, tmp_pool = MISSING,
		tmp_rel = MISSING, tmp_with = MISSING, minStor, poolEntering; 
	double prescribedR = _release, newR = 0.0, prescribedW = _withdraw;
        int reviseWith = 0;
        double TSsec = Method::getTimeStepSec();

	// Copy some values for later reference
	poolEntering = _pool;

	// The first thing, and primary function of finalize Solution, is
	// to solve the continuity equation for this reservoir.
	if( _storage == MISSING ) {
		PrintWarning( 1, routine, "Must have a carryover storage "
			"value for %s.", _id );
		return( STATUS_FAILURE );
	}

	if( _endInflow == MISSING ) {
		PrintWarning( 1, routine, "Reservoir %s is solving using an "
			"inflow value of 0 for time step %s.  At least one of "
			"its input inflow time series has MISSING values.", 
			_id, cur_date.toString() );
		_endInflow = 0.0;
	}

	// Check for MISSING value of _release
	if ( _release == MISSING ) {

		PrintWarning( 1, routine, "No rules used on %s "
			"do pass inflow %s", _id, cur_date.toString());
		//_release = _endInflow - _withdraw;
		_release = _endInflow;
	}

	// Address mass balance (or lack thereof)

	if( !_conserve_mass ) {
		cur_stor = _elev_stor_tbl.lookup( _pool, GETCOLUMN_2, 
			ALLOW_BOUNDS );
	}
	else {
		// Waterbalance based on change in storage and average flows 
		// 	over the timestep.  (losses are already removed)
		cur_stor =  _storage + 
			(_startInflow+_endInflow)/2 * Method::getTimeStepSec() -
			(_startRelease+_release)/2 * Method::getTimeStepSec() -
			(_startWithdraw+_withdraw)/2 * Method::getTimeStepSec();
		// in terms of carryover . . . 
		//	_startInflow is new INITIALINFLOW
		//	_startRelease is INITIALRELEASE
		//	_startWithdraw is INITIALWITHDRAW
		//	_endInflow is in the timeseries
		//	_release and _withdraw were calculated this timestep.

		_pool = _elev_stor_tbl.lookup( cur_stor, GETCOLUMN_1, ALLOW_BOUNDS );
		double pool_top = _elev_stor_tbl.getMax( 0 );
		if (_pool > pool_top - 0.1) {
			PrintWarning( 1, routine, "%s pool %.2f Hit top of elevation-storage curve %.2f %s "
				"do pass inflow ", _id,  _pool, pool_top, cur_date.toString());
			// _release = _endInflow - _withdraw;
			_release = _endInflow;
		}
		// If _pool was determined to be less than this reservoir's 
		// _min_pool, it is then necessary to solve for a new release 
		// to match this pool.  Because this constraint on the 
		// operations has precedence over the _min_release constraint, 
		// take care of it first. 
		
		//The second mass balance solution is 
		// for normal operations in which the _min_pool constraint was 
		// not violated.

		// Check for _min_pool restrictions and if necessary
		// reduce _release.  The _min_pool test is made actually by
		// checking the minimum pool storage and the storage we just
		// calculated with the waterbalance.
		minStor =  _elev_stor_tbl.lookup( _min_pool, GETCOLUMN_2, 
			ALLOW_BOUNDS );
		double maxRelease = 0.0;
		if( cur_stor < minStor ) {
			_pool = _min_pool;
			cur_stor = _elev_stor_tbl.lookup( _pool, GETCOLUMN_2, 
				ALLOW_BOUNDS );
			// Calculate the maximum amount of release available
			// according to storage and pool limits (Losses are
			// already removed)
			maxRelease = 2 / Method::getTimeStepSec() * 
				( ( _storage - cur_stor ) +
				(_endInflow + _startInflow) / 2 * 
				Method::getTimeStepSec() -
				(_withdraw + _startWithdraw) / 2 * 
				Method::getTimeStepSec() ) - _startRelease;
			
			// Calculate some values used to reduce / eliminate
			// oscillation.  The following code is based on concepts
			// developed in SetElevation oscillation work.  
			// Therefore, there may be some redundancy in 
			// calculating values for new variables, whose values
			// already have been calculated in this function.
			double inToStore = 0.0;		// Apparently not useful
							// in this function.
			double decision;
			decision = (_endInflow - _withdraw) - inToStore;
			// Compare size, deal w/ double precision
			if( (maxRelease - decision) > 0.000001 ) {
				// reduce preRelease by averaging it with water
				// coming available at end of time step
				_release = (maxRelease + _endInflow - _withdraw
					 - inToStore ) / 2;
				// If maxRelease is negative, _release will be 
				// assigned to zero, nevertheless, print 
				// informative warning now
				newR = _release;
				if ( maxRelease < 0.0 ) {
					newR = 0.0;
				}
				PrintWarning( 1, routine, 
					"Unable to meet release (%f) on %s--"
					"anti-oscillation algorithm reset "
					"release to %f on %s", prescribedR, 
					cur_date.toString(), newR, _id );
				cur_stor =  _storage + 
					(_startInflow+_endInflow)/
					2 * Method::getTimeStepSec() - 
					(_startRelease+newR)/2 * 
					Method::getTimeStepSec() - 
					(_startWithdraw+_withdraw)/2 * 
					Method::getTimeStepSec();
				_pool = _elev_stor_tbl.lookup( cur_stor, 
					GETCOLUMN_1, ALLOW_BOUNDS );
				if( _pool < _min_pool ) {
					_pool = _min_pool;
				}
				// Now recalculate cur_stor just in case we hit
				// a boundary.
				cur_stor = _elev_stor_tbl.lookup( _pool,
					GETCOLUMN_2, ALLOW_BOUNDS );
				// Possible variable signs:
				// maxRelease -; decision -; _release -;
				// maxRelease +; decision -; _release +;
				// maxRelease +; decision +; _release +;
			}
			else {
				// Either:
				// maxRelease < (_endInflow - _withdraw) - 
				//	inToStore ) 
				// In which case- We will not address possible 
				//	oscillation, as no predictable and 
				//	reasonable algorithm has been developed.
				//	Likely the following approach will allow
				//	any oscillation that may be developing 
				//	to be addressed next time step with the 
				//	reduction technique above.  Some of it 
				//	may be addressed below (if _release<0).
				// 	Possible variable signs:
				//	maxRelease -; decision -; _release -;
				//	maxRelease -; decision +; _release -;
				//	maxRelease +; decision +; _release +;
				// Or:
				// maxRelease = (_endInflow - _withdraw) - 
				//	inToStore )
				// In which case- There is no oscillation likely
				//	to occur.
				// 	Possible variable signs:
				//	maxRelease -; decision -; _release -;
				//	maxRelease +; decision +; _release +;
				// If maxRelease is negative, _release will be 
				// assigned to zero, nevertheless, print 
				// informative warning now
				_release = maxRelease;
				newR = _release;
				if ( maxRelease < 0.0 ) {
					newR = 0.0;
				}
				if( prescribedR - newR > 0.000001 )
				{
					PrintWarning( 1, routine, "Unable to "
					   "meet release (%f) on %s--reset to "
					   "%f and pool to minimum pool on %s", 
					   prescribedR, cur_date.toString(),
					   newR, _id );
				}
			}

			double maxWithdrawal = 0.0;
			if ( _release < 0.0 )
			{
			    _release = 0.0;
			    reviseWith = 1;
			    // Calculate the maximum amount of withdrawal
			    // available according to storage and pool limits
			    // (Losses are already removed and _release = 0). 
			    // The value can be negative.
			    maxWithdrawal = 2 / TSsec * 
			        ( ( _storage - cur_stor ) +
			        (_endInflow + _startInflow) / 2 * TSsec -
			        (_release + _startRelease) / 2 * TSsec )
			        - _startWithdraw;

			    if( _withdraw > maxWithdrawal )
			    {
			        if( maxWithdrawal > _endInflow )
			        {
			            // Reduce potential for oscillation by
			            // reducing maxWithdrawal
			            maxWithdrawal = ( maxWithdrawal +
			                _endInflow ) / 2;
			            // We still didn't have enough water.
			            PrintWarning( 1, routine, "Unable to meet "
			                "withdrawl (%f) on %s at %s--anti-"
			                "oscillation algorithm active.",
			                prescribedW, _id, cur_date.toString() );
			        }
			    }

			    // Modify the withdrawal and transfer any flow
			    // ToComp
			    valueToMethod( cur_date, maxWithdrawal );
			    // _withdraw may have been revised in valueToMethod.

			    // Now double check the mass balance and send messages.
			    cur_stor =  _storage + (_startInflow+_endInflow)/2 *
			        TSsec - (_startRelease+_release)/2 * TSsec -
			        (_startWithdraw+_withdraw)/2 * TSsec;

			    if( cur_stor < minStor )
			    {
			        // We still didn't have enough water.
			        PrintWarning( 1, routine, "Low inflows and "
			            "MINPOOL have caused minor oscillation "
			            "problems in solution of %s at %s.  "
			            "Water balance has been violated and "
			            "withdrawal (if applicable) has been "
			            "reduced from %f to %f", _id, 
			            cur_date.toString(), prescribedW,
			           _withdraw  );
			        cur_stor = minStor;
			    }
			    else
			    {
			        // We still didn't have enough water.
			        PrintWarning( 1, routine, "Unable to meet "
			            "withdrawal on %s at %s.  Withdrawal (if "
			            "applicable) has been reduced from %f to "
			            "%f", _id, cur_date.toString(), prescribedW,
			           _withdraw  );
			    }

			    _pool = _elev_stor_tbl.lookup( cur_stor,
			         GETCOLUMN_1, ALLOW_BOUNDS );
			}
		}

	/*********
		// If maxRelease is negative, then assign release value of 0.0,
		// reduce withdrawls, and sound a warning.
		double maxWithdrawal = 0.0;
		if ( maxRelease < 0.0 ) {
			_release = 0.0;
			// Calculate the maximum amount of withdrawal available
			// according to storage and pool limits (Losses are
			// already removed and _release = 0)
			maxWithdrawal = 2 / Method::getTimeStepSec() * 
				( ( _storage - cur_stor ) +
				(_endInflow + _startInflow) / 2 * 
				Method::getTimeStepSec() -
				(_release + _startRelease) / 2 * 
				Method::getTimeStepSec() )
				- _startWithdraw;

			if ( _withdraw > maxWithdrawal ) {
				if ( maxWithdrawal > 0.0 ) {
					_withdraw = maxWithdrawal;
					PrintWarning( 1, routine, 
						"Unable to meet withdraw "
						"(%f)--reset to %f on %s", 
						prescribedW, _withdraw, _id );
				}
				else {
					_withdraw = 0.0;
					PrintWarning( 1, routine, 
						"Low inflows and MINPOOL have "
						"caused minor oscillation "
						"problems in solution of %s on "
						"%s.  Water balance has been "
						"violated and withdrawal (if "
						"applicable) has been reset to "
						"0.0", _id, 
						cur_date.toString() );
				}
				//
				//PrintWarning( 1, routine, 
					//"Unable to meet withdraw (%f)--reset "
					//"to %f on %s", prescribedW, _withdraw,
					//_id );
				//
			}
		}
	********/
	}

        // Deal with any transfer of withdrawal "TOCOMP"
	if ( _specialTieOut )
	{
            // Transfer flow ToComp.  Do not revise withdrawal.
	    if( !reviseWith )
            {
                valueToMethod( cur_date );
            }
	}

	_storage = cur_stor;
/*
 printf(" !!!!F resname=%s _s1= %10f _h1= %10f _i0= %10f _i1= %10f "
         "_r0= %10f _r1= %10f  _w0= %10f _w1= %10f \n", _id,
         _storage, _pool, _startInflow, _endInflow, _startRelease, _release,
	 _startWithdraw, _withdraw);
*/
	mean_flow = ( _release + _startRelease ) / 2.0;

	// Set the time series values...

	_pool_ts->setDataValue( cur_date, _pool );
	_withdraw_ts->setDataValue( cur_date, _withdraw );
	_release_ts->setDataValue( cur_date, _release );
	_spill_ts->setDataValue( cur_date, _spill );
	_storage_ts.setDataValue( cur_date, _storage / 86400.0 );
	_outflow_ts.setDataValue( cur_date, _release );
	_mean_outflow_ts.setDataValue( cur_date, mean_flow );

	PrintDebug( 5, routine, "TIME \"%s\":", cur_date.toString() );
	PrintDebug( 5, routine, "Reservoir \"%s\":", _id );
	PrintDebug( 5, routine, "Inflow:  %f", _endInflow );
	PrintDebug( 5, routine, "Storage:  %f", _storage );
	PrintDebug( 5, routine, "Withdrawal:  %f", _withdraw );
	PrintDebug( 5, routine, "Loss:  %f", _loss );
	PrintDebug( 5, routine, "Release:  %f", _release );
	PrintDebug( 5, routine, "Mean Release:  %f", mean_flow );
	PrintDebug( 5, routine, "Pool:  %f", _pool );

	// Update Rules states
	_endPool = _pool;
	_endRelease = _release;
	_endWithdraw = _withdraw;
	
	// And others
	_endStorage = _storage;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reservoir_finalizeSolution.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_finalizeSolution.cxx,v 1.12 2006/10/26 15:32:25 hsu Exp $";}
/*  ===================================================  */

}
