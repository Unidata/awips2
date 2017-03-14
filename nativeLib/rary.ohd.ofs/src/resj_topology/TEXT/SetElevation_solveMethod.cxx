//------------------------------------------------------------------------------
// SetElevation :: solveMethod - Algorithm that solves the SetElevation method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 14 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 28 Aug 1998  DKW, RTi		Redid blending logic
// 24 Sep 1998  DKW, RTi	Redid data retrieval based on Method-centric
//				input time series storage.
// 07 Oct 1998  DKW, RTi		Redid blending logic (again)
// 18 May 2001	James R. VanShaar, RTi	Revamped algorithm for BLENDTBL and BLENDTS
// 24 May 2001	JRV, RTi	Modified solution technique to semi-implicit in
//				that this method peeks at inflows and the 
//				currently prescribed withdrawals in order to
//				prescribe a release so that the elevation at the
//				end of the timestep is the target elevation.
// 31 May 2001	JRV, RTi	Introduced an anti-oscillation solution 
//				technique.
// 28 Nov 2001	JRV, RTi	Corrected blending to blend from previous 
//				pool. (Before it was from _owner->_pool
//				which (conceivably) may be set by earlier method
//				(this time step).
// 01 Aug 2002  KSH, HRL        Added BLENDTBL keyword to indicate table data
//                              blending. BLEND is retained for old datasets.
// 11 Dec 2002	JRV, RTi	Added assignment of _Active.
// 19 Feb 2004	JRV, RTi	Replaced calls to sumInflow with calls to 
// 				getTotalInflow.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetElevation.h"

int SetElevation :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[]="SetElevation :: solveMethod";
	int 	i, blend_flag = 0, blend_ts_flag = 0, haveTSvalue = 0;
	double	elev_value = MISSING, changeInStor = 0.0, deviate = 0.0,
		preRelease, release, target_value;
	TSDate prev, next;
	double prev_inflow, prev_release, prev_withdraw, prev_pool;
	TSDate prev_date = _owner->_prev_date;

	// Set _Active to 1
	_Active = 1;

	// Prepare previous values, if not immediately available, for reference
	//	and expected water balance calculation below
	if ( prev_date < Method::getForecastDate1() ) {
		// Need to access from carryover
		// prev_inflow
		if ( _owner->_inflowCO == MISSING ) {
			TSDate t1 = Method::getForecastDate1();
			prev_inflow = _owner->getTotalInflow( t1 );
		}
		else {
			prev_inflow = _owner->_inflowCO;
		}
		// prev_release
		prev_release = _owner->_releaseCO;
		// prev_withdraw
		prev_withdraw = _owner->_withdrawCO;
		// prev_pool
		prev_pool = _owner->_poolCO;
	}
	else {
		// access from timeseries
		prev_inflow = _owner->getTotalInflow( prev_date );
		prev_release = _owner->_release_ts->getDataValue( prev_date );
		prev_withdraw = _owner->_withdraw_ts->getDataValue( prev_date );
		prev_pool = _owner->_pool_ts->getDataValue( prev_date );
	}
	double storage = _owner->_elev_stor_tbl.lookup( prev_pool, 
		GETCOLUMN_2, ALLOW_BOUNDS );

	// Before we get into all sorts of calculations, check to see if we have
	// an observed elevation time series with a non-missing value.  If we 
	// do, we will jump to the calculation of the release
	if ( _pool_obs ) {
		// An observed elevation time series exists.  Is its value 
		// missing?
                elev_value = _pool_obs->getDataValue( cur_date );
                if( elev_value != MISSING ) {
                       	_ts_step = 1;
			haveTSvalue = 1;
			if( elev_value < _owner->_min_pool ) {
				// Print a warning; adjustments (if necessary)
				// will take place in 
				// Reservoir::finalizeSolution
				PrintWarning( 1, routine, "Observed pool for "
					"%s on %s (%.3f) < MINIMUMPOOL (%.3f). "
					"MINIMUMPOOL used.", _owner->_id, 
					cur_date.toString(), elev_value, 
					_owner->_min_pool );
				elev_value = _owner->_min_pool;
			}
			// Proceed with calculation of the release.
		}
	}

	// If we don't have an elevation from the time series, lets calculate
	if ( !haveTSvalue ) {
		target_value = _elev_ctl.getDataValue( cur_date, _mode );
		
		// Check for BLENDTS and existing observed time series
		if ( ( _n_blend_ts > 1 ) && _pool_obs ) {
			// BLENDTS is defined and an observed pool timeseries 
			// exists.  If we get here we already know that we are 
			// missing the data.
			// We may need to apply BLENDTS
			if( _ts_step <= _n_blend_ts ) {
                               	// Perform timeseries blending using the target
				// value
                               	blend_ts_flag = 1;
                       	}
                       	else {
				// We have completed the BLENDTS.  We won't set 
				// the blend_ts_flag, but may end up using 
				// BLENDTBL, if defined.
                       	}
                }

		// If we need to apply BLENDTS, do so now
		if ( blend_ts_flag ) {
	   	// NOTE for PROGRAMMER:  Changes include 1) replacing 
		//	_tbl_step with _ts_step, 2) Under section commented 
		//	'Check to see if blending conditions are met', combine 
		//	the two 'prev' date conditionals into one, and if true 
		//	simply assign a value of 1 to _ts_step (removing the 
		//	other stuff).
			if ( _mode != INTERPOLATE ) {
				// We are not interpolating across time, which 
				// would make the following check irrelevant.

				// Check if cur_date is either on a data date in
				// the _elev_ctl DistributedTS (the SetElevation
				// table) or if the cur_date and the _prev_date 
				// straddle a data date.

				// Get prev, which will be equal to cur_date if
				// cur_date is a defining point in the 
				// SetElevation table, or will be equal to the 
				// date previous to cur_date in the table.
				_elev_ctl.getPrevNextDates( cur_date, &prev, 
					&next );

				// Must manually set the years on the previous 
				// dates because these are coming from a 
				// relative time series i.e. no years are set.
				prev.setYear( cur_date.getYear() );
				if ( prev > cur_date ) {
					// prev and curr_date are straddling the
					// year's end
					prev.addYear( -1 );
				}

				// Check to see if blending conditions are met
				if ( ( prev == cur_date ) || 
					( _owner->_prev_date < prev ) ) {
					// cur_date is on a date defining the 
					// setElevation table, meaning current 
					// elevation is different from before.
					// OR
					// Time period from the last solution to
					// the current solution stradles a 
					// defining point on the SetElevation 
					// table
					_ts_step = 1;
				}
			}

	   		// We now have the appropriate _ts_step.  Perform the 
			// BLENDTS
	   		if( target_value != MISSING ) {
				deviate = target_value - prev_pool;
	   		}
	   		else {
				deviate = 0.0;
	   		}
	   		elev_value = prev_pool + ( deviate / 
				( _n_blend_ts - _ts_step + 1 ) );
           		_ts_step++;
		}

		// Check for BLENDTBL and determine types to do.
		else if ( _n_blend_tbl > 1 ) {
			// Determine if we have any NEW blending conditions
			// blend_flags are set:	0 = no NEW blend required
			//			1 = NEW blending due to TIME 
			//				change
			//			2 = Continuation of old blend
			// All non-zero blend_flags are handled identically, but
			// 	are assigned to maintain knowledge of source of
			//	BLENDTBL
			if ( _mode != INTERPOLATE ) {
				// We are not interpolating across time, which 
				// would make the following check irrelevant.

				// Check if cur_date is either on a data date in
				// the _elev_ctl DistributedTS (the SetElevation
				// table) or if the cur_date and the _prev_date 
				// straddle a data date.

				// Get prev, which will be equal to cur_date if
				// cur_date is a defining point in the 
				// SetElevation table, or will be equal to the 
				// date previous to cur_date in the table.
				_elev_ctl.getPrevNextDates( cur_date, &prev, 
					&next );

				// Must manually set the years on the previous 
				// dates because these are coming from a 
				// relative time series i.e. no years are set.
				prev.setYear( cur_date.getYear() );
				if ( prev > cur_date ) {
					// prev and curr_date are straddling the
					// year's end
					prev.addYear( -1 );
				}

				// Check to see if blending conditions are met
				if ( prev == cur_date ) {
					// cur_date is on a date defining the 
					// setElevation table, meaning current 
					// elevation is different from before.
					blend_flag = 1;
					_tbl_step = 1;
				}
				else if ( _owner->_prev_date < prev ) {
					// Time period from the last solution to
					// the current solution stradles a 
					// defining point on the SetElevation 
					// table
					blend_flag = 1;
					_tbl_step = 1;
				}
			}
		
			// Handle blend_flags
			if ( blend_flag == 0 ) {
				// No new blending requirements.
				// Are we in the middle of a previously 
				// prescribed blend?
				if( (_tbl_step > 1) && 
					(_tbl_step <= _n_blend_tbl) ) {
					// We are in the middle of a blend.  
					blend_flag = 2;
				}
				else {
					// We are not in the middle of a blend.
					// Assign unblended value derived from 
					// the table
					elev_value = target_value;
				}
			}
			if ( blend_flag >= 1 ) {
				// Calculate the difference over which to blend
				deviate = target_value - prev_pool;	
				
				// Calculate the blended value
				elev_value = prev_pool + deviate / 
					( _n_blend_tbl - _tbl_step + 1 ); 
	
				// Increment the counter of blend timesteps to 
				// equal one more than the number already done 
				// (so it is ready for the next round.)
				_tbl_step++;
			}
		}

		// No BLENDTS nor BLENDTBL work done.  Assign target value from 
		// table as elevation
		else {
			elev_value = target_value;
		}

		if ( elev_value == MISSING ) {
			// We are in an interesting predicament which should not
			// occur
			PrintWarning( 1, routine, "SetElevation \"%\" is "
				"missing its target value for %s.  Setting it "
				"to current pool.", _id, cur_date.toString() );
			elev_value = prev_pool;
		}
	}

	// prepare current water balance terms
	double cur_stor = _owner->_elev_stor_tbl.lookup( elev_value, 
		GETCOLUMN_2, ALLOW_BOUNDS );
	double withdraw = _owner->_withdraw;
	double cur_inflow = _owner->getTotalInflow( cur_date );
	
	// Calculate the preRelease corresponding to the expected water balance
	preRelease = 2 / Method::getTimeStepSec() * ( ( storage - cur_stor ) +
		(cur_inflow + prev_inflow) / 2 * Method::getTimeStepSec() -
		(withdraw + prev_withdraw) / 2 * Method::getTimeStepSec() ) - 
		prev_release;

	// Calculate some values used to reduce / eliminate oscillation
	double changeStor;
	if ( !haveTSvalue && _mode == INTERPOLATE ) {
		double prev_target_value = _elev_ctl.getDataValue( prev_date, 
			_mode );
		double prev_stor_val = _owner->_elev_stor_tbl.lookup( 
			prev_target_value, GETCOLUMN_2, ALLOW_BOUNDS );
		changeStor = cur_stor - prev_stor_val;
	}
	  /*** No need for splitting this up
	  // else if ( blend_ts_flag ) {
		  // changeStor = 0.0;
	  // }
	  ***/
	else if ( blend_flag >= 1 ) {
		// Note to JRV: This business is not fully tested, but appears
		// 	to do best when we ignore the changeStor term
		//elev_value = prev_pool + deviate / 
			//( _n_blend_tbl - _tbl_step + 1 ); 
		//changeStor = cur_stor - storage;
		changeStor = 0.0;
	}
	else {
		// No blending or interpolation
		changeStor = 0.0;
	}
	double inToStore = changeStor / Method::getTimeStepSec();

	// If, at this point, the release is below min release, that means 
	// that the requested elevation cannot be met by cutting back on the  
	// normal release schedules.
	if( preRelease < _owner->_min_release ) {
		/***
		PrintWarning( 1, routine, "Cannot meet SetElevation target "
			"of %f on %s for %s - setting release to minimum "
			"release of %f.", elev_value, _owner->_id, 
			cur_date.toString(), _owner->_min_release );
		release = (preRelease + cur_inflow - withdraw) / 2;
		if( release < _owner->_min_release ) {
			release = _owner->_min_release;
		}
		***/
		release = _owner->_min_release;
	}
	else if ( preRelease <= 0.0001 ) {
		// We are in a filling type pattern
		release = 0;
	}
	// Check into oscillation and if necessary, modify release
	// Note: Oscillation may occur when we are not maintaining an constant,
	//	ongoing, previously met elevation goal.  It will be eliminated
	//	when our release equals the net inflow (inflow - withdrawal) 
	// 	minus the portion of the inflow required to raise / lower 
	//	storage 
	else if ( preRelease != (cur_inflow - withdraw) - inToStore ) {
		// Address special case of oscillation when by traditional 
		// method every other release is forced to zero. ( This is 
		// observed with interpolated, rising target pool and low 
		// inflow values).
			//release = (preRelease + cur_inflow - withdraw) / 2;
			release = (preRelease + cur_inflow - withdraw -
				inToStore ) / 2;
		if( release < _owner->_min_release ) {
		  /*
			PrintWarning( 1, routine, "Cannot meet SetElevation "
				"target of %f on %s for %s - setting release "
				"to minimum release of %f.", elev_value,
				 _owner->_id, cur_date.toString(), 
				_owner->_min_release );
		  */
			release = _owner->_min_release;
		}
	}
	else {
		// No oscillation apparent
		// Note: The conditional expression comparing equality between
		//	two doubles will almost always return true, so this
		//	may never actually be executed.
		release = preRelease;
	}

        // Only set the _owner members if calling function is not
	// a ComboMethod.
	if( !group_val ) {
		// Change the release to reflect its solution
		_owner->_release =  release; 
	}
	else {
		*group_val[0] = release; 
//		return( STATUS_SUCCESS );
	}

// printf(" !RRR SetElevation group_id=%d res=%s meth=%s pool=%f release=%f \n", _group_id, _owner->getID(), _id, elev_value, release);
	PrintDebug( 5, routine, "Setting release on %s to %f.", _owner->_id,
		_owner->_release );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetElevation_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: SetElevation_solveMethod.cxx,v 1.8 2006/10/26 15:33:14 hsu Exp $";}
/*  ===================================================  */

}
