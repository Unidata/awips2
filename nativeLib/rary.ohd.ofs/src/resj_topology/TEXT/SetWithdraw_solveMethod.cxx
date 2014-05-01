//------------------------------------------------------------------------------
// SetWithdraw :: solveMethod - Solver for the SetWithdraw method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 27 Aug 1998	DKW, RTi		Redid blending logic.
// 24 Sep 1998	DKW, RTi	Redid data retrieval based on Method-centric
//				input time series storage..
// 21 May 2001 JRV, RTi		Expanded time interpolation functionality to 
//				include interpolation across ELEV and both TIME
//				and ELEV (together).
// 21 May 2001 JRV, RTi		Revamped algorithm for BLENDTBL and BLENDTS.
// 21 May 2001 JRV, RTi         Removed check on available volume for withdrawal
//				as it duplicates the waterbalance check in
//				Reservoir::finalizeSolution and doesn't consider
//				inflows in the calculation.  Also eliminated
//				update to state variables as inconsistent with
//				documentation and any similarities.
// 20 Jun 2001	JRV, RTi	Replaced usage of _n_withdraw with
//				_n_elev for consistency with SetRelease
// 20 Jun 2001	JRV, RTi	Changed handling of missing target_value.
// 28 Nov 2001	JRV, RTi	Corrected blending to blend from previous 
//				withdraw. (Before it was from _owner->_withdraw
//				which may be set by earlier method (this time
//				step).
// 01 AUG 2002  KSH, HRL        Corrected numerical stability problem on
//                              constant release among elevation option
// 01 Aug 2002  KSH, HRL        Added BLENDTBL keyword to indicate table data
//                              blending. BLEND is retained for old datasets.
// 11 Dec 2002	JRV, RTi	Added assignment of _Active, _myValue.
// 19 Feb 2004	JRV, RTi	Replaced calls to sumInflow with calls to 
// 				getTotalInflow.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetWithdraw.h"

int SetWithdraw :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[] = "SetWithdraw :: solveMethod";
	double with_value = MISSING, deviate = 0.0, blend_with, prev_pool,
		pool, target_value, obs_value = MISSING, 
		prev_withdraw = MISSING;
	int i, j, pp_index, blend_flag = 0, blend_ts_flag = 0;
	TSDate prev, next, prev_date;

	// Set _Active to 1
	_Active = 1;

	// Before we get into all sorts of calculations, check to see if we have
	// an observed withdraw time series with a non-missing value.  If we do,
	// we will quickly return the appropriate value.
	if ( _withdraw_obs ) {
		// An observed withdraw timeseries exists.  Is its value 
		//	missing?
                with_value = _withdraw_obs->getDataValue( cur_date );
                if( with_value != MISSING ) {
                       	_ts_step = 1;
			// The value will be with_value.  Where it gets set 
			// depends on who (a ComboMethod or a non-Combo method)
			// called this function.
			_myValue = with_value;
			if( !group_val ) {
				_owner->_withdraw = with_value; 
			}
			else {
				*group_val[0] = with_value;
				return( STATUS_SUCCESS );
			}

			PrintDebug( 5, routine, "Setting withdraw for %s to %f "
				"on %s.", _owner->_id, with_value, 
				cur_date.toString() );
			return( STATUS_SUCCESS );
		}
	}

	// We don't have a non-missing value from an observed time series.
	// Search for the proper elevation and corresponding withdraw 
	// time series.  Resulting 'i' corresponds to the largest elevation 
	// which is smaller than or equal to the current pool elevation.
	for( i = _n_elev - 1; i >= 0; i-- ) {
	//for( i = _n_withdraw - 1; i >= 0; i-- ) {
		if( _owner->_pool >= _elev[i] || i == 0 ) {
			break;
		}
	}

	//  ****Calculate target_value based on interpolation (or no 
	//	interpolation) mode
	// No interpolation or only interpolation in time
	if ( _mode == NORMAL || _mode == INTERPOLATE_TIME ) {
	  //		target_value = _withdraw_ctl[i].getDataValue( cur_date, _mode )
	  //			* 24.0 / (double)_t_mult;
          // A modified algorithm for the POOLQ constant discharge approach.

          int cur_elev_ind, next_elev_ind, count; 
          double cur_date_inflow, prev_date_inflow;
          double cur_elev, cur_stor, cur_inflow, cur_outflow;
          double next_elev, next_stor;
          double t_interval, diff_inflow, delta_inflow;
          double a, b, c, d, sol_1, sol_2;
          double time_this_range, time_used, time_left;
          
          // Get the inflow value for the starting time of the period.
          prev_date = _owner->_prev_date;
	  if ( prev_date < Method::getForecastDate1() ) {
		// Need to access from carryover
		// prev_inflow
		prev_date_inflow = _owner->_inflowCO;
	  }
	  else {
		// access from timeseries
          prev_date_inflow = _owner->getTotalInflow(_owner->_prev_date);
	  }

          // Get the inflow value for the ending time of the period.
          cur_date_inflow = _owner->getTotalInflow(cur_date);

	  //cout << routine << endl;
	  //cout << _owner->_prev_date << endl;
	  //       	  cout << "prev_inflow:" << prev_date_inflow << ", cur_inflow:" << cur_date_inflow << endl;

          // Length of the time period in second.
          t_interval = getTimeInterval()*getTimeMult(); 

          diff_inflow = cur_date_inflow-prev_date_inflow;
          delta_inflow = diff_inflow/t_interval;

          // Set initial values.
          cur_elev_ind = i;
          cur_elev = _owner->_pool;
          cur_inflow = prev_date_inflow;
          cur_outflow = _withdraw_ctl[i].getDataValue(cur_date,_mode)*24.0/
                        (double)_t_mult;
          time_used = 0.0;

          // Use the continuity equation (CE) iteratively to determine the 
          // period ending withdraw.
          // Note:
          // The implemented algorithm does not calculate the elevation at 
          // the end of the period to obtain the period ending withdraw.
          do 
          {
            // First, solve the CE for the elevation range to the right of the
            // elevation corresponding to cur_elev_ind. If a real non-negative
            // solution is found, exit the FOR loop. Otherwise, solve the CE 
            // for the left elevation range. If no real non-negative solution 
            // is found after the two iterations of the FOR loop, assign the 
            // current withdraw cur_outflow to target_value.
            // Note: 
            // A. This FOR loop starts at the period starting time. Initially, 
            // cur_elev = _owner->_pool. Based on the method for finding 'i' 
            // above, we consider four cases for cur_elev: 
            //   1. cur_elev > _elev[i].
            //   2. cur_elev = _elev[i]. 
            //   After the first iteration of the DO-WHILE loop, cur_elev will 
            //   only take on an _elev value at some index.
            //   3. cur_elev < _elev[0]. In this case, try to solve the CE 
            //   for the elevation range bounded by _elev[0] and _elev[1].
            //   4. cur_elev >= _elev[_n_elev-1]. Only try the left range.
            // B. The initial elevation range may just be a portion of an
            // elevation range defined in the discharge -- elevation look-up
            // table if cur_elev > _elev[i].

            time_this_range = -999.0; // This variable will get changed to a 
                                      // positive number if the CE has a 
                                      // positive solution.
            for (count = 0; count < 2; count++)
            {
              // Try to solve the CE for the "right range" if count = 0.
              // Try to solve the CE for the "left range" if count = 1.

              // Determine which range to use.
              if (count == 0)
              {
                if (cur_elev_ind == _n_elev-1) continue; // No right range.
                                                         // Try the left one.
                next_elev_ind = cur_elev_ind+1;
              }
              else
                if (cur_elev > _elev[cur_elev_ind])
                  // Only for the first round of the loop.
                  next_elev_ind = cur_elev_ind;
                else
                {
                  if (cur_elev_ind == 0) break; // Break out of the FOR loop.
                  next_elev_ind = cur_elev_ind-1;
                }
              next_elev = _elev[next_elev_ind];

              // Make an attempt to solve the CE for the current range.  
              a = delta_inflow;
              b = 2*(cur_inflow-cur_outflow);
              cur_stor = _owner->_elev_stor_tbl.lookup(cur_elev,GETCOLUMN_2,
                                                       ALLOW_BOUNDS);
              next_stor = _owner->_elev_stor_tbl.lookup(next_elev,GETCOLUMN_2,
                                                        ALLOW_BOUNDS);
              c = -2*(next_stor-cur_stor);
              d = b*b-4*a*c;
              if (a == 0) // The inflow is constant, so the CE is degenerate.
              {
                if (b == 0) break; // The inflow is equal to the outflow.
                time_this_range = -c/b;
                if (time_this_range >= 0) break;
              }
              else
              {
                if (d >= 0) // The CE has a pair of real solutions.
                {
                  d = sqrt(d);
                  sol_1 = (-b+d)/(2*a);
                  sol_2 = (-b-d)/(2*a);
                  if (sol_1 < 0)
                    if (sol_2 < 0) 
                      continue; // No non-negative solution for this range.
                    else
                      time_this_range = sol_2;
                  else
                    if (sol_2 < 0)
                      time_this_range = sol_1;
                    else
                    {
                      if (sol_1 > sol_2)
                        time_this_range = sol_2;
                      else
                        time_this_range = sol_1;
                    }
                  break; // A non-negative solution is found.
                }
              }
            }

            // If for both the left and right elevation range, the CE has no
            // positive solution, exit the WHILE loop.
            if (time_this_range <= 0) break;

            time_used = time_used+time_this_range;
            time_left = t_interval-time_used;

            if (time_left > 0.0) // Prepare to go to the next elevation range.
            {
              cur_elev_ind = next_elev_ind;
              cur_elev = _elev[cur_elev_ind];
              cur_inflow = prev_date_inflow+delta_inflow*time_used;
              cur_outflow = _withdraw_ctl[cur_elev_ind].getDataValue(cur_date,
                            _mode)*24.0/(double)_t_mult;
            }

          } while (time_left > 0.0);

          target_value = cur_outflow;
	  double cur_elevft= cur_elev*3.281;
	  double cur_outflowcfs= cur_outflow*35.31;
	  //       	  cout << routine << "_TIME cur_elev:" << cur_elevft << ", cur_outflow:" << cur_outflowcfs << endl;

	}

	// Interpolate across elevations only
	else if ( _mode == INTERPOLATE_ELEV ) {
		// Based on algorithm for finding 'i' above, we will interpolate
		// using _withdraw_ctl[i] and _withdraw_ctl[i+1].
		// However, we will NOT extrapolate!
		//	if i == _n_elev - 1,  only the maximum elevation is used
		//	if _owner->_pool < _elev[0], only the minimum elevation
		//		is used.  
		//		NOTE: If this is the case, i equals 0.
		if ( ( i == (_n_elev -1) ) || ( _owner->_pool < _elev[0] ) ) {
			// Interpolation would result in extrapolation. Use the
			//	value w/out any interpolation.
			target_value = _withdraw_ctl[i].getDataValue
				(cur_date, NORMAL) * 24.0 / (double)_t_mult;
		}
		else {
			// Access data for each elevation w/out interpolation
			// in time
			double lowVal = _withdraw_ctl[i].getDataValue
				(cur_date, NORMAL) * 24.0 / (double)_t_mult;
			double highVal = _withdraw_ctl[i+1].getDataValue
				(cur_date, NORMAL) * 24.0 / (double)_t_mult;
			// interpolate between the two values based on _pool
			target_value = lowVal + ((_owner->_pool - _elev[i]) / 
				(_elev[i+1] - _elev[i])) * (highVal-lowVal);
		}
	}


	// Interpolate across elevations only
	else if ( _mode == INTERPOLATE_ELEV ) {
		// Based on algorithm for finding 'i' above, we will interpolate
		// using _withdraw_ctl[i] and _withdraw_ctl[i+1].
		// However, we will NOT extrapolate!
		//	if i == _n_elev - 1,  only the maximum elevation is used
		//	if _owner->_pool < _elev[0], only the minimum elevation
		//		is used.  
		//		NOTE: If this is the case, i equals 0.
		if ( ( i == (_n_elev -1) ) || ( _owner->_pool < _elev[0] ) ) {
			// Interpolation would result in extrapolation. Use the
			//	value w/out any interpolation.
			target_value = _withdraw_ctl[i].getDataValue
				(cur_date, NORMAL) * 24.0 / (double)_t_mult;
		}
		else {
			// Access data for each elevation w/out interpolation
			// in time
			double lowVal = _withdraw_ctl[i].getDataValue
				(cur_date, NORMAL) * 24.0 / (double)_t_mult;
			double highVal = _withdraw_ctl[i+1].getDataValue
				(cur_date, NORMAL) * 24.0 / (double)_t_mult;
			// interpolate between the two values based on _pool
			target_value = lowVal + ((_owner->_pool - _elev[i]) / 
				(_elev[i+1] - _elev[i])) * (highVal-lowVal);
		}
	}

	// Interpolate in time and across elevations
	else if ( _mode == INTERPOLATE_ALL ) {
		// Based on algorithm for finding 'i' above, we will interpolate
		// using _withdraw_ctl[i] and _withdraw_ctl[i+1].
		// However, we will NOT extrapolate!
		//	if i == _n_elev - 1,  only the maximum elevation is used
		//	if _owner->_pool < _elev[0], only the minimum elevation
		//		is used.  
		//		NOTE: If this is the case, i equals 0.
		if ( ( i == (_n_elev -1) ) || ( _owner->_pool < _elev[0] ) ) {
			// Interpolation across elevations would result in 
			//	extrapolation.  Interpolate in time only, using
			//	the boundary elevation (minimum or maximum on
			// 	the withdraw table.)
			target_value = _withdraw_ctl[i].getDataValue
				(cur_date, INTERPOLATE_TIME ) * 24.0 / 
				(double)_t_mult;
		}
		else {
			// Start by interpolating in time for each elevation
			double lowVal = _withdraw_ctl[i].getDataValue
				( cur_date, INTERPOLATE_TIME ) * 24.0 / 
				(double)_t_mult;
			double highVal = _withdraw_ctl[i+1].getDataValue
				( cur_date, INTERPOLATE_TIME ) * 24.0 / 
				(double)_t_mult;
			// Interpolate between the two values based on _pool
			target_value = lowVal + ((_owner->_pool - _elev[i]) / 
				(_elev[i+1] - _elev[i])) * (highVal-lowVal);
		}
	}
	
	// Unknown _mode value
	else {
		// This should not happen unless there are loopholes in the
		// read-in algorithm!
		PrintWarning( 1, routine, "Unknown SetWithdraw INTERPOLATE "
			"value '%d' in solution! No interpolation done.", 
			_mode );
		target_value = _withdraw_ctl[i].getDataValue( cur_date, NORMAL )
			* 24.0 / (double)_t_mult;
	}

	// Get previous withdraw, if not immediately available
	prev_date = _owner->_prev_date;
	if ( prev_date < Method::getForecastDate1() ) {
		// Need to access from carryover
		// prev_inflow
		prev_withdraw = _owner->_withdrawCO;
	}
	else {
		// access from timeseries
		HourTS *withdrawTS;
		withdrawTS = _owner->_withdraw_ts;
		prev_withdraw = withdrawTS->getDataValue( prev_date );
	}

	if( target_value == MISSING ) {
		// This should not happen, but . . .
		PrintWarning(1, routine, "SetWithdraw %s unable to find "
			"target value for %s.  Last withdraw (%s) used.", _id,
			cur_date.toString(), prev_withdraw );
		target_value = prev_withdraw;
	}

	// By the time we get to this point, we have figured out our target
	// withdrawal by whatever method is appropriate.  Now we need to 
	// determine if this target value is our final goal, or if we need to 
	// blend toward the target; in essence, reduce our target by some 
	// portion, for the current solution.

	// Check for BLENDTS and existing observed time series
	if ( ( _n_blend_ts > 1 ) && _withdraw_obs ) {
		// BLENDTS is defined and an observed withdrawal timeseries 
		// exists.
		// If we get here we already know that we are missing the data

		// We may need to apply BLENDTS
		if( _ts_step <= _n_blend_ts ) {
			// Perform timeseries blending using the target value
			blend_ts_flag = 1;
		}
		else {
			// We have completed the BLENDTS.  We won't set the 
			// blend_ts_flag, but may end up using BLEND, if 
			// defined.
		}
	}

	// If we need to apply BLENDTS, do so now
	if ( blend_ts_flag ) {
	   // NOTE: The indent in the following code is slightly irregular,
	   //	but was used to allow code to be used from the BLENDTBL work below.
	   //	NOTE for PROGRAMMER:  Changes include 1) replacing _tbl_step 
	   //		with _ts_step, 2) Commenting out references to the
	   //		blend_flag, 3) Under section commented 'Check to see
	   // 		if blending conditions are met', combine the two 'prev'
	   //		date conditionals into one, and if true simply assign
	   //		a value of 1 to _ts_step (removing the other stuff).
	   if( _ts_step != 1 ) {
		// We have an ongoing BLENDTS
	   	// Determine whether we need to restart the blend based on a
	   	// change in SetWithdraw table status. 
		if ( _mode != INTERPOLATE_ELEV && _mode != INTERPOLATE_ALL ) {
			// We are not interpolating across elevations, which
			// would make the following check irrelevant.

			// Check if we have changed an elevation column.  We are
			// solving for withdrawal at cur_date, so the present 
			// pool value is the value solved last timestep.  We 
			// need to know the pool value prior to that, which can
			// be found in the timeseries at 2 timesteps before 
			// cur_date.
			TSDate two_prior = cur_date;
			TSDate modStart =  Method::getForecastDate1();
			two_prior.addInterval( getTimeInterval(), 
				-2*getTimeMult() );
			if ( two_prior < modStart ) {
				// We need to access pool values from the data
				// that came from reservoir carryover
				if ( cur_date == modStart ) {
					// First solution timestep
					prev_pool = _owner->_prevPoolCO;
				}
				else {
					// Second solution timestep
					prev_pool = _owner->_poolCO;
				}
			}
			else {
				prev_pool = _owner->_pool_ts->
					getDataValue( two_prior );
			}
			if( prev_pool == MISSING ) {
				// ****RESTART BLENDTS
				PrintWarning( 1, routine, "Pool value missing "
					"for %s used to check a change in "
					"setWithdraw %s table elevation "
					//"condition.  Beginning new BLENDTBL as "
					"condition.  Beginning new BLENDTS as "
					"if it had. ", getID(), 
					two_prior.toString() );
				// blend_flag = 2;
				_ts_step = 1;
			}
			else {
				for( j = _n_elev - 1; j >= 0; j-- ) {

					if( prev_pool >= _elev[j] || j == 0 ) {
						pp_index = j;
						break;
					}
				}
				// if pp_index != i then we have changed our 
				// elevation index in the control file time 
				// series.  
				if( i != pp_index ) {
					// ****RESTART BLENDTS
					//blend_flag = 2;
					_ts_step = 1;
				}
			}
		}
		// Skip this part if _ts_step was recently reassigned to 1
		if ( _ts_step != 1 && ( _mode != INTERPOLATE_TIME && 
			_mode != INTERPOLATE_ALL ) ) {
			// We are not interpolating across time, which would 
			// make the following check irrelevant.

			// Check if cur_date is either on a data date in the 
			// _withdraw_ctl[i] DistributedTS (the SetWithdraw 
			// table) or if the cur_date and the _prev_date 
			// straddle a data date.

			// Get prev, which will be equal to cur_date if cur_date
			// is a defining point in the SetWithdraw table, or 
			// which will be equal to the date previous to cur_date
			// in the table.
			_withdraw_ctl[i].getPrevNextDates( cur_date, &prev, 
				&next );

			// Must manually set the years on the previous dates
			// because these are coming from a relative time series
			// i.e. no years are set.
			prev.setYear( cur_date.getYear() );
			if ( prev > cur_date ) {
				// prev and curr_date are straddling the year's
				// end
				prev.addYear( -1 );
			}

			// Check to see if blending conditions are met
			if ( (prev == cur_date) || 
				( prev_date < prev ) ) {
				// cur_date is on a date defining the 
				// setWithdraw table, meaning current withdrawal
				// is different from before.
				// OR
				// Time period from the last solution to the
				// current solution stradles a defining point on
				// the SetWithdraw table
				// ****RESTART BLENDTS
				_ts_step = 1;
			}
		}
	   }

	   // We now have the appropriate _ts_step.  Perform the BLENDTS

	   // Calculate the difference over which to blend
	   deviate = target_value - prev_withdraw;

	   // Calculate the blended values
	   with_value = prev_withdraw + ( deviate / 
		( _n_blend_ts - _ts_step + 1 ) );

	   // Increment the counter of blend timesteps to equal one
	   // more than the number already done (so it is ready for
	   // the next round.)
           _ts_step++;
	}

	// Check for BLENDTBL and determine types to do.
	else if ( _n_blend_tbl > 1 ) {
		// Determine if we have any NEW blending conditions
		// blend_flags are set:	0 = no NEW blend required
		//		1 = NEW blend due to TIME change only
		//		2 = NEW blend due to ELEV change only
		//		3 = NEW blend due to ELEV & TIME change
		//		4 = Continuation of old blend, reasons unknown.
		// All non-zero blend_flags are handled identically, but are
		//	assigned to maintain knowledge of source of BLENDTBL
		if ( _mode != INTERPOLATE_ELEV && _mode != INTERPOLATE_ALL ) {
			// We are not interpolating across elevations, which
			// would make the following check irrelevant.

			// Check if we have changed an elevation column.  We are
			// solving for withdrawal at cur_date, so the present 
			// pool value is the value solved last timestep.  We 
			// need to know the pool value prior to that, which can
			// be found in the timeseries at 2 timesteps before 
			// cur_date.
			TSDate two_prior = cur_date;
			TSDate modStart =  Method::getForecastDate1();
			two_prior.addInterval( getTimeInterval(), 
				-2*getTimeMult() );
			if ( two_prior < modStart ) {
				// We need to access pool values from the data
				// that came from reservoir carryover
				if ( cur_date == modStart ) {
					// First solution timestep
					prev_pool = _owner->_prevPoolCO;
				}
				else {
					// Second solution timestep
					prev_pool = _owner->_poolCO;
				}
			}
			else {
				prev_pool = _owner->_pool_ts->
					getDataValue( two_prior );
			}
			if( prev_pool == MISSING ) {
				PrintWarning( 1, routine, "Pool value missing "
					"for %s used to check a change in "
					"setWithdraw %s table elevation "
					"condition.  Beginning new BLENDTBL as "
					"if it had. ", getID(), 
					two_prior.toString() );
				blend_flag = 2;
				_tbl_step = 1;
			}
			else {
				for( j = _n_elev - 1; j >= 0; j-- ) {
					if( prev_pool >= _elev[j] || j == 0 ) {
						pp_index = j;
						break;
					}
				}
				// if pp_index != i then we have changed our 
				// elevation index in the control file time 
				// series.  
				if( i != pp_index ) {
					blend_flag = 2;
					_tbl_step = 1;
				}
			}
		}
		// If we decided we didn't care why we are starting a new BLENDTBL,
		// we could skip this part if blend_flag != 0.  It would improve
		// runtime.  Currently, it is coded to continue with time check,
		// allowing us to know (for some future case) all the reasons 
		// why we are starting a new blend.
		if ( _mode != INTERPOLATE_TIME && _mode != INTERPOLATE_ALL ) {
			// We are not interpolating across time, which would 
			// make the following check irrelevant.

			// Check if cur_date is either on a data date in the 
			// _withdraw_ctl[i] DistributedTS (the SetWithdraw 
			// table) or if the cur_date and the _prev_date 
			// straddle a data date.

			// Get prev, which will be equal to cur_date if cur_date
			// is a defining point in the SetWithdraw table, or 
			// which will be equal to the date previous to cur_date
			// in the table.
			_withdraw_ctl[i].getPrevNextDates( cur_date, &prev, 
				&next );

			// Must manually set the years on the previous dates
			// because these are coming from a relative time series
			// i.e. no years are set.
			prev.setYear( cur_date.getYear() );
			if ( prev > cur_date ) {
				// prev and curr_date are straddling the year's
				// end
				prev.addYear( -1 );
			}

			// Check to see if blending conditions are met
			if ( prev == cur_date ) {
				// cur_date is on a date defining the 
				// setWithdraw table, meaning current withdraw 
				// is different from before.
				if ( blend_flag !=0 ) {
					// Elevation blending is also set to go
					blend_flag = 3;
					_tbl_step = 1;
				}
				else {
					// Only blend through TIME
					blend_flag = 1;
					_tbl_step = 1;
				}
			}
			else if ( prev_date < prev ) {
				// Time period from the last solution to the
				// current solution stradles a defining point on
				// the SetWithdraw table
				if ( blend_flag !=0 ) {
					// Elevation blending is also set to go
					blend_flag = 3;
					_tbl_step = 1;
				}
				else {
					// Only blend through TIME
					blend_flag = 1;
					_tbl_step = 1;
				}
			}
		}
		
		// Handle blend_flags
		if ( blend_flag == 0 ) {
			// No new blending requirements
			// Are we in the middle of a previously prescribed
			// blend?
			if( (_tbl_step > 1) && (_tbl_step <= _n_blend_tbl) ) {
				// We are in the middle of a blend.  While we 
				// don't know what type of blend we were in, any
				// value larger than 0 will cause us to cycle 
				// through the blend calculation.
				blend_flag = 4;
			}
			else {
				// We are not in the middle of a blend.
				// Assign unblended value derived from the
				// table
				with_value = target_value;
			}
		}
		if ( blend_flag >= 1 ) {
			// Calculate the difference over which to blend
			deviate = target_value - prev_withdraw;	
			
			// Calculate the blended values
			with_value = prev_withdraw + deviate / 
				( _n_blend_tbl - _tbl_step + 1 ); 

			// Increment the counter of blend timesteps to equal one
			// more than the number already done (so it is ready for
			// the next round.)
			_tbl_step++;
		}
	}

	// No BLENDTS nor BLENDTBL work done.  Assign target value from table as 
	// withdraw
	else {
		with_value = target_value;
	}

	if( with_value == MISSING ) {
		// This should not happen, but . . .
		PrintWarning( 1, routine, "Value returned from a SetWithdraw"
			" time series is MISSING - Setting to 0." );
		with_value = 0.0;
	}

	// The following value for pool is not the state variable _pool.
	//	Nevertheless, use of pool as _pool would likely go against
	//	documented procedure.  Currently, 6/19/01, no usage of 
	//	pool is made, except to assign itself to also-unused 
	//	group_val[1], below.
	/**** REMOVED despite above explanation
	//pool = _owner->_elev_stor_tbl.lookup( ( _owner->_storage - 
		//with_value * Method :: getTimeStepSec() ),
		//GETCOLUMN_1, ALLOW_BOUNDS );
	****/

	_myValue = with_value;
	if( !group_val ) {
		_owner->_withdraw = with_value;

		/**** REMOVED
		// Recalculate the working copy of the pool...
		//_owner->_pool = _owner->_elev_stor_tbl.lookup(
			// ( _owner->_storage + with_value*Method :: 
			// getTimeStepSec() ), GETCOLUMN_1, ALLOW_BOUNDS );
		****/
	}
	else {
		*group_val[0] = with_value;
		/**** REMOVED
		//*group_val[1] = pool;	// Doesn't appear to be used anywhere
		****/
		return( STATUS_SUCCESS );
	}

	PrintDebug( 5, routine, "Setting outflow for %s to %f on %s.",
		_owner->_id, with_value, cur_date.toString() );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetWithdraw_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: SetWithdraw_solveMethod.cxx,v 1.8 2006/10/26 15:35:26 hsu Exp $";}
/*  ===================================================  */

}
