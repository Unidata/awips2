//------------------------------------------------------------------------------
// Lookup3 :: solveMethod - Solver for the Lookup3 method.
//------------------------------------------------------------------------------
// History:
//
// 01 Feb 2006  James R. VanShaar, RTi  Created initial version
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     HourTS::getDataValue()
//     getType()
//     TSDate::toString()
//     TSDate::toNoYearJulianDouble()
//     Expression::evaluate()
//     ReferencedMatrix::getDataValue()
//     TSDate::getWeekday()
//     restartBlend()
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword WEEKLYVARIATION and related parameters including advancing the row
 counter.
@return int value signifying success
@param cur_date TSDate of current solution time step
@param isPrimarySolution int flag referencing whether this is part of a cloned
 subset of the greater RES-J system.
@param group_val double array for storage of solution results when this method
 is part of a Combo-type method
*/
#include "Lookup3.h"

int Lookup3 :: solveMethod( TSDate& cur_date, int isPrimarySolution,
    double** group_val )
{
    char routine[] = "Lookup3 :: solveMethod";
    double with_value = MISSING, blend_with, prev_pool,
        pool, target_value, obs_value = MISSING,
        prev_withdraw = MISSING, prev_myValue;
    int i, j, pp_index, blend_flag = 0, blend_ts_flag = 0;
    TSDate prev, next, prev_date;

    double ts_val1, ts_val2;

    // Set _Active to 1
    _Active = 1;
    prev_myValue = _myValue;

    // Before entering all sorts of calculations, check to see if an observed
    // time series (or maybe two) exists with non-missing value(s).  If it does,
    // quickly return the appropriate value.
    if( _tableVar == TVAR_RELEASE )
    {
        // Release-type.  Check _release_obs time series
        if( _release_obs )
        {
            // Get the current time step value and check for < 0.
            // Negative values are treated as missing!
            ts_val1 = _release_obs->getDataValue( cur_date );
            if( ts_val1 >= 0 )
            {
                _ts_step = 1;
                // The value will be ts_val1.
                // Where it gets set depends on who (a ComboMethod or a
                // non-Combo method) called this function.
                _myValue = ts_val1;
                if( !group_val )
                {
                    ((Reservoir *)_owner)->_release = ts_val1;
                }
                else {
                    *group_val[0] = ts_val1;
                }

                _lastValKnown = 1;

                return( STATUS_SUCCESS );
            }
            // Otherwise, there is no acceptable observed release.
            // Work with the table.
        }
        // Otherwise, there is no acceptable observed release.
        // Work with the table.
    }
    // This is a Withdrawal, Diversion or Augmentation parameterization.
    // Check to see if an observed time series exists for at least one of these.
    else if ( _withdraw_obs || _diversion_obs || _augment_obs )
    {
        // Look at withdrawal and augmentation or diversion and augmentation.
        // Directions (signs) of each time series will be handled at the end.

        int checkTwo = -1;
        ts_val1 = 0;    // withdrawal or diversions
        ts_val2 = 0;    // augmentation

        // Start with the withdrawal (or diversion) time series
        if( _withdraw_obs || _diversion_obs )
        {
            // Get the current time step value.
            if( _tableVar == TVAR_WITHDRAWAL )
            {
                ts_val1 = _withdraw_obs->getDataValue( cur_date );
            }
            else if( _tableVar == TVAR_DIVERSION )
            {
                ts_val1 = _diversion_obs->getDataValue( cur_date );
            }
            else
            {
                // This is an augmentation type.
                // The owning component defines whether withdrawal or diversion
                // is appropriate.
                if( _ownerType == CM_RESERVOIR )
                {
                    // Reservoir.  Look at withdrawal.
                    ts_val1 = _withdraw_obs->getDataValue( cur_date );
                }
                else
                {
                    // Node.  Look at diversion.
                    ts_val1 = _diversion_obs->getDataValue( cur_date );
                }
            }

            // By now the initial ts_val1 value of 0 has been replaced.
            // Check for < 0.  Negative values are treated as missing!
            if( ts_val1 >= 0 )
            {
                // The value is acceptable.

                // The augmentation time series (if it exists) still needs to
                // be checked, but the first check has passed.
                checkTwo = 1;
            }
            else
            {
                // Because a time series exists, without an acceptable
                // withdrawal, ignore any other observed time series.
                checkTwo = 0;
            }
        }
        // Otherwise, there is no withdrawal or diversion time series.

        // If it has not already been determined that an observed time series
        // will not be used (that is checkTwo != 0), the augmentation time
        // series (if it exists) will be considered.
        if( checkTwo && _augment_obs )
        {
            // Get the current time step value and check for < 0.
            // Treat any negative values as missing!
            ts_val2 = _augment_obs->getDataValue( cur_date );
            if( ts_val2 >= 0 )
            {
                // The value is acceptable.

                // The second check has passed.
                checkTwo = 2;
            }
            else
            {
                // Because a time series exists, without an acceptable
                // augmentation, ignore any other observed time series.
                checkTwo = 0;
            }
        }

        // If it has not already been determined that an observed time series
        // will not be used (that is checkTwo > 0), proceed to assign the
        // value. In reality, execution would not be in this loop if checkTwo
        // remained at -1 to this point in the loop.

        // The resulting variable will be a withdrawal or diversion.

        // Both withdrawal (diversion) and augmentation were initialized to 0
        // and modified according to the values on the time series (if the time
        // series existed).
        if( checkTwo > 0 )
        {
            _ts_step = 1;
            // When execution reaches here, strictly working with observed time
            // series, the _tableVar (TABLEVAR) value is unimportant.  The value
            //  will be placed on the Reservoir _withdraw variable.
            // Therefore, independent of the Lookup3 table type:
            //     ts_val1 will be treated as positive (added), and
            //     ts_val2 will be treated as negative (subtracted).
            _myValue = ts_val1 - ts_val2;

            // Where this value goes depends on who (a ComboMethod or a
            // non-Combo method) called this function.
            if( !group_val )
            {
                // Put it directly on the appropriate variable.
                if( _ownerType == CM_RESERVOIR )
                {
                    // The owner is a reservoir
                    ((Reservoir *)_owner)->_withdraw = _myValue;
                }
                else
                {
                    // The owner is a node
                    ((Node *)_owner)->_diversion = _myValue;
                }
            }
            else {
                // Pass the value up to a Combo method.

                // Any signs (diversion or withdrawal vs. augmentation) were
                // already addressed.
                // The Combo method will take care of the rest.
                *group_val[0] = _myValue;
            }

            _lastValKnown = 1;

            return( STATUS_SUCCESS );
        }
        // Otherwise, look to the table for withdrawal (diversion) or
        // augmentation.
    }

    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    // If execution reaches this point the table will be used.
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------

    // Get the column indexing value
    double cur_colVal;
    if( _colVar == INDEX_DATE )
    {
        cur_colVal = cur_date.toNoYearJulianDouble();
    }
    else if( _colVar == INDEX_TS )
    {
        cur_colVal = _colIndex->getDataValue( cur_date );
        // The assumption is that even negative values (and thus MISSING values
        // are desired.  There is no further check for irregular / irrational
        // values out.
    }
    else
    {
        // No need to check for _colVar == INDEX_COMPSTATE as this was done
        // during intialization.
        cur_colVal = _colExpr->evaluate();
    }

    // Get the row indexing value
    double cur_rowVal;
    if( _rowVar == INDEX_DATE )
    {
        cur_rowVal = cur_date.toNoYearJulianDouble();
    }
    else if( _rowVar == INDEX_TS )
    {
        cur_rowVal = _rowIndex->getDataValue( cur_date );
        // The assumption is that even negative values (and thus MISSING values
        // are desired.  There is no further check for irregular / irrational
        // values out.
    }
    else
    {
        // No need to check for _colVar == INDEX_COMPSTATE as this was done
        // during intialization.
        cur_rowVal = _rowExpr->evaluate();
    }

    // Now get the target value from the table.  Also get index references
    // for comparison, if necessary, in the blending sections.
    int rowI, colI;
    target_value = _table.getDataValue(
        cur_rowVal, cur_colVal, _mode, &rowI, &colI );

    // Note: SetWithdraw and SetRelease have "a modified algorithm for the POOLQ
    //       constant discharge approach.
    //       Due to the flexible nature of Lookup3, it was determined that
    //       replication of the algorithm for the rare case when it is
    //       applicable was unwise.  There are too many scenarios where it
    //       would not be applicable to spend time on it.  Besides, if the
    //       user really wants it, it is available for the appropriate
    //       scenario within the SetWithdraw or SetRelease methods.

    // Address weekly variation, if necessary.
    if( _inWeekScalars ) {
        // Determine which day of the week the solution time step is.
        int weekday=cur_date.getWeekday( 1 );

        // Now determine which timestep of the day the solution time step is.
        // Use integer division.
        // Therefore if simulation is at a 6 hour time step and current time
        // step hour is 5 or less, then it is weekly time step 0.  If simulation
        // is within hours 6 - 11, then it is weekly time step 1. Etc.
        int dayStep=cur_date.getHour() / _t_mult;

        // Now determine which scalar
        i = weekday * ( 24 / _t_mult ) + dayStep;

        // Scale
        target_value *= _inWeekScalars[i];
    }

    // By the time execution reaches this point, the target value has been
    // defined by whatever method is appropriate.
    // Now determine if this target value is the final goal, or if blending
    // toward the target goal is necessary; in essence, reduce our target by
    // some portion, for the current solution.


    // Check for BLENDTS and knowledge of the table indexes from last time step.
    // NOTE: This index check is different from SetRelease and SetWithdraw.
    //       It is necessary because the indexing variables from last time step
    //       are not necessarily available this time step.  (In SetRelease and
    //       SetWithdrawal, they are DATE and always-calculated reservoir pool.
    //if ( _n_blend_ts && _iValKnown )
    if ( _n_blend_ts )
    {
        // BLENDTS is defined, and this method was active last time step.
        // If execution reaches here it is already known that the datum is
        // missing.

        // Is either the method in the middle of a blend or should it start a
        // new one?
        if( _ts_step <= _n_blend_ts ) {
            // Yes, perform timeseries blending using the target value
            blend_ts_flag = 1;
        }
        else {
            // No, the BLENDTS is complete.
            // Don't set the blend_ts_flag, but BLENDTBL may yet be used, if
            // defined.
        }
    }
    // Otherwise, there is no BLENDTS or the method wasn't active last time step
    // so a blend from last step does not make sense.

    // If BLENDTS is required, do so now
    if ( blend_ts_flag && _lastValKnown )
    {
        // Is the method in the middle of an ongoing blend?
        // NOTE: If _ts_step != 1 then _iValKnown = 1 also.
        if( _ts_step != 1 )
        {
            // Yes, a BLENDTS is ongoing.

            // Should the blend be restarted?
            if( restartBlend( rowI, colI ) )
            {
                // The table target has shifted across a non-interpolated
                // boundary.
                // Restart the blend.
                _ts_step = 1;
            }
        }

        // The appropriate _ts_step has been determined.  Perform the BLENDTS

        // Calculate the difference over which to blend
        double deviate = target_value - prev_myValue;

        // Calculate the blended values
        _myValue = prev_myValue + ( deviate / ( _n_blend_ts - _ts_step + 1 ) );

        // Increment the counter of blend timesteps to equal one more than the
        // number already done (so it is ready for the next round.)
        _ts_step++;
    }
    // Otherwise, look at BLENDTBL and knowledge of the table indexes from last
    // time step.
    // NOTE: This index check is different from SetRelease and SetWithdraw.
    //       It is necessary because the indexing variables from last time step
    //       are not necessarily available this time step.  (In SetRelease and
    //       SetWithdrawal, they are DATE and always-calculated reservoir pool.
    else if ( _n_blend_tbl && _iValKnown && _lastValKnown )
    {
        int blend_flag = 0;

        // Should the blend be restarted?
        if( restartBlend( rowI, colI ) )
        {
            // The table target has shifted across a non-interpolated
            // boundary.
            // Restart the blend.
            _tbl_step = 1;
            blend_flag = 1;
        }

        // Handle blend_flags
        if ( blend_flag == 0 ) {
            // No new blending requirements.

            // Is the method in the middle of an ongoing blend?
            if( (_tbl_step > 1) && (_tbl_step <= _n_blend_tbl) ) {
                // Yes, a blend is ongoing.
                // At this point, execution does not know what type of blend is
                // occurring, however, any value larger than 0 will cause us to
                // cycle through the blend calculation.
                blend_flag = 4;
            }
            else {
                // No, a blend is not ongoing.
                // Assign unblended value derived from the table
                _myValue = target_value;
            }
        }
        if ( blend_flag >= 1 ) {
            // Calculate the difference over which to blend
            double deviate = target_value - prev_myValue;

            // Calculate the blended values
            _myValue = prev_myValue + deviate / ( _n_blend_tbl - _tbl_step + 1 );

            // Increment the counter of blend timesteps to equal one
            // more than the number already done (so it is ready for
            // the next round.)
            _tbl_step++;
        }
    }
    // Otherwise, no BLENDTS nor BLENDTBL work done.
    // Assign target value from table as _myValue
    else {
        _myValue = target_value;
    }

    // If execution reaches here, the value being used came from the table (even
    // if it was subsequently modified in some fashion).
    if( !group_val ) {
        if( _ownerType == CM_RESERVOIR )
        {
            if( _tableVar == TVAR_RELEASE )
            {
                ((Reservoir *)_owner)->_release = _myValue;
            }
            else if( _tableVar == TVAR_WITHDRAWAL )
            {
                ((Reservoir *)_owner)->_withdraw = _myValue;
            }
            else
            {
                // The only other option is an augmentation.
                // It is handled internally as a negative withdrawal.
                ((Reservoir *)_owner)->_withdraw = - _myValue;
            }
        }
        else
        {
            // The owner is a NODE
            if( _tableVar == TVAR_DIVERSION )
            {
                ((Node *)_owner)->_diversion = _myValue;
            }
            else
            {
                // The only other option is an augmentation.
                // It is handled internally as a negative diversion.
                ((Node *)_owner)->_diversion = - _myValue;
            }
        }
    }
    else {
        // The sign of any AUGMENTATION needs to reversed.
        // Beyond that, the Combo Method above will decifer how to handle it.
        if( _tableVar == TVAR_AUGMENTATION )
        {
            *group_val[0] = - _myValue;
        }
        else
        {
            *group_val[0] = _myValue;
        }
    }

    // Now set up the index trackers for next time
    _iValKnown = 1;
    _colI = colI;
    _rowI = rowI;

    _lastValKnown = 1;

    return( STATUS_SUCCESS );
}

//------------------------------------------------------------------------------
// Lookup :: restartBlend - Determines whether blending algorithms should be
//                          restarted.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying the need to restart blending algoritms
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Determines whether blending algorithms should be restarted.
@return int value signifying need to restart (0=no, 1=yes due to columns, 2=yes
 due to rows)
@param rowI int index of previous solution row
@param colI int index of previous solution row
*/
int Lookup3::restartBlend( int rowI, int colI )
{

    // Restart the blend if a non-interpolated index threshold is crossed in the
    // table.

    // Check for columns.
    // They must not be interpolated.
    if ( _mode != INTERPOLATE_COLS && _mode != INTERPOLATE_ALL )
    {
        // No interpolation across columns.

        // Compare last column indexing value against the current column
        // indexing integer
        if( _colI != colI )
        {
            // Start a new blend.
            return 1;
        }
    }

    // Check for rows.
    // They must not be interpolated.
    if ( _mode != INTERPOLATE_ROWS && _mode != INTERPOLATE_ALL )
    {
        // No interpolation across rows.

        // Compare last row indexing value against the current row indexing
        // integer
        if( _rowI != rowI )
        {
            // Start a new blend.
            return 2;
        }
    }

    // No need to restart blending
    return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Lookup3_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: Lookup3_solveMethod.cxx,v 1.1 2006/10/26 15:25:25 hsu Exp $";}
/*  ===================================================  */

}
