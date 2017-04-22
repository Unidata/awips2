//------------------------------------------------------------------------------
// Reservoir setStates - routine initializes and reads data into all of the
//			state variable Time series for the reservoir object.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Apr 1998  Daniel Weiler, RTi	Created initial version and broke out 
//					from Reservoir set/get.
// 22 Sep 1998	DKW			Revised getOuputTS to be a virtual 
//					recursive function.
// 13 Nov 2001	James R. VanShaar, RTi	Added setCOstring(TSDate&), moving and 
//					updating similar code from 
//					Reservoir::finalizeSolution
// 13 Nov 2001	JRV, RTi	Added setCOstring().
// 05 Jun 2002	JRV, RTi	Added setEndInflow( TSDate& ).
// 06 Jun 2002	JRV, RTi	Expanded getInternalValuePtr to work with 
//				ENDINGINFLOW, STARTINGINFLOW, PREVIOUSINFLOW,
//				ENDINGPOOL, STARTINGPOOL, PREVIOUSPOOL,
//				ENDINGWITHDRAW, STARTINGWITHDRAW, 
//				PREVIOUSWITHDRAW, ENDINGRELEASE, 
//				STARTINGRELEASE, and PREVIOUSRELEASE.
// 06 Jun 2002  JRV, RTi        Created initial version of
//                              setEndOfTimestepStates by copying stuff from
//                              Component::setEndOfTimestepStates().
// 12 Jun 2002	JRV, RTi	Added _prevStorage, _startStorage, _endStorage
// 14 Oct 2002	JRV, RTi	Added logic for ADJUST method ADJSIM OFF mode to
// 				setEndOfTimestepStates( TSDate& ).
// 14 Oct 2002	JRV, RTi	Revised logic for setCOstring( TSDate& ) to take
// 				advantage of _prev* and _start* states.
// 30 Jan 2002	JRV, RTi	Revised sendToTS to clarify variable meaning.
// 12 Jul 2003	JRV, RTi	Added _release to MISSING in 
// 				setEndOfTimestepStates for Spillway bug fix.
// 19 Feb 2004	JRV, RTi	Modified setEndInflow and added an overloaded
// 				version of it.
// 27 Mar 2006  JRV, RTi    Added valueToMethod( TSDate ),
//                          valueToMethod( TSDate, double ) and removed
//                          valueToMethod ( TSDate, double, double )
//                          double).
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "Reservoir.h"
#include "TSList.h"
#include "Adjust.h"

double* Reservoir::getInternalValuePtr( char* name )
{
	char temp[MAXC];
	char routine[]="Reservoir :: getInternalValuePtr";

	// It must be ensured that the variable is in capitals
	ToUpper( name );
	
	// The following is ordered as estimated to be most efficient.
	if( NULL != strstr( name, "POOL" ) ) {
		if( !strcmp( name, "STARTINGPOOL" ) ) {
			return( &_startPool );
		}
		if( !strcmp( name, "ENDINGPOOL" ) ) {
			return( &_endPool );
		}
		if( !strcmp( name, "PREVIOUSPOOL" ) ) {
			return( &_prevPool );
		}
		if( !strcmp( name, "POOL" ) ) {
			sprintf( temp, "**NOTE** Use of \"POOL\" in RULES is "
				"not recommended.  Consider \"STARTINGPOOL\" "
				"or \"ENDINGPOOL\"" );
			int length = strlen( temp );
			int ipr = ResJSys::getIPR(); 
			ResJ_ccwrite( &length, temp, &ipr );
			return( &_startPool );
		}
	}
	if( NULL != strstr( name, "INFLOW" ) ) {
		if( !strcmp( name, "ENDINGINFLOW" ) ) {
			return( &_endInflow );
		}
		if( !strcmp( name, "STARTINGINFLOW" ) ) {
			return( &_startInflow );
		}
		if( !strcmp( name, "PREVIOUSINFLOW" ) ) {
			return( &_prevInflow );
		}
	}
	if( NULL != strstr( name, "RELEASE" ) ) {
		if( !strcmp( name, "STARTINGRELEASE" ) ) {
			return( &_startRelease );
		}
		if( !strcmp( name, "ENDINGRELEASE" ) ) {
			return( &_endRelease );
		}
		if( !strcmp( name, "PREVIOUSRELEASE" ) ) {
			return( &_prevRelease );
		}
		if( !strcmp( name, "RELEASE" ) ) {
			sprintf( temp, "**NOTE** Use of \"RELEASE\" in RULES "
				"is not recommended.  Consider "
				"\"STARTINGRELEASE\" or \"ENDINGRELEASE\"" );
			int length = strlen( temp );
			int ipr = ResJSys::getIPR(); 
			ResJ_ccwrite( &length, temp, &ipr );
			return( &_startRelease );
		}
	}
	if( NULL != strstr( name, "WITHDRAW" ) ) {
		if( !strcmp( name, "ENDINGWITHDRAW" ) ) {
			return( &_endWithdraw );
		}
		if( !strcmp( name, "STARTINGWITHDRAW" ) ) {
			return( &_startWithdraw );
		}
		if( !strcmp( name, "PREVIOUSWITHDRAW" ) ) {
			return( &_prevWithdraw );
		}
	}

	return( NULL ); 
}

// Used for initialization of the CO string as called from Reservoir::setStates
// Results will ultimately size the CO array for use by NWSRFS.
int Reservoir :: setCOstring( )
{
	char routine[]="Reservoir::setCOstring";
	double prev_inflow, prev_pool, prev_withdraw, prev_release = 0.0, prev_spill = 0.0;
	char future[9]="*FUTURE*";
	char rel_str[9], pool_str[9], with_str[9], inflow_str[9], 
		prevInflow_str[9], prevPool_str[9], prevRel_str[9], 
		prevWithdraw_str[9], future_str[81], value_str[65];
	char temp_str[28+64+80+1];	// Labels, values, future space, end

	// Get current values (values at end of the current time step)
	sprintf( rel_str, "%f", _release );
	sprintf( pool_str, "%f", _pool );
	sprintf( with_str, "%f", _withdraw );
	sprintf( inflow_str, "%f", _inflowCO );

	// Get previous-type values (values at end of the previous of time step)
	sprintf( prevRel_str, "%f", _prevReleaseCO );
	sprintf( prevPool_str, "%f", _prevPoolCO );
	sprintf( prevWithdraw_str, "%f", _prevWithdrawCO );
	sprintf( prevInflow_str, "%f", _prevInflowCO );

	// Prepare value portion of the string
	sprintf(value_str, "%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s",
		rel_str, pool_str, with_str, inflow_str, prevRel_str, 
		prevPool_str, prevWithdraw_str, prevInflow_str);

	// Prepare future place holders
	sprintf(future_str, 
		"%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s", future,
		future,future,future,future,future,future,future,future,future);

	// Prepare the full variable string
	sprintf( temp_str, 
		"%-12.12s%-12.12s%4.4s%64.64s%80.80s",
		_type, _id, "-999", value_str, future_str );

	// Write the string to the system CO string
	if( ResJSys::addCOString( temp_str ) ) {
		PrintWarning( 1, routine, 
			"Troubles adding carryover data to "
			"CO array." );
		return( STATUS_FAILURE );
	}
	
	return( STATUS_SUCCESS );
}

int Reservoir :: setCOstring( TSDate& cur_date )
{
// Preparation of the CO array during model simulation.  Results will ultimately
//	fill an existing CO array prepared by NWSRFS.  It is expected that this
//	is called from setEndOfTimestepStates( TSDate& date ), after the states
//	are shifted in preparation for the next time step.

	char routine[]="Reservoir::setCOstring";
	double prev_inflow, prev_pool, prev_withdraw, prev_release = 0.0;
	char future[9]="*FUTURE*";
	char rel_str[9], pool_str[9], with_str[9], inflow_str[9], 
		prevInflow_str[9], prevPool_str[9], prevRel_str[9], 
		prevWithdraw_str[9], future_str[81], value_str[65];
	char temp_str[52+64+80];	// Orig, enhanced, future space

	// The intention of recently solved for and previous values is to 
	// capture the state at the end of this solution time step plus the 
	// values at the end of the previous time step (or beginning of this 
	// one).
	// When these states are used in initializing a run, those recorded
	// here as recently solved for values will become the condition at the 
	// beginning of the first time step.  The previous values here will 
	// represent one time step prior to that.

	// Get recently solved for values (values at end of the current time 
	//	step)
	sprintf( rel_str, "%f", _startRelease );
	sprintf( pool_str, "%f", _startPool );
	sprintf( with_str, "%f", _startWithdraw );
	sprintf( inflow_str, "%f", _startInflow );

	/***************
	// Get recently solved for values (values at end of the current time 
	//	step)
	sprintf( rel_str, "%f", _release );
	sprintf( pool_str, "%f", _pool );
	sprintf( with_str, "%f", _withdraw );
	sprintf( inflow_str, "%f", getTotalInflow( cur_date ) );	

	// Prepare previous values, if not immediately available
	if ( _prev_date < Method::getForecastDate1() ) {
		prev_release = _releaseCO;
		prev_spill = _spill_ts->getDataValue( _prev_date );
		prev_pool = _poolCO;
		prev_withdraw = _withdrawCO;
		// Need to access from carryover
		// prev_inflow
		if ( _inflowCO == MISSING ) {
			TSDate t1 = Method::getForecastDate1();
			prev_inflow = getTotalInflow( t1 );
		}
		else {
			prev_inflow = _inflowCO;
		}
	}
	else {
		// access from timeseries
		prev_release = _release_ts->getDataValue( _prev_date );
		prev_pool = _pool_ts->getDataValue( _prev_date );
		prev_withdraw = _withdraw_ts->getDataValue( _prev_date );
		prev_inflow = getTotalInflow( _prev_date );
	}

	// Get previous-type values (values at end of the previous of time step)
	sprintf( prevRel_str, "%f", prev_release );
	sprintf( prevPool_str, "%f", prev_pool );
	sprintf( prevWithdraw_str, "%f", prev_withdraw );
	sprintf( prevInflow_str, "%f", prev_inflow );
	***************/

	// Get previous-type values (values at end of the previous of time step)
	sprintf( prevRel_str, "%f", _prevRelease );
	sprintf( prevPool_str, "%f", _prevPool );
	sprintf( prevWithdraw_str, "%f", _prevWithdraw );
	sprintf( prevInflow_str, "%f", _prevInflow );

	// Prepare value portion of the string
	sprintf(value_str, "%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s",
		rel_str, pool_str, with_str, inflow_str, prevRel_str, 
		prevPool_str, prevWithdraw_str, prevInflow_str);

	// Prepare future place holders
	sprintf(future_str, 
		"%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s", future,
		future,future,future,future,future,future,future,future,future);

	// Prepare the full variable string
	/******
	sprintf( temp_str, "%-12.12s%-12.12s%4.4s%8.8s%8.8s%8.8s",
		_type, _id, "-999", rel_str, pool_str, with_str );
	******/
	sprintf( temp_str, 
		"%-12.12s%-12.12s%4.4s%64.64s%80.80s",
		_type, _id, "-999", value_str, future_str );

	// Write the string to the system CO string
	if( ResJSys::addCOString( temp_str ) ) {
		PrintWarning( 1, routine, 
			"Troubles adding carryover data to "
			"CO array." );
		return( STATUS_FAILURE );
	}
	
	return( STATUS_SUCCESS );

}


// Sums inflows at the end of the current timestep and sets the value of 
// _endInflow.
void Reservoir :: setEndInflow( TSDate& cur_date )
{
	_endInflow = sumInflow(cur_date);
	_totalInflow.setDataValue( cur_date, _endInflow );
	return;
}

void Reservoir :: setEndInflow ( TSDate& date, double value )
{
	_endInflow = value;
	_totalInflow.setDataValue( date, value );
	return;
}

// Reset states for next timestep and output carryover, if necessary.
int Reservoir :: setEndOfTimestepStates( TSDate& date )
{
	char	routine[]="Reservoir :: setEndOfTimestepStates";
	int	i=0, adjsim = 1;
	Method *currMethod = NULL;
	Adjust *adjMethod = NULL;

	// We have to recursively call the function on the 
	// sons...
	for( i=0; i<_n_son; i++ ){
		if( _son_list[i]->setEndOfTimestepStates( date ) ){
			return( STATUS_FAILURE );
		}
	}

	// Shift PREVIOUS states
	_prevInflow = _startInflow;
	_prevPool = _startPool;
	_prevRelease = _startRelease;
	_prevWithdraw = _startWithdraw;
	_prevStorage = _startStorage;

	// Shift STARTING states
	_startInflow = _endInflow;
	_startPool = _endPool;
	_startRelease = _endRelease;
	_startWithdraw = _endWithdraw;
	_startStorage = _endStorage;

	// Reinitialize ENDING states
	_endInflow = MISSING;
	_endPool = MISSING;
	_endRelease = MISSING;
	_endWithdraw = MISSING;
	_endStorage = MISSING;

	// Reset loss
	_loss = 0.0;

	// Check for the need to write carryover
	if( _is_co_date ) {
		// Determine if any ADJUST methods with ADJSIM OFF 
		// parameterization exist for this reservoir.  We begin at the 
		// end of the list because any ADJUST method is likely to be 
		// parameterized there.
		i = _n_meth - 1;
		while (i >= 0 && adjsim) {
			currMethod = _method_list[i];
			if( !strcasecmp(currMethod->getType(),"ADJUST")) {
				adjMethod = (Adjust *)currMethod;
				// Check to ensure that the method has been
				// activated.  It was initialized as inactive,
				// but will have been activated if its 
				// conditional expression had ever evaluated to
				// true.  After activation, inactivation is
				// prevented.
				// This check, therefore, is for a method which
				// was parameterized but not included in the
				// rules section.
				if( adjMethod->_Active ) {
					adjsim = adjMethod->adjsim();
				}
			}
			i--;
		}
		if( !adjsim ) {
			// Call the Adjust method's handling for the
			// Reservoir's states.
			adjMethod->setCOstring_AdjsimOff( date );
		}
		else {
			setCOstring(date);
		}
		// write method carryover
		for( i=0; i<_n_meth; i++ ) {
			if( _method_list[i]->_hasStates ) {
				_method_list[i]->setCOstring(date);
			}
		}
	}

	// Now that we have completed the carryover business (if necessary)
	// proceed with final re-initialization
	_release = MISSING;	// Required for possible lone Spillway method.

	// Set the previous date on this component for use at the next time 
	//	step.
	setPrevDate( date );
	
	return( STATUS_SUCCESS );
}

//------------------------------------------------------------------------------
// Reservoir :: valueToMethod - Handles any withdrawal transfer to components
//                              when no reduction of withdrawal is necessary.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     Integer defining successful construction of the minimum remainder TS.
// Calls:
//     sendToTS( TSDate )
//     Method::setInactiveState( )
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Handles any withdrawal transfer to components when no reduction of withdrawal
 is necessary.
@return None
@param date TSDate of current time step.
*/

void Reservoir :: valueToMethod ( TSDate date )
{
    int i, foundActive = 0;
    Method *meth;

    // No reduction.  Each method can divert (transfer) as much as they
    // wanted.
    for( i=_n_meth-1; i>=0; i-- )
    {
        meth = _method_list[i];
        // Ensure the method is a "top level" method by checking for
        // an expression.  Also check to make sure we are working with
        // a withdrawal-type method (currently Lookup3 only).
        if( _expr_list[i] != NULL && meth->_group_id == WITHD_METHOD )
        {
            // If this method is active, then it was executed.
            if ( _method_list[i]->_Active && !foundActive )
            {
                // Transfer, if appropriate.
                _method_list[i]-> sendToTS( date );

                // We have found the last active method.
                foundActive = 1;
            }
            else if( foundActive )
            {
                // We already found the governing (last active) method.
                // Deactivate this method (and any under it if it is a
                // ComboMethod).
                // We do this to avoid bad carryover values for
                // INITIALTRANSFER on methods that were overwritten by
                // subsequent methods due to their execution order defined
                // by the RULES section.
                _method_list[i]->setInactiveState( );
            }
        }
    }
}

//------------------------------------------------------------------------------
// Reservoir :: valueToMethod - Handles any withdrawal transfer to components
//                              when reduction of withdrawal is necessary.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     Integer defining successful construction of the minimum remainder TS.
// Calls:
//     Methdo::getMyValueAsOut( double*, double* )
//     Method::setInactiveState( )
//     sendToTS( TSDate )
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Handles any withdrawal transfer to components when reduction of withdrawal
 is necessary.
@return None
@param date TSDate of current time step.
@param maxWith Double estimate of the maximum net withdrawal allowed by the
 Reservoir component to meet minimum pool requirements.
*/
void Reservoir :: valueToMethod ( TSDate date, double maxWith )
{
    int i, foundActive = 0;
    Method *meth;

    // We need to reduce the diversions.
    // To do this we need ti define specifically inflows (augmentations)
    // and outflows (diversions, withdrawals).
    double availIN = 0.0;
    double planOUT = 0.0;
    double methValIN, methValOUT;
    for( i=_n_meth-1; i>=0; i-- )
    {
        meth = _method_list[i];
        // Ensure the method is a "top level" method by checking for
        // an expression.  Also check to make sure we are working with
        // a withdrawal-type method (currently SetWithdraw and Lookup3
        // only).
        if( _expr_list[i] != NULL && meth->_group_id == WITHD_METHOD )
        {
            // If this method is active, then it was executed.
            if ( _method_list[i]->_Active && !foundActive )
            {
                // Request the method's value as a withdrawal
                methValIN = 0.0;
                methValOUT = 0.0;
                _method_list[i]->getMyValueAsOut( &methValIN, &methValOUT );
                availIN += methValIN;
                planOUT += methValOUT;

                // We have found the last active method.
                foundActive = 1;
            }
            else if( foundActive )
            {
                // We already found the governing (last active) method.
                // Deactivate the method (and any under it if it is a
                // ComboMethod).
                // We do this to avoid bad carryover values for
                // INITIALTRANSFER on methods that were overwritten by
                // subsequent methods due to their execution order defined
                // by the RULES section.
                _method_list[i]->setInactiveState( );
                // This also eliminates them from further consideration
                // later in this method.
            }
        }
    }

    // Available inflow is now applicable to satisfy maxWith.
    // We already know there is insufficient to meet it however.
    // Given maxWith, how much Withdrawal can we have?
    double newOUT;
    newOUT = maxWith + availIN;

    double scalar;
    if( newOUT > 0 && planOUT > 0 )
    {
        scalar = newOUT / planOUT;
        // Anti-oscillation values passed into this method could cause scalar
        // to be > 1.
        if( scalar > 1 )
        {
            scalar = 1.0;
        }
    }
    else
    {
        // Even with augmentations, we don't have enough water.
        scalar = 0;
    }

    // Reduce the withdrawal by multiplying by the scaling factor and send
    // the scaled withdrawals ToComp (as appropriate).
    for( i=_n_meth-1; i>=0; i-- )
    {
        meth = _method_list[i];
        // Ensure the method is a "top level" method by checking for
        // an expression.  Also check to make sure we are working with
        // a withdrawal-type method (currently Lookup3 only).
        if( _expr_list[i] != NULL && meth->_group_id == WITHD_METHOD )
        {
            // If this method is active, then it was executed.
            if ( _method_list[i]->_Active )
            {
                // Transfer, if appropriate.
                _method_list[i]->sendToTS( date, scalar );

                // We have found the active method (it may be Combo or
                // simple), but we don't need to continue to look.
                break;
            }
        }
    }

    // Now, lets retraverse the methods and regather the inflows
    // (augmentations) and outflows (withdrawals) so we can revise our net
    // withdrawals.
    availIN = 0.0;
    planOUT = 0.0;
    for( i=_n_meth-1; i>=0; i-- )
    {
        meth = _method_list[i];
        // Ensure the method is a "top level" method by checking for
        // an expression.  Also check to make sure we are working with
        // a withdrawal-type method (currently Lookup3 only).
        if( _expr_list[i] != NULL && meth->_group_id == WITHD_METHOD )
        {
            // If this method is active, then it was executed.
            if ( _method_list[i]->_Active )
            {
                // Request the method's value as a withdrawal
                methValIN = 0.0;
                methValOUT = 0.0;
                _method_list[i]->getMyValueAsOut( &methValIN, &methValOUT );
                availIN += methValIN;
                planOUT += methValOUT;

                // We have found the active method (it may be Combo or
                // simple), but we don't need to continue to look.
                break;
            }
        }
    }
    // Modify the _withdraw variable.
    _withdraw = planOUT - availIN;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reservoir_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_SetGet.cxx,v 1.9 2006/10/26 15:32:18 hsu Exp $";}
/*  ===================================================  */

}
