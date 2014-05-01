//------------------------------------------------------------------------------
// CalcInflow::solveMethod - Solver for the CalcInflow method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 16 Feb 2004	James R. VanShaar,	Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "CalcInflow.h"
#include "ResJSys.h"


int CalcInflow :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[] = "CalcInflow::solveMethod";

	TSDate prev_date = _owner->_prev_date;

	// Set _Active to 1
	_Active = 1;

	// Get storage variables.
	double obsPool=0, startStor=0, endStor=0;
	if( prev_date < Method::getForecastDate1() ) {
		// Access release from carryover
		obsPool=_startPool;
	}
	else {
		obsPool=_pool_obs->getDataValue(prev_date);
	}
	if( obsPool == MISSING ) {
		// We cannot solve this
		setInactiveState();
		_inflow_calc->setDataValue(cur_date, MISSING);
		return STATUS_SUCCESS;
	}
	startStor=_owner->_elev_stor_tbl.lookup( obsPool, GETCOLUMN_2, 
		ALLOW_BOUNDS );
		// NOTE: Any observed pool elevation beyond the reservoir
		//       elevation / storage curve will return a storage
		//       value equal to the largest or smallest in the table.

	obsPool=_pool_obs->getDataValue(cur_date);
	if( obsPool == MISSING ) {
		// We cannot solve this
		setInactiveState();
		_inflow_calc->setDataValue(cur_date, MISSING);
		return STATUS_SUCCESS;
	}
	endStor=_owner->_elev_stor_tbl.lookup( obsPool, GETCOLUMN_2, 
		ALLOW_BOUNDS );
		// NOTE: Any observed pool elevation beyond the reservoir
		//       elevation / storage curve will return a storage
		//       value equal to the largest or smallest in the table.

	// Get release variables
	double startRel=0, endRel=0;
	if( _release_obs) {
		if( prev_date < Method::getForecastDate1() ) {
			// Access release from carryover
			startRel=_startRelease;
		}
		else {
			startRel=_release_obs->getDataValue(prev_date);
		}
		if( startRel == MISSING ) {
			// We will not solve this
			setInactiveState();
			_inflow_calc->setDataValue(cur_date, MISSING);
			return STATUS_SUCCESS;
		}
		endRel=_release_obs->getDataValue(cur_date);
	}
	if( endRel == MISSING ) {
		// We will not solve this
		setInactiveState();
		_inflow_calc->setDataValue(cur_date, MISSING);
		return STATUS_SUCCESS;
	}

	// Get withdrawal variables
	double startWith=0, endWith=0;
	if( _withdraw_obs) {
		if( prev_date < Method::getForecastDate1() ) {
			// Access release from carryover
			startWith=_startWithdrawal;
		}
		else {
			startWith=_withdraw_obs->getDataValue(prev_date);
		}
		if( startWith == MISSING ) {
			// We will not solve this
			setInactiveState();
			_inflow_calc->setDataValue(cur_date, MISSING);
			return STATUS_SUCCESS;
		}
		endWith=_withdraw_obs->getDataValue(cur_date);
	}
	if( endWith == MISSING ) {
		// We will not solve this
		setInactiveState();
		_inflow_calc->setDataValue(cur_date, MISSING);
		return STATUS_SUCCESS;
	}

	// Get loss volume variable
	double netLossVol=0;
	if( _useLoss ) {
		netLossVol=_owner->_loss;
	}

	// Get inflow variable.
	// We only need it, under the current solution scheme, if we are using
	// some inflow constraint.  We won't worry if we aren't constraining
	// the change.
	double startInfl=0;
	if( _constrainChange ) {
		if( prev_date < Method::getForecastDate1() ) {
			// Access release from carryover.  We will have 
			// something here, even if it was taken from Reservoir 
			// carryover.
			startInfl=_startInflow;
		}
		else {
			// We may or may not have a real value for this 
			// timestep.
			startInfl=_inflow_calc->getDataValue(prev_date);
		}
		if( startInfl == MISSING ) {
			// Probably didn't solve last timestep, but we have
			// to start somewhere.  Note, this is a state variable.
			startInfl = _owner->_startInflow;
		}
	}
	
	// Waterbalance
	// We are going to assume that endInfl represents the average inflow 
	// over the timestep.  There is some error in this approach, given
	// that the Reservoir::finalizeSolution will not use the endInfl as
	// an average, but rather as an instantaneous value.  This error,
	// we hope, will average out over the long term.  We will track it
	// during development and testing to see whether our assumption is
	// valid.  We also hope that this assumption will eliminate, or
	// greatly reduce, oscillation observed during development.
	double endInfl=0, secPerStep=Method::getTimeStepSec();
//	endInfl= 2 * ( (endStor - startStor) + netLossVol ) / 
//		secPerStep + endRel + startRel + endWith + startWith - 
//		startInfl + _remainingVol / secPerStep;
//	endInfl= ( (endStor - startStor) + netLossVol + _remainingVol ) / 
//		secPerStep + ( endRel + startRel + endWith + startWith ) / 2;
//	double theta=0.5;
//	endInfl= ( ( (endStor - startStor) + netLossVol ) / secPerStep + 
//		( endRel + startRel + endWith + startWith ) / 2 - 
//		( 1 - theta ) * startInfl ) / theta + _remainingVol / secPerStep;
//	endInfl=( ( (endStor - startStor) + netLossVol ) / secPerStep + 
//		( endRel + startRel + endWith + startWith ) / 2 - 
//		( 1 - theta ) * startInfl + _remainingVol / secPerStep ) / 
//		theta;
	// Start by calculating the mass balance, strictly for this timestep.
	// We assume endInfl to be the average over the timestep.
	double endInflAvg = ( (endStor - startStor) + netLossVol ) / secPerStep + 
		( endRel + startRel + endWith + startWith ) / 2;
	//endInfl=2*endInflAvg - startInfl;
	//_meanToInstErr+=0.5*(endInfl-endInflAvg)*secPerStep;

	// Now adjust the endInflAvg to address the remainingVolume:
	endInflAvg += _remainingVol / secPerStep;

	// DEBUG
	endInfl=endInflAvg;

	// We have applied the _remainingVol.  Reinitialize it!
	_remainingVol=0;

	// Handle Constraints
	// Our constraint potentially strips off a certain volume of inflow
	// for application at some future timestep.

	// Check for change constraints.
	if( _constrainChange ) {
		double constrInflow, change;
		if( endInflAvg >= startInfl ) {
			// Need to look in the maxIncrease table.
			// Determine the maximum inflow
			change = _maxIncrease.lookup(startInfl, GETCOLUMN_2, 
				ALLOW_BOUNDS);
			constrInflow = startInfl + change;
			if( endInflAvg > constrInflow ) {
				// Constrain
				// This will be an increase in _remainingVolume 
				// because endInflAvg will be larger than 
				// constrInflow.
				_remainingVol = secPerStep *
					(endInflAvg - constrInflow);
				endInflAvg = constrInflow;
			}
		}
		else {
			// Need to look in the maxDecrease table, and
			// maybe consider the _minInflow value.
			change = _maxDecrease.lookup(startInfl, GETCOLUMN_2, 
				ALLOW_BOUNDS);
			constrInflow = startInfl - change;
			if( endInflAvg < constrInflow ) {
				// Constrain
				// This will be a decrease in _remainingVolume 
				// because endInflAvg will be smaller than 
				// constrInflow.
				_remainingVol = secPerStep * (endInflAvg - 
					constrInflow);
				endInflAvg = constrInflow;
			}

		}
	}
	// Check for MinimumFlow constraints
	if( _minInflow != MISSING ) {
		if( endInflAvg < _minInflow ) {
			// Apply remaining volume to variable
			// This will be a decrease in _remainingVolume because
			// endInflAvg will be smaller than _minInflow.
			_remainingVol += secPerStep *
				(endInflAvg - _minInflow);
			endInflAvg = _minInflow;
		}
	}

	// DEBUG
if( !strcasecmp( _owner->_id, "RESB" ) ){
	int ipr = ResJSys::getIPR(); 
	char temp[MAXC];
	sprintf( temp, "%s", _owner->_id );
	sprintf( temp, "date=%s\nendStor=%f\nstartStor=%f\nnetLossVol=%f\n"
		"_remainingVol=%f\nendRel=%f\nstartRel=%f\nendWith=%f\n"
		"startWith=%f\nendInfl=%f\nstartInfl=%f\n_meanToInstErr=%f\n"
		"origInfl=%f\n",
	       	cur_date.toString(), endStor, startStor, netLossVol, _remainingVol, endRel, startRel,
	       	endWith, startWith, endInflAvg, startInfl, _meanToInstErr, endInfl);
	int length = strlen( temp );
	ResJ_ccwrite( &length, temp, &ipr );
}

	// Now shift to using the endInflAvg as if it were instantaneous:
	endInfl=endInflAvg;

	// We cannot have endInfl able to be interpreted as missing.
	if( endInfl < -997.9 && endInfl > -999.1 ) {
		_remainingVol += secPerStep * (endInflAvg - (-997.8));
		endInfl = -997.8;
	}

	// Prevent the unlikely case that _remainingVol appears to be MISSING
	if( _remainingVol < -997.5 && _remainingVol > -1000 ) {
		_remainingVol = -999.2;
		PrintWarning( 1, routine, "_remainingVol was adjusted to not "
			"appear to be missing for %s %s %s.", _type, 
			_owner->_id, _id );
	}

	// At this point we have determined our inflow.

	// Place the inflow in the appropriate locations
	_myValue=endInfl;
	_inflow_calc->setDataValue(cur_date, endInfl);
	if( _useForSim ) {
		// Put on Component Total Inflow TS
		_owner->setEndInflow( cur_date, _myValue );
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_solveMethod.cxx,v 1.2 2006/10/26 15:11:54 hsu Exp $";}
/*  ===================================================  */

}

