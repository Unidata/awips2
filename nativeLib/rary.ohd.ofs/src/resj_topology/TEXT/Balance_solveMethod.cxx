//------------------------------------------------------------------------------
// Balance :: solveMethod - Algorithm that solves the Balance method.
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
// 12 Jun 2002	James R. VanShaar, RTi	Revised scheme to use setElevation-type
//					release calculation (including use of
//					_startStorage) and to fix a percent bug.
// 13 Jun 2002	JRV, RTi	Added use of _totBalStor and _ace.
// 11 Dec 2002	JRV, RTi	Added assignment of _Active.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Balance.h"

int Balance :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[]="Balance :: solveMethod";
	double afs = 0.0, avg_afs, targPool, del_stor, release, 
		ubs = 0.0, percent, targStor;
	int i;

	// Set _Active to 1
	_Active = 1;

	if( _bal_mode == VOLUME ) {
		// First thing - determine the available free storage
		// for all of the reservoirs in the system.
		for( i = 0; i < _n_res; i++ ) {
			afs += _upper_stor[i] - _balance_res[i]->_startStorage;
		}

		// Compute the average of this total storage over the system
		// to determine how much each reservoir will store.
		avg_afs = afs / (double)(_n_res);

		// Calculate desired storage.
		targStor = _upper_stor[ _owner_pos ] - avg_afs;
	}	
	else if( _bal_mode == PERCENT ) {
		// Determine the used balancing storage.
		for( i = 0; i < _n_res; i++ ) {
			ubs += _balance_res[i]->_startStorage - _lower_stor[i];
		}
		// Compute the average of this total storage over the system
		// to determine how much each reservoir will store.
		percent = ubs / _totBalStor;

		// Calculate desired storage.  Use nomenclature from 
		// SetElevation algorithm.
		double targStorPrelim;
		targStorPrelim = percent*(_upper_stor[_owner_pos] - 
			_lower_stor[_owner_pos]) + _lower_stor[_owner_pos];
		if ( targStorPrelim < _owner->_startStorage ) {
			targStor = _ace * ( targStorPrelim - 
				_owner->_startStorage ) + _owner->_startStorage;
		}
		else {
			targStor = targStorPrelim;
		}
	}

	// Test limits on targStor as compared with balancing storage limits.
	// Adjust, if necessary.
	if ( targStor > _upper_stor[_owner_pos] ) {
		targStor = _upper_stor[_owner_pos];
	}
	else if ( targStor < _lower_stor[ _owner_pos ] ) {
		targStor = _lower_stor[ _owner_pos ];
	}

	// prepare current water balance terms
	double withdraw = _owner->_withdraw;	
		// Likely this has been modified by a withdrawal method, so it
		// will not equal _startWithdraw, but has not been set to 
		// _endWithdraw.
	double endInflow = _owner->_endInflow;
	double startInflow = _owner->_startInflow;
	double startRelease = _owner->_startRelease;
	double startWithdraw = _owner->_startWithdraw;
	double startStorage = _owner->_startStorage;
	
	// Calculate the preRelease corresponding to the expected water balance
	double preRelease;
	preRelease = 2 / Method::getTimeStepSec() * ((startStorage - targStor) +
		(endInflow + startInflow) / 2 * Method::getTimeStepSec() -
		(withdraw + startWithdraw) / 2 * Method::getTimeStepSec()) - 
		startRelease;

	// Attempt to reduce tendancy to oscillate.
	// Similar to SetElevation::solve anti-oscillation algorithm, except
	// 	that we assume that we are never maintaining a constant, 
	//	ongoing, previously met elevation goal.  Nevertheless, we act
	//	as if the elevation goal at the next timestep is the same as
	//	the current goal.  We also act as if the startStorage was 
	//	balanced, but different that the current goal.
	double inToStore = (targStor - startStorage) / Method::getTimeStepSec();
	release = (preRelease + endInflow - withdraw - inToStore ) / 2;

	// Check to make sure we don't release beyond _max_rel
	if ( release > _max_rel[_owner_pos] ) {
		release = _max_rel[ _owner_pos ];
	}

	// If, at this point, the release is below min release, that means 
	// that the requested elevation cannot be met by cutting back on the  
	// normal release schedules.
	// NOTE: _min_rel is the minrelease parameterized for the method owner 
	//	in this method, not the minimumrelease parameterized for the 
	//	reservoir itself.  Balance::construct ensures that the latter is
	//	never larger than the prior.  It also ensures that _max_rel >=
	//	_min_rel.
	else if( release < _min_rel[ _owner_pos ] ) {
		release = _min_rel[ _owner_pos ];
	}
	else if ( release <= 0.0001 ) {
		// We are in a filling type pattern
		release = 0;
	}

	// Ready for return
	if( !group_val ) {
		_owner->_release = release;
	}
	else {
		*group_val[0] = release; 
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Balance_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: Balance_solveMethod.cxx,v 1.5 2006/10/26 15:11:25 hsu Exp $";}
/*  ===================================================  */

}
