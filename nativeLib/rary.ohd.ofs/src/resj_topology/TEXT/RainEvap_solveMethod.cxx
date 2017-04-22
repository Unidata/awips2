//------------------------------------------------------------------------------
// RainEvap :: solveMethod - Algorithm that solves the RainEvap method.
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
// 24 Sep 1998  DKW, RTi	Redid data retrieval based on Mehtod-centric
//				inputTS data storage.
// 01 Jul 2002	James R. VanShaar, RTi	Added updating of _storage on owner
//					for non-combo method solution.
// 11 Dec 2002	JRV, RTi	Added assignment of _Active.
// 11 Jul 2003	JRV, RTi	Revised handling of timeseries and tables.
// 				Also, revised definition of loss so a reduction
// 				in storage is a positive loss.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "RainEvap.h"

int RainEvap :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[]="RainEvap :: solveMethod";
	double 	elev_gain = 0.0, evap = MISSING, precip = MISSING, new_pool, 
		loss;
	int i;

	// Set _Active to 1
	_Active = 1;

	// For now we are going to assume that there is a direct relation-
	// ship between the precip, evap and change in elevation. That is to
	// say, that the walls of the reservoir in question are vertical, and
	// and inch of rain that falls is equal to an inch of elevataion gain.

	// Need to check the existence of the evap ptr before using - if it 
	// doesn't exist, then we need to use the value supplied in the 
	// control file
	if( _evap_obs ) {
		evap = _evap_obs->getDataValue( cur_date );
	}
	// If the timeseries contained MISSING data or there was no timeseries
	// (the value will still be MISSING)
	if( evap == MISSING ) {
		// Check to see if we have a table
		if( _n_evap ) {
			evap = _evap_ctl.getDataValue( cur_date, _mode );
			// NOTE: If the table was not specifically defined
			// with a diurnal distribution, values of evap will 
			// represent the input distribution assumption.  That 
			// is, the result will equal _t_mult / 24 * original 
			// value.  This is equivalent to assuming that the
			// input values are daily totals and here we are taking
			// only a fraction of the daily total for this timestep.
			// Please reference the setDataValue and getDataValue
			// commands here and in ::construct and compare them
			// to how similar commands are applied in, say, the
			// SetRelease method.
		}
		else {
			PrintWarning( 1, routine, "MISSING data in evap "
				"timeseries for RainEvap %s on %s.  Defaulting "
				"to 0.", _id, _owner->_id );
			// We will default to 0
			evap = 0;
		}
	}

	// Need to check the existence of the precip ptr before using - if it 
	// doesn't exist, then we need to use the value supplied in the 
	// control file
	if( _precip_obs ) {
		precip = _precip_obs->getDataValue( cur_date );
	}
	// If the timeseries contained MISSING data or there was no timeseries
	// (the value will still be MISSING)
	if( precip == MISSING ) {
/*
		// Check to see if we have a table
		if( _n_precip ) {
			precip = _precip_ctl.getDataValue( cur_date, _mode );
			// NOTE: If the table was not specifically defined
			// with a diurnal distribution, values of precip will 
			// represent the input distribution assumption.  That 
			// is, the result will equal _t_mult / 24 * original 
			// value.  This is equivalent to assuming that the
			// input values are daily totals and here we are taking
			// only a fraction of the daily total for this timestep.
			// Please reference the setDataValue and getDataValue
			// commands here and in ::construct and compare them
			// to how similar commands are applied in, say, the
			// SetRelease method.
		}
		else {
			PrintWarning( 1, routine, "MISSING data in precip "
				"timeseries for RainEvap %s on %s.  Defaulting "
				"to 0.", _id, _owner->_id );
			// We will default to 0
			precip = 0;
		}
*/
		precip = 0;
	}

//	// If either one of these data points are missing, print a warning and
//	// return without adjusting the loss.
//	if( evap == MISSING || precip == MISSING ) {
//		// This really shouldn't happen if the tables are set up.
//		_owner->_loss = 0.0;
//		PrintWarning( 1, routine, "MISSING data in for RainEvap %s on "
//			"%s.", _id, _owner->_id );
//		return( STATUS_SUCCESS );	
//	}

	// difference in elevation due to precip and evap and the resulting new 
	// pool ( converted from mm to m )...
	elev_gain = ( precip - evap ) / 1000.0;

	// Check this first - it should never be true...
	if( _owner->_pool == MISSING ) {
		PrintWarning( 1, routine, "Previous pool elevation on %s is "
			"missing - not setting loss.", _owner->_id );
		return( STATUS_SUCCESS );
	}
	new_pool = _owner->_pool + elev_gain;
	if( new_pool < _owner->_min_pool ) {
		PrintWarning( 1, routine, "RainEvap %s on generated losses "
			"that cause pool on %s to drop below Minimum Pool (%f "
			"< %f). Setting pool to minimum.", _id, _owner->_id, 
			new_pool, _owner->_min_pool );
		new_pool = _owner->_min_pool;
	}

	// Calculate the loss due to the change in elevation...
	// Loss is old storage - new storage.  Therefore, if storage has gone
	// down, we have a positive loss.
	double oldStor = _owner->_elev_stor_tbl.lookup( _owner->_pool, 
		GETCOLUMN_2, ALLOW_BOUNDS);
       	double newStor =  _owner->_elev_stor_tbl.lookup( new_pool, 
		GETCOLUMN_2, ALLOW_BOUNDS); 
	loss = oldStor - newStor;

        // Only set the _owner members if calling function is not
	// a ComboMethod.
	if( !group_val ) {
		_owner->_loss = loss;
		// Reset the working copy of the pool and storage
		_owner->_pool = new_pool;
		_owner->_storage = newStor;
	}
	else {
		*group_val[0] = loss;
		*group_val[1] = new_pool;
		return( STATUS_SUCCESS );
	}

	PrintDebug( 5, routine, "Setting loss on %s to %f for %s.",
		_owner->_id, loss, cur_date.toString() ); 
	
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/RainEvap_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: RainEvap_solveMethod.cxx,v 1.9 2006/10/26 15:29:38 hsu Exp $";}
/*  ===================================================  */

}
