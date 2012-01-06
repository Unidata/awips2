//------------------------------------------------------------------------------
// SetAdjust :: SetGet - Set/gets for data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 2001	James R. VanShaar, RTi
//					Created initial version.
// 21 Nov 2001	JRV, RTi	Created setCOstring
// 14 Oct 2002	JRV, RTi	Created adjsim().
// 11 Dec 2002	JRV, RTi	Enhanced setInactiveState.
// 18 Dec 2002	JRV, RTi	Revised method carryover to work with entire
// 				method identification set (type, owner_id,
// 				method_id).
// 16 Jan 2003	JRV, RTi	Revised setInactiveState to prevent inactivation
// 				of ADJSIM OFF methods.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Adjust.h"
#include "ResJSys.h"

int Adjust :: setCOstring_AdjsimOff (TSDate& cur_date)
{
	char routine[]="Adjust::setCOstring_AdjsimOff";
	double adjPrevPool, adjStartPool, adjPrevRelease, adjStartRelease;
	TSDate prev_date;

	prev_date = _owner->_prev_date;

	// Make a copy of possible adjusted states
	adjPrevPool = _owner->_prevPool;
	adjStartPool = _owner->_startPool;
	adjPrevRelease = _owner->_prevRelease;
	adjStartRelease = _owner->_startRelease;

	// Modify the states using the observed time series.
	if ( _pool_obs != NULL ) {
		double adjPool = _pool_obs->getDataValue( prev_date );
		if ( adjPool != MISSING ) {
			// Ensure compliance with pool MinimumPool
			if( adjPool < _owner->_min_pool ) {
				PrintWarning( 1, routine, "Observed pool for "
					"%s on %s is less than the Minimum "
					"pool (%f < %f). Setting to Minimum "
					"pool.", _id, _owner->_id, adjPool, 
					_owner->_min_pool );
				adjPool = _owner->_min_pool;
			}
			_owner->_prevPool = adjPool;
		}
		adjPool = _pool_obs->getDataValue( cur_date );
		if ( adjPool != MISSING ) {
			// Ensure compliance with pool MinimumPool
			if( adjPool < _owner->_min_pool ) {
				PrintWarning( 1, routine, "Observed pool for "
					"%s on %s is less than the Minimum "
					"pool (%f < %f). Setting to Minimum "
					"pool.", _id, _owner->_id, adjPool, 
					_owner->_min_pool );
				adjPool = _owner->_min_pool;
			}
			_owner->_startPool = adjPool;
		}
	}

	if ( _release_obs != NULL ) {
		double adjRelease = _release_obs->getDataValue( prev_date );
		if( adjRelease != MISSING ) {
			// Since the observed release geing less than MINRELEASE
			// isn't fatal to the program, like going below the
			// MINPOOL value, we assume that it really happened and
			// proceed to prescribe it without testing it agains the
			// simulation parameter.
			_owner->_prevRelease = adjRelease;
		}
		adjRelease = _release_obs->getDataValue( cur_date );
		if( adjRelease != MISSING ) {
			// Since the observed release geing less than MINRELEASE
			// isn't fatal to the program, like going below the
			// MINPOOL value, we assume that it really happened and
			// proceed to prescribe it without testing it agains the
			// simulation parameter.
			_owner->_startRelease = adjRelease;
		}
	}

	// With the states modified, set the CO string
	_owner->setCOstring(cur_date);

	// If necessary, reset states which may have been modified.  If they 
	// were not modified, some of this is unnecessary, but faster than 
	// determining which states were actually changed.
	_owner->_prevPool = adjPrevPool;
	_owner->_startPool = adjStartPool;
	_owner->_prevRelease = adjPrevRelease;
	_owner->_startRelease = adjStartRelease;

	return( STATUS_SUCCESS );
}

int Adjust :: adjsim()
{
	return _adjsim;
}

void Adjust :: setInactiveState()
{
	char routine[]="Adjust::setInactiveState";

	if( _adjsim ) {
		// Reinitialize as inactive any past blending variables
		_n_tstep = _n_blend + 1;

		_Active = 0;
	}
	else {
		// We do not allow an "ADJSIM OFF" method to be deactivated.
		// It produces too many confusing scenarios when it comes
		// time to writing carryover adjusted in some fashion by an
		// adjust method.
		// The documentation also states that "ADJSIM OFF" methods must
		// be associated with the TRUE expression.
		PrintWarning( 1, routine, "Inactivation of \"ADJSIM OFF\" "
			"method \"ADJUST %s %s\" is not allowed.  It should be "
			"associated with the [TRUE] expression only.", 
			_owner->_id, _id );
	}	

	return;
}

int Adjust :: setCOstring( )
{
	char routine[]="Adjust::setCOstring";
	char future[9]="*FUTURE*";
	char COtype[13]="METHOD";
	char tsStep_str[5], future_str[17], value_str[5];
	char temp_str[52+4+16+1];	// Labels, values, future space, end

	// Get current values (values at end of the current time step)
	sprintf( tsStep_str, "%d", _n_tstep );

	// Prepare value portion of the string
	sprintf(value_str, "%4.4s", tsStep_str );

	// Prepare future place holders
	sprintf(future_str, "%8.8s%8.8s", future, future);
	future_str[16] = '\0';

	// Prepare the full variable string
	COtype[6] = '\0';
	sprintf( temp_str, 
		"%-12.12s%-12.12s%4.4s%-12.12s%-12.12s%4.4s%16.16s",
		COtype, _id, "-999", "ADJUST", _owner->_id, value_str, 
		future_str );

	// Write the string to the system CO string
	if( ResJSys::addCOString( temp_str ) ) {
		PrintWarning( 1, routine, 
			"Troubles adding carryover data to "
			"CO array." );
		return( STATUS_FAILURE );
	}

	// Need to develop CO etc.
	
	return( STATUS_SUCCESS );
}

int Adjust :: setCOstring( TSDate& cur_date )
{
	char routine[]="Adjust::setCOstring";
	int success;

	// CO is date independent.  Therefore simply call the non-dated 
	//	setCOstring.
	
	success = setCOstring( );

	return( success );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Adjust_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Adjust_SetGet.cxx,v 1.4 2006/10/26 14:48:53 hsu Exp $";}
/*  ===================================================  */

}
