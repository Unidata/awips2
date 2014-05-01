//------------------------------------------------------------------------------
// CalcInflow :: SetGet - Set/gets for data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Feb 2004	James R. VanShaar, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "CalcInflow.h"
#include "ResJSys.h"

void CalcInflow :: setInactiveState()
{
	// Reinitialize as inactive carryover values
	_remainingVol = 0;
	_myValue = MISSING;

	_Active = 0;

	return;
}

int CalcInflow :: setCOstring( )
{
	char routine[]="CalcInflow::setCOstring";
	char future[9]="*FUTURE*";
	char COtype[13]="METHOD";
	char remainingVol_Str[17]="MISSINGX", startInflow_Str[9]="MISSINGX", 
		startPool_Str[9]="MISSINGX", startRelease_Str[9]="MISSINGX",
		startWithdrawal_Str[9]="MISSINGX",
		future_str[81], value_str[49];
	char temp_str[52+48+80+1];	// Labels, values, future space, end

	// Prepare value portion of the string.  The string defaults to MISSINGX
	// but overwrite it with the real value if the variable is not MISSING.
	if( _remainingVol != MISSING ) {
		// There really should be no way to have _remainingVol=MISSING,
		// but we check just in case.
		sprintf( remainingVol_Str, "%16f", _remainingVol );
	}
	if( _startInflow != MISSING ) {
		// There really should be no way to have _startInflow=MISSING,
		// but we check just in case.
		sprintf( startInflow_Str, "%f", _startInflow );
	}
	if( _startPool != MISSING ) {
		sprintf( startPool_Str, "%f", _startPool );
	}
	if( _startRelease != MISSING ) {
		sprintf( startRelease_Str, "%f", _startRelease );
	}
	if( _startWithdrawal != MISSING ) {
		sprintf( startWithdrawal_Str, "%f", _startWithdrawal );
	}

	sprintf( value_str, "%16.16s%8.8s%8.8s%8.8s%8.8s", remainingVol_Str, 
		startInflow_Str, startPool_Str, startRelease_Str, 
		startWithdrawal_Str );
	value_str[48] = '\0';
	
	// Prepare future place holders
	sprintf(future_str, 
		"%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s", future,
		future,future,future,future,future,future,future,future,future);
	future_str[80] = '\0';

	// Prepare the full string
	COtype[6] = '\0';
	sprintf( temp_str, 
		"%-12.12s%-12.12s%4.4s%-12.12s%-12.12s%48.48s%80.80s",
		COtype, _id, "-999", "CALCINFLOW", _owner->_id, value_str, 
		future_str );

	// Write the string to the system CO string
	if( ResJSys::addCOString( temp_str ) ) {
		PrintWarning( 1, routine, 
			"Troubles adding carryover data to "
			"CO array." );
		return( STATUS_FAILURE );
	}

	return( STATUS_SUCCESS );
}

int CalcInflow :: setCOstring( TSDate& cur_date )
{
	char routine[]="CalcInflow::setCOstring";
	int success;

	// CO is date dependent.  Prepare date appropriate values herein.
	// NOTE: This will be called at the end of a timestep.  Therefore, we
	//       want to use the ending values, which become the starting
	//       values for the next timestep to be solved, providing carryover
	//       for continuation at the next timestep.

	// Nothing to do with _remainingVol.  
	// It has already been set appropriately, elsewhere.

	// Get inflow variable.
	_startInflow=_inflow_calc->getDataValue(cur_date);
	if( _startInflow == MISSING ) {
		// Probably didn't solve this timestep, but we have to start 
		// somewhere.  NOTE: The reservoir variable is a state.  It
		// is also the value from the end of this time step, but will
		// have been transferred to the _startInflow variable in
		// preparation for the next time step.
		_startInflow = _owner->_startInflow;
	}

	// Get pool variable.  We will store a "MISSINGX" value, if necessary.
	_startPool=_pool_obs->getDataValue(cur_date);
	
	// Get release variable.  We will store a "MISSINGX" value, if 
	// necessary.
	if( _release_obs) {
		_startRelease=_release_obs->getDataValue(cur_date);
	}
	// Else: we have not changed _startRelease from its value
	//       of MISSING (either initialized or revised in construct).  
	//       We will store MISSING in the COstring.
	
	// Get withdrawal variable.  We will store a "MISSINGX" value, if 
	// necessary.
	if( _withdraw_obs) {
		_startWithdrawal=_withdraw_obs->getDataValue(cur_date);
	}
	// Else: we have not changed _startWithdrawal from its value
	//       of MISSING (either initialized or revised in construct).  
	//       We will store MISSING in the COstring.

	// Now that we have prepared the data required, according to the
	// cur_date, call setCOstring().
	success = setCOstring();
	
	return( success );


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_SetGet.cxx,v 1.2 2006/10/26 15:11:36 hsu Exp $";}
/*  ===================================================  */

}
