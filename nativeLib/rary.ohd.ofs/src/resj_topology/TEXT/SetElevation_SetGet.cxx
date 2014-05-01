//------------------------------------------------------------------------------
// SetElevation :: SetGet - Set/gets for data members.
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
// 21 Nov 2001  JRV, RTi        Created setCOstring
// 11 Dec 2002	JRV, RTi	Enhanced setInactiveState.
// 18 Dec 2002	JRV, RTi	Revised method carryover to work with entire
// 				method identification set (type, owner_id,
// 				method_id).
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetElevation.h"
#include "ResJSys.h"

void SetElevation :: setInactiveState()
{
	// Reinitialize as inactive any past blending variables
	_ts_step = _n_blend_ts + 1;
	_tbl_step = 1;

	// Note: PREVIOUSPOOL (a state value required as carry over is stored 
	//	as part of the reservoir info and / or time series.

	_Active = 0;

	return;
}

int SetElevation :: setCOstring( )
{
	char routine[]="SetElevation::setCOstring";
	char future[9]="*FUTURE*";
	char COtype[13]="METHOD";
	char tsStep_str[5], tblStep_str[5], future_str[17], value_str[11];
	char temp_str[52+8+16+1];	// Labels, values, future space, end

	// Get current values (values at end of the current time step)
	sprintf( tsStep_str, "%d", _ts_step );
	sprintf( tblStep_str, "%d", _tbl_step );

	// Prepare value portion of the string
	sprintf(value_str, "%4.4s%4.4s", tsStep_str, tblStep_str);

	// Prepare future place holders
	sprintf(future_str, 
		"%8.8s%8.8s", future,future);
	future_str[16] = '\0';

	// Prepare the full variable string
	COtype[6] = '\0';
	sprintf( temp_str, 
		"%-12.12s%-12.12s%4.4s%-12.12s%-12.12s%8.8s%16.16s",
		COtype, _id, "-999", "SETELEVATION", _owner->_id, value_str,
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

int SetElevation :: setCOstring( TSDate& cur_date )
{
	char routine[]="SetElevation::setCOstring";
	int success;

	// CO is date independent.  Therefore simply call the non-dated 
	//	setCOstring.
	
	success = setCOstring( );

	return( success );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetElevation_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: SetElevation_SetGet.cxx,v 1.4 2006/10/26 15:32:54 hsu Exp $";}
/*  ===================================================  */

}
