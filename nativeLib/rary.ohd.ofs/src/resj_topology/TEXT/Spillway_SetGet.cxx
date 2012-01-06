//------------------------------------------------------------------------------
// Spillway :: SetGet - Set/gets for data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 24 Dec 2002	James R. VanShaar, RTi
//					Created initial version.
// 14 Jan 2003	JRV, RTi	Added _myValue to carryover.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Spillway.h"
#include "ResJSys.h"

void Spillway :: setInactiveState()
{
	_myValue = 0.0;

	_Active = 0;

	return;
}

int Spillway :: setCOstring( )
{
	char routine[]="Spillway::setCOstring";
	char future[9]="*FUTURE*";
	char COtype[13]="METHOD";
	char future_str[81], value_str[9];
	char temp_str[52+8+80+1];	// Labels, value, future space, end
	
	// Prepare value portion of the string
	sprintf(value_str, "%f", _myValue);
	value_str[8] = '\0';
	
	// Prepare future place holders
	sprintf(future_str, 
		"%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s", future,
		future,future,future,future,future,future,future,future,future);
	future_str[80] = '\0';

	// Prepare the full string
	COtype[6] = '\0';
	sprintf( temp_str, 
		"%-12.12s%-12.12s%4.4s%-12.12s%-12.12s%8.8s%80.80s",
		COtype, _id, "-999", "SPILLWAY", _owner->_id, value_str, 
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

int Spillway :: setCOstring( TSDate& cur_date )
{
	char routine[]="Spillway::setCOstring";
	int success;

	// CO is date independent.  Therefore simply call the non-dated 
	//	setCOstring.
	
	success = setCOstring( );

	return( success );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_SetGet.cxx,v 1.2 2006/10/26 15:36:01 hsu Exp $";}
/*  ===================================================  */

}
