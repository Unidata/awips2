//------------------------------------------------------------------------------
// SetWithdraw :: initialize - initialized instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 Mar 1998	Matthew J. Rutherford, RTi
//					Created initial version.
// 20 Apr 1998	Daniel Weiler, RTi	Added blending functionality.
// 20 Jun 2001	James R. VanShaar, RTi	Replaced usage of _n_withdraw with
//					_n_elev for consistency with SetRelease
// 26 Jun 2001	JRV, RTi	Initialized _elev to NULL
// 12 Nov 2001	JRV, RTi	Added _hasStates initialization.
// 12 Dec 2002	JRV, RTi	Added _receivingComp, _Comp_ts, _toCompMode
// 				initialization.
// 12 Dec 2002	JRV, RTi	Added _Comp_ts.
// 10 Jul 2003	JRV, RTi	Included initialization of _myValue, as 
// 				submitted by Kuang-Shen Hsu.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetWithdraw.h"

int SetWithdraw :: initialize()
{
	 char routine[]="SetWithdraw::construct";

	strcpy( _type, "SETWITHDRAW" );
	_group_id = WITHD_METHOD;
	_withdraw_ctl = NULL;
	_withdraw_obs = NULL;
	_elev = NULL;

	_n_elev = 0;
	//_n_withdraw = 0;
	_n_blend_tbl = -1;
	_n_blend_ts = -1;
	_tbl_step = 1;
	_ts_step = 1;

	// Initialize this to provide way for error trapping in 
	// SetWithdraw::construct
	_myValue = MISSING;

	// Allocate and set up the _outflow TS...
	TSDate t;
	TSDate t1 = Method :: getForecastDate1();
	TSDate t2 = Method :: getForecastDate2();
	int timeInt = Method :: getTimeInterval();
	int timeMult = Method :: getTimeMult();

	_Comp_ts.setDate1( t1 );
	_Comp_ts.setDate2( t2 );
	_Comp_ts.setDataInterval( timeInt, timeMult );
	if( _Comp_ts.allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for outflow time series on %s.", _id );
		return( STATUS_FAILURE );
	}
	// Initialize all values to zero
	for( t=t1; t<=t2; t.addInterval( timeInt, timeMult ) ) {
		_Comp_ts.setDataValue( t, 0.0 );
	}

	_receivingComp = NULL;
	_toCompMode = 1;

	_hasStates = 1;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetWithdraw_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: SetWithdraw_initialize.cxx,v 1.6 2006/10/26 15:35:20 hsu Exp $";}
/*  ===================================================  */

}
