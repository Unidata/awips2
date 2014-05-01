//------------------------------------------------------------------------------
// MaxDecrease :: solveMethod - Algorithm that solves the MaxDecrease method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//		new_rel = prev_release + _max_decrease * ( _t_mult / 24.0 );
//		Increments previous release by the fraction of the maximum 
//			decrease calculated by time step / 24 hours.
//		EG: 6 hr timestep, maximum decrease = 10, then
//			new_rel = prev_release + 10 * ( 6 / 24 ) or
//			new_rel = prev_release + 2.5
//
//------------------------------------------------------------------------------
// History:
// 
// 17 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 27 Nov 2001	James R. VanShaar, RTi	Changed calculations to be consistent
//					with intent to not increase releases
//					more than _max_increase in any 24 hour
//					period.  
// 11 Dec 2002	JRV, RTi	Added assignment of _Active.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "MaxDecrease.h"

int MaxDecrease :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[]="MaxDecrease::solveMethod";
	double	prev_release = 0.0, new_rel = 0.0;
	TSDate	idate;

	// Set _Active to 1
	_Active = 1;

	idate = cur_date;
	idate.addInterval( _t_int, -1*_t_mult );

	// Get previous release from the dam
	if ( idate >= getForecastDate1() ) {
		// We can look at the time series
		prev_release = _owner->_outflow_ts.getDataValue( idate );
	}
	else {
		// We need to access carry over data
		prev_release = _owner->_releaseCO;
	}
	
	new_rel = prev_release + _max_decrease * ( _t_mult / 24.0 );

	// Check to see that this release is reasonable...
	if( new_rel < _owner->_min_release ) { 
		PrintWarning( 1, routine, "%s %s on %s produced "
			"release that is less than the reservoir's minimum "
			"release (%f < %f). Setting release to minimum "
			"release.", _type, _id, _owner->_id, new_rel, 
			_owner->_min_release );
		new_rel = _owner->_min_release;
	}

	// Only set the _owner members if calling function is not
	// a ComboMethod.
	if( !group_val ) {
		_owner->_release = new_rel;
	}
	else { 
		*group_val[0] = new_rel;
		return( STATUS_SUCCESS );
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxDecrease_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: MaxDecrease_solveMethod.cxx,v 1.4 2006/10/26 15:26:15 hsu Exp $";}
/*  ===================================================  */

}
