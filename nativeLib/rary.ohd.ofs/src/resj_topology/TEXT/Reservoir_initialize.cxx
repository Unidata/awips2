//------------------------------------------------------------------------------
// Reservoir :: initialize - initializes data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 17 May 2001	James R. VanShaar, RTi	Added *CO variables
// 10 Jul 2001	JRV, RTi	Removed duplicate _withdraw_ts work
// 19 Nov 2001	JRV, RTI	Added initialization of new carry over values
// 06 Jun 2002	JRV, RTi	Added _endInflow, _startInflow, _prevInflow,
//				_endPool, _startPool, _prevPool, _endWithdraw,
//				_startWithdraw, _prevWithdraw, _endRelease,
//				_startRelease, _prevRelease.
// 12 Jun 2002	JRV, RTi	Added _endStorage, _startStorage, _prevStorage.
// 18 Feb 2003	JRV, RTi	Minor fix in error messaging.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reservoir.h"

int Reservoir :: initialize()
{
	char routine[] = "Reservoir :: initialize";
	strcpy( _type, "RESERVOIR" );

	_pool_ts 	= NULL;
	_release_ts	= NULL;
	_spill_ts	= NULL;
	_withdraw_ts	= NULL;

	_loss_ts.setDate1( Method :: getForecastDate1() );
	_loss_ts.setDate2( Method :: getForecastDate2() );
	_loss_ts.setDataInterval( Method :: getTimeInterval(),
		Method :: getTimeMult() );
	if( _loss_ts.allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for loss time series on %s.", _id );
		return( STATUS_FAILURE );
	}

	_pool_ts = new HourTS();
	_pool_ts->setDate1( Method :: getForecastDate1() );
	_pool_ts->setDate2( Method :: getForecastDate2() );
	_pool_ts->setDataInterval( Method :: getTimeInterval(),
		Method :: getTimeMult() );
	if( _pool_ts->allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for pool time series on %s.", _id );
		return( STATUS_FAILURE );
	}

	_withdraw_ts = new HourTS();
	_withdraw_ts->setDate1( Method :: getForecastDate1() );
	_withdraw_ts->setDate2( Method :: getForecastDate2() );
	_withdraw_ts->setDataInterval( Method :: getTimeInterval(),
		Method :: getTimeMult() );
	if( _withdraw_ts->allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for withdrawal time series on %s.", _id );
		return( STATUS_FAILURE );
	}

	_release_ts = new HourTS();
	_release_ts->setDate1( Method :: getForecastDate1() );
	_release_ts->setDate2( Method :: getForecastDate2() );
	_release_ts->setDataInterval( Method :: getTimeInterval(),
		Method :: getTimeMult() );
	if( _release_ts->allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for release time series on %s.", _id );
		return( STATUS_FAILURE );
	}

	_spill_ts = new HourTS();
	_spill_ts->setDate1( Method :: getForecastDate1() );
	_spill_ts->setDate2( Method :: getForecastDate2() );
	_spill_ts->setDataInterval( Method :: getTimeInterval(),
		Method :: getTimeMult() );
	if( _spill_ts->allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for spill time series on %s.", _id );
		return( STATUS_FAILURE );
	}
	/*****_withdraw_ts = new HourTS();
	_withdraw_ts->setDate1( Method :: getForecastDate1() );
	_withdraw_ts->setDate2( Method :: getForecastDate2() );
	_withdraw_ts->setDataInterval( Method :: getTimeInterval(),
		Method :: getTimeMult() );
	if( _withdraw_ts->allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for loss time series on %s.", _id );
		return( STATUS_FAILURE );
	}*****/

	_storage_ts.setDate1( Method :: getForecastDate1() );
	_storage_ts.setDate2( Method :: getForecastDate2() );
	_storage_ts.setDataInterval( Method :: getTimeInterval(),
		Method :: getTimeMult() );
	if( _storage_ts.allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for storage time series on %s.", _id );
		return( STATUS_FAILURE );
	}

	_pool		= MISSING;
	_min_pool	= MISSING;
	_release	= 0.0;
	_spill  	= 0.0;
	_min_release	= MISSING;
	_storage	= MISSING;
	_loss		= 0.0;
	_withdraw	= 0.0;

	_inflowCO	= 0.0;
	_releaseCO	= 0.0;	// Will be set same as _release after 
					// assignment from INITIALRELEASE
	_meanOutflowCO	= MISSING;
	_poolCO		= MISSING;	// Will be set same as _pool after
					// assignment from INITIALPOOL
	_prevPoolCO	= MISSING;	// Will default to the value for _poolCO
					// (if available) unless explicity 
					// assigned
	_withdrawCO	= 0.0;		// Will be set same as _withdraw after
					// assignment from INITIALWITHDRAW

	_prevInflowCO	= 0.0;
	_prevReleaseCO	= 0.0;
	_prevWithdrawCO	= 0.0;

	// The following values are required for Rule states.  There may be
	// some duplication with variables above.  Perhaps someday we will
	// eliminate the duplication.
	_endInflow	= MISSING;	// The actual value will be calculated 
					// at the beginning of each timestep.
	_startInflow 	= 0.0;	// Will be assigned to value of _inflowCO
	_prevInflow 	= 0.0;	// Will be assigned to value of _prevInflowCO

	_endPool	= MISSING;	// The actual value will be calculated 
					// at the end of each timestep.
	_startPool 	= 0.0;	// Will be assigned to value of _poolCO
	_prevPool 	= 0.0;	// Will be assigned to value of _prevPoolCO

	_endWithdraw	= MISSING;	// The actual value will be calculated 
					// at the end of each timestep.
	_startWithdraw 	= 0.0;	// Will be assigned to value of _withdrawCO
	_prevWithdraw 	= 0.0;	// Will be assigned to value of _prevWithdrawCO

	_endRelease	= MISSING;	// The actual value will be calculated 
					// at the end of each timestep.
	_startRelease 	= 0.0;	// Will be assigned to value of _releaseCO
	_prevRelease 	= 0.0;	// Will be assigned to value of _prevReleaseCO

	// The following values were added to follow the above Rule states
	// convention in preparation for storage usage in the Balance method.
	_endStorage	= MISSING;	// The actual value will be calculated 
					// at the end of each timestep.
	_startStorage 	= 0.0;	// Will be assigned to value determined from
				// _poolCO
	_prevStorage 	= 0.0;	// Will be assigned to value determined from
				// _prevPoolCO

 	return(STATUS_SUCCESS);

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reservoir_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_initialize.cxx,v 1.7 2006/10/26 15:32:32 hsu Exp $";}
/*  ===================================================  */

}
