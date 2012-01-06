//------------------------------------------------------------------------------
// CalcInflow::initialize - initialized instance data members.
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

int CalcInflow::initialize()
{
	char routine[] = "CalcInflow::print";
	strcpy( _type, "CALCINFLOW" );
	_group_id = INFLOW_METHOD;

	_hasStates=1;

	_pool_obs = NULL;
	_release_obs = NULL;
	_withdraw_obs = NULL;
	_inflow_calc = NULL;

	_outputCalcTS = 0;
	_useForSim = 0;
	_constrainChange = 0;
	_meanToInstErr = 0;
	_isCopy = 0;

	_mode = NORMAL;

	_minInflow = MISSING;
	_remainingVol = 0;
	_startPool = MISSING;
	_startRelease = MISSING;
	_startWithdrawal = MISSING;
	_startInflow = MISSING;
	_useLoss = 0;

	// Create the calculated inflow timeseries
	_inflow_calc = new HourTS();
	_inflow_calc->setDate1( Method :: getForecastDate1() );
	_inflow_calc->setDate2( Method :: getForecastDate2() );
	_inflow_calc->setDataInterval( Method :: getTimeInterval(),
		Method :: getTimeMult() );
	if( _inflow_calc->allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space "
			"for calculated inflow time series on %s.", _id );
		return( STATUS_FAILURE );
	}
	
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_initialize.cxx,v 1.2 2006/10/26 15:11:47 hsu Exp $";}
/*  ===================================================  */

}
