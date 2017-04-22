//------------------------------------------------------------------------------
// Reservoir :: Reservoir - constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 31 May 2001	James R. VanShaar, RTi	Added copying of carryover values in
//					copy constructor
// 2003-11-24 LT and JRV, RTi - Added 'delete _pool_ts;', 'delete _release_ts;'
//				and 'delete _withdraw_ts;' to prevent memory
//				leaks (temporarely).
//				These change need revision. A better approach
//                              may be to move the code initializing these three
//				member from the 'initialize' method to the 
//                              default constructor and simply create these new
//				timeseries objects in the overload constructor
//				where the test for NULL and the 'delete' entries
//				may not be needed because these members are 
//				initialized as NULL in the 'initialize' method.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reservoir.h"

Reservoir :: Reservoir() : Component() 
{
	initialize();
}

Reservoir :: Reservoir( const Reservoir& res ) : Component( res )
{
	initialize();

	// All of the Time Series
	_elev_stor_tbl = res._elev_stor_tbl;
	if( res._pool_ts != NULL ) {
		delete _pool_ts; 	// 2003-11-24 LT and JRV
		_pool_ts = new HourTS( *(res._pool_ts) );
	}
	if( res._release_ts != NULL ) {
		delete _release_ts; 	// 2003-11-24 LT and JRV
		_release_ts = new HourTS( *(res._release_ts) );
	}
	if( res._spill_ts != NULL ) {
		delete _spill_ts; 	// 2003-11-24 LT and JRV
		_spill_ts = new HourTS( *(res._spill_ts) );
	}
	if( res._withdraw_ts != NULL ) {
		delete _withdraw_ts; 	// 2003-11-24 LT and JRV
		_withdraw_ts = new HourTS( *(res._withdraw_ts) );
	}
	_loss_ts = HourTS( res._loss_ts );
	_storage_ts = HourTS( res._storage_ts );

	// doubles
	_pool = res._pool;
	_release = res._release;
	_spill = res._spill;
	_withdraw = res._withdraw;
	_loss = res._loss;
	_storage = res._storage;
	_min_release = res._min_release;
	_min_pool = res._min_pool;

	_inflowCO = res._inflowCO;
	_releaseCO = res._releaseCO;
	_meanOutflowCO = res._meanOutflowCO;
	_poolCO = res._poolCO;
	_prevPoolCO = res._prevPoolCO;
	_withdrawCO = res._withdrawCO;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reservoir_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_Constructors.cxx,v 1.5 2006/10/26 15:32:04 hsu Exp $";}
/*  ===================================================  */

}

