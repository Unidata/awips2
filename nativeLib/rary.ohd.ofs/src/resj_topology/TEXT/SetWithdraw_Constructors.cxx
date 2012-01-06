//------------------------------------------------------------------------------
// SetWithdraw :: SetWithdraw - Constructors.
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
// 22 Apr 1998	Daniel Weiler, RTi	Derived from ReservoirMethod
// 20 Jun 2001	James R. VanShaar, RTi	Replaced usage of _n_withdraw with
//					_n_elev for consistency with SetRelease
// 13 Dec 2002	JRV, RTi	Added ToComp work.
// 10 Jul 2003	JRV, RTi	Re-included copy of _myValue, as submitted by
// 				Kuang-Shen Hsu.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetWithdraw.h"
#include "Reservoir.h"

SetWithdraw :: SetWithdraw( Reservoir* owner ) : ReservoirMethod( owner )
{
	if( owner == NULL ) {
		PrintWarning( 1, "SetWithdraw :: constructor",
		"Cannot set owner to NULL." );
	}
	initialize();

	// Set the _owner
	_owner = owner;
}

SetWithdraw :: SetWithdraw( const SetWithdraw& meth, Reservoir* owner ) 
	: ReservoirMethod( meth, owner )
{
	char	routine[]="SetWithdraw :: SetWithdraw";
	int i;

	initialize();

	// Copy integers
	_n_elev = meth._n_elev;
	//_n_withdraw = meth._n_withdraw;
	_n_blend_tbl = meth._n_blend_tbl;
	_n_blend_ts = meth._n_blend_ts;
	_tbl_step = meth._tbl_step;
	_ts_step = meth._ts_step;

	// Copy floats
	_myValue = meth._myValue;

	// Allocate some memory for the elevation and withdraw arrays...
	_elev = new double[ _n_elev ];
	//_elev = new double[ _n_withdraw ];
	if( !_elev ) {
		PrintWarning( 1, routine, "Unable to allocate %d floats.",
			_n_elev );
			//_n_withdraw );
	}
	_withdraw_ctl = new DistributedTS[ _n_elev ];
	//_withdraw_ctl = new DistributedTS[ _n_withdraw ];
	if( !_withdraw_ctl ) {
		PrintWarning( 1, routine, "Unable to allocate %d DistributedTS.",
			_n_elev );
			//_n_withdraw );
	}
	// Arrays...
	for( i = 0; i < _n_elev; i++ ) {
	//for( i = 0; i < _n_withdraw; i++ ) {
		_elev[i] = meth._elev[i];
		_withdraw_ctl[i] = meth._withdraw_ctl[i];
	}

	//input time series
	if( meth._withdraw_obs != NULL ) {
		_withdraw_obs = new HourTS( *(meth._withdraw_obs) );
	}

	// Handle to Comp work
	_toCompMode = meth._toCompMode;
	if( &meth._Comp_ts != NULL ) {
		//_Comp_ts = new HourTS( meth._Comp_ts );
	}
	if ( meth._receivingComp ) {
		// Ensure the appropriate Comp exists

		
		// Assign _Comp_ts to the Comp

	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetWithdraw_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: SetWithdraw_Constructors.cxx,v 1.7 2006/10/26 15:35:00 hsu Exp $";}
/*  ===================================================  */

}
