//------------------------------------------------------------------------------
// SetRelease::SetRelease - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998	Daniel Weiler, Riverside Technology, inc
//				Created initial version.
// 04 Mar 1998	Matthew J. Rutherford, RTi
//				Changed so that initialize is called before
//				the _owner is set.
// 22 Apr 1998 	DKW		Derived from ReservoirMethod
// 12 Mar 2004	James R. VanShaar, RTi	Added handling of weekly variation.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetRelease.h"

SetRelease::SetRelease( Reservoir* owner ) : ReservoirMethod( owner )
{
	initialize();
}
SetRelease::SetRelease( const SetRelease& meth, Reservoir* owner ) : 
	ReservoirMethod( meth, owner )
{
	char	routine[]="SetRelease::SetRelease";
	int i;

	initialize();

	// Copy integers first...
	_n_elev = meth._n_elev;
	_n_blend_tbl = meth._n_blend_tbl;
	_n_blend_ts = meth._n_blend_ts;
	_tbl_step = meth._tbl_step;
	_ts_step = meth._ts_step;

	// Allocate some memory for the elevation, and release, arrays...
	_elev = new double[ _n_elev ];
	if( !_elev ) {
		PrintWarning( 1, routine, "Unable to allocate %d floats.",
			_n_elev );
	}
	_release_ctl = new DistributedTS[ _n_elev ];
	if( !_release_ctl ) {
		PrintWarning( 1, routine, "Unable to allocate %d DistributedTS.",
		_n_elev );
	}

	// Arrays...
	for( i = 0; i < _n_elev; i++ ) {
		_elev[i] = meth._elev[i];
		_release_ctl[i] = meth._release_ctl[i];
	}

	// Allocate some memory and fill the weekly variation arrays...
	if( _inWeekScalars != NULL ) {
		// Determine how many values should be given
		int requiredSize = ( 24 / _t_mult ) * 7;

		// Size the array.
		_inWeekScalars =  new double[ requiredSize ];

		// Populate the array.
		for( i = 0; i < requiredSize; i++ ) {
			_inWeekScalars[i]=meth._inWeekScalars[i];
		}
	}

	// time series
	if( meth._release_obs != NULL ) {
		_release_obs = new HourTS( *(meth._release_obs) );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetRelease_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: SetRelease_Constructors.cxx,v 1.5 2006/10/26 15:34:05 hsu Exp $";}
/*  ===================================================  */

}
