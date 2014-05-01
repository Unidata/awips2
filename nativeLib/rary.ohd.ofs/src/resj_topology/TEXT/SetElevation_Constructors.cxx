//------------------------------------------------------------------------------
// SetElevation :: SetElevation - Constructors.
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
// 22 Apr 1998	Daniel Weiler, RTi	Derived from ReservoirMethod.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetElevation.h"
#include "Reservoir.h"

SetElevation :: SetElevation( Reservoir* owner ) : ReservoirMethod( owner )
{
	initialize();
}

SetElevation :: SetElevation( const SetElevation& meth, Reservoir* owner )
	: ReservoirMethod( meth, owner )
{
	char	routine[]="SetElevation :: SetElevation";

	initialize();

	// Copy the integers...
	_n_blend_tbl = meth._n_blend_tbl;
	_n_blend_ts = meth._n_blend_ts;
	_tbl_step = meth._tbl_step;
	_ts_step = meth._ts_step;

	// time series 
	_elev_ctl = meth._elev_ctl;
	if( meth._pool_obs != NULL ) {
		_pool_obs = new HourTS( *(meth._pool_obs) );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetElevation_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: SetElevation_Constructors.cxx,v 1.3 2006/10/26 15:32:49 hsu Exp $";}
/*  ===================================================  */

}
