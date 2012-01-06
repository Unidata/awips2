//------------------------------------------------------------------------------
// Adjust :: Adjust - Constructors.
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
// 14 Oct 2002	James R. VanShaar, RTi	Added _adjsim.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Adjust.h"
#include "Reservoir.h"

Adjust :: Adjust( Reservoir* owner ) : ReservoirMethod( owner )
{
	initialize();
}

Adjust :: Adjust( const Adjust& meth, Reservoir* owner ) : 
	ReservoirMethod( meth, owner )
{
	char	routine[]="Adjust :: Adjust";

	initialize();

	// Copy the integers
	_n_blend = meth._n_blend;
	_n_tstep = meth._n_tstep;
	_adjsim = meth._adjsim;

	// doubles...
	_last_obs_rel = meth._last_obs_rel;

	// TSDates
	_last_obs = meth._last_obs;

	// input TS
	if( meth._release_obs != NULL ) {
		_release_obs = new HourTS( *(meth._release_obs) );
	}
	if( meth._pool_obs != NULL ) {
		_pool_obs = new HourTS( *(meth._pool_obs) );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Adjust_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Adjust_Constructors.cxx,v 1.4 2006/10/26 14:23:18 hsu Exp $";}
/*  ===================================================  */

}
