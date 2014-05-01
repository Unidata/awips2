//------------------------------------------------------------------------------
// RainEvap :: RainEvap - Constructors.
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
// 04 Mar 1998  Matthew J. Rutherford, RTi
//                              Changed so that initialize is called before
//                              the _owner is set.
// 22 Apr 1998  DKW		Derived from ReservoirMethod.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "RainEvap.h"
#include "Reservoir.h"

RainEvap :: RainEvap( Reservoir* owner ) : ReservoirMethod( owner )
{
	initialize();
}

RainEvap :: RainEvap( const RainEvap& meth, Reservoir* owner ) : 
	ReservoirMethod( meth, owner )
{
	char	routine[]="RainEvap :: RainEvap";

	initialize();

	// integers
	_n_precip = meth._n_precip;
	_n_evap = meth._n_evap;

	// DistributedTS
	_evap_ctl = meth._evap_ctl;
	_precip_ctl = meth._precip_ctl;

	// input TS
	if( meth._precip_obs != NULL ) {
		_precip_obs = new HourTS( *(meth._precip_obs) );
	}
	if( meth._evap_obs != NULL ) {
		_evap_obs = new HourTS( *(meth._evap_obs) );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/RainEvap_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: RainEvap_Constructors.cxx,v 1.3 2006/10/26 15:29:15 hsu Exp $";}
/*  ===================================================  */

}
