//------------------------------------------------------------------------------
// Balance :: Balance - Constructors.
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
// 22 Apr 1998  Daniel Weiler, RTi	Dervied from ReservoirMethod.
// 05 May 1998	DKW			Enabled copy constructor.
// 12 Jun 2002	James R. VanShaar, RTi	Added _max_rel.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Balance.h"

Balance :: Balance( Reservoir* owner ) : ReservoirMethod( owner )
{
	if( initialize() ) {
		PrintWarning( 1, "Balance Constructor", "Troubles initializing"
		" Balance method." );
	}
}

Balance :: Balance( const Balance& meth, Reservoir* owner ) : 
	ReservoirMethod( meth, owner )
{
	char	routine[]="Balance :: Balance(copy)";
	int i;

	initialize();

	// Copy integers
	_bal_mode = meth._bal_mode;
	_n_res = meth._n_res;
	_owner_pos = meth._owner_pos;

	// doubles and Reservoirs in the method...
	for( i = 0; i < _n_res; i++ ) {
		_lower_stor[i] = meth._lower_stor[i];
		_upper_stor[i] = meth._upper_stor[i];
		_min_rel[i] = meth._min_rel[i];
		_max_rel[i] = meth._max_rel[i];
		_balance_res[i] = (Reservoir*)(owner->getComponentPtr( 
			meth._balance_res[i]->getID() ) );
	}


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Balance_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Balance_Constructors.cxx,v 1.4 2006/10/26 15:08:47 hsu Exp $";}
/*  ===================================================  */

}
