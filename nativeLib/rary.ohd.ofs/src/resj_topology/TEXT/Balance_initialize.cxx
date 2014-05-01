//------------------------------------------------------------------------------
// Balance :: initialize - initialized instance data members.
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
// 24 Apr 1998	Daniel Weiler, RTi	Initialized data members.
// 12 Nov 2001	James R. VanShaar, RTi	Added _hasStates initialization.
// 12 Jun 2002	JRV, RTi	Added _max_rel initialization.
// 13 Jun 2002	JRV, RTi	Added _totBalStor, _ace initialization.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Balance.h"

int Balance :: initialize()
{
	char routine[] = "Balance :: initialize";
	int i, j;
	strcpy( _type, "BALANCE" );
	_group_id = RELEASE_METHOD;
	_owner_pos = -1;

	_lower_stor = new double [MAX_COMPONENT];
	_upper_stor = new double [MAX_COMPONENT];
	_min_rel = new double [MAX_COMPONENT];
	_max_rel = new double [MAX_COMPONENT];

	_balance_res = new Reservoir* [MAX_COMPONENT];
        if( !_balance_res ) {
		PrintWarning( 1, routine, "Couldn't allocate %d Reservoir "
		"pointers.", MAX_COMPONENT );
		return( STATUS_FAILURE );
	}

	for( i = 0; i < MAX_COMPONENT; i++ ) {
		_upper_stor[i] = MISSING;
		_lower_stor[i] = MISSING;
		_min_rel[i] = MISSING;
		_max_rel[i] = MISSING;
		_balance_res[i] = NULL;
	}

	_bal_mode = 0;
	_n_res = 0;
	_ace = 1.0;
	_totBalStor = 0.0;

	_hasStates = 0;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Balance_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Balance_initialize.cxx,v 1.4 2006/10/26 15:11:17 hsu Exp $";}
/*  ===================================================  */

}
