//------------------------------------------------------------------------------
// RainEvap :: initialize - initialized instance data members.
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
//				Changed so that _owner is initialized and not
//				checked here.
// 24 Sep 1998	DKW,		Initialized observed precip and evap TS
// 12 Nov 2001	James R. VanShaar, RTi	Added _hasStates initialization.
// 12 Jul 2003	JRV, RTi	Revised interpolation mode to interpolate in
// 				time, rather than no interpolation.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "RainEvap.h"

int RainEvap :: initialize()
{
	strcpy( _type, "RAINEVAP" );
	_group_id = LOSS_METHOD;

	_n_precip = 0;
	_n_evap	= 0;

	_mode = INTERPOLATE_TIME;

	// Declaring the precip and evap TS from the control file as relative...
	_precip_ctl.setRelativeFlag( 1 );
	_evap_ctl.setRelativeFlag( 1 );
	_precip_obs = NULL;
	_evap_obs = NULL;

	_hasStates = 0;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/RainEvap_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: RainEvap_initialize.cxx,v 1.4 2006/10/26 15:29:29 hsu Exp $";}
/*  ===================================================  */

}
