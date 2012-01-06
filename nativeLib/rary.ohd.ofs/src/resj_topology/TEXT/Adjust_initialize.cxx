//------------------------------------------------------------------------------
// Adjust :: initialize - initialized instance data members.
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
// 28 Apr 1998 	Daniel Weiler, RTi	Added first cut at data members.
// 24 Sep 1998 	DKW, RTi	Added _release_obs.
// 08 Sep 1998 	DKW, RTi	Added _last_obs_rel initialization.	
// 12 Nov 2001	James R. VanShaar, RTi	Added _hasStates initialization.
// 14 Oct 2002	JRV, RTi	Added _adjsim initialization.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Adjust.h"

int Adjust :: initialize()
{
	strcpy( _type, "ADJUST" );
	_group_id = RELEASE_METHOD;

	_deviate = 0.0;
	_n_blend = -1;
	_n_tstep = 1;
	_last_obs = Method :: getForecastDate1();
	_release_obs = NULL;
	_pool_obs = NULL;
	_last_obs_rel = MISSING;
	_hasStates = 1;
	_adjsim = 1;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Adjust_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Adjust_initialize.cxx,v 1.5 2006/10/26 15:08:25 hsu Exp $";}
/*  ===================================================  */

}
