//------------------------------------------------------------------------------
// SetRelease::initialize - initialized instance data members.
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
//				Changes so that _owner is initialized here and
//				not checked.
// 20 Apr 1998	DKW		Added blending functionality.
// 12 Nov 2001	James R. VanShaar, RTi	Added _hasStates initialization.
// 09 Mar 2004	JRV, RTi	Added _inWeekScalars initialization.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetRelease.h"

int SetRelease::initialize()
{
	strcpy( _type, "SETRELEASE" );
	_group_id = RELEASE_METHOD;

	_release_ctl = NULL;
	_release_obs = NULL;

	_inWeekScalars = NULL;
	_elev = NULL;
	
	_n_elev = 0;	
	
	_n_blend_tbl = -1;
	_n_blend_ts = -1;
	_tbl_step = 1;
	_ts_step = 1;

	_hasStates = 1;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetRelease_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: SetRelease_initialize.cxx,v 1.5 2006/10/26 15:34:25 hsu Exp $";}
/*  ===================================================  */

}
