//------------------------------------------------------------------------------
// SetElevation :: initialize - initialized instance data members.
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
// 22 Apr 1998  Daniel Weiler, RTi	Added first cut at data members.
// 12 Nov 2001	James R. VanShaar, RTi	Added _hasStates initialization.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetElevation.h"

int SetElevation :: initialize()
{
	strcpy( _type, "SETELEVATION" );
	_elev_ctl.setRelativeFlag( 1 );
	_group_id = RELEASE_METHOD;

	_pool_obs = NULL;
	_n_blend_tbl = -1;
	_n_blend_ts = -1;
	_tbl_step = 1; 
	_ts_step = 1; 

	_hasStates = 1;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetElevation_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: SetElevation_initialize.cxx,v 1.3 2006/10/26 15:33:06 hsu Exp $";}
/*  ===================================================  */

}
