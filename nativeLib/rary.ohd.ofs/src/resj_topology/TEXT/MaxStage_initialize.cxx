//------------------------------------------------------------------------------
// MaxStage :: initialize - initialized instance data members.
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
// 11 May 1998	Daniel Weiler, RTi	Added _dcp, _min_release, _max_stage,
//					_criterion, _next_ds, _sub_root, etc.
// 04 Jun 2001	James R. Vanshaar, RTi	Added _sumLag
// 12 Nov 2001	James R. VanShaar, RTi	Added _hasStates initialization.
// 14 Aug 2007	Darrin Sharp, RTi	Added MAXIMUMDISCHARGE for Reservoir
//					Tools Enhancement
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "MaxStage.h"

int MaxStage :: initialize()
{
	strcpy( _type, "MAXSTAGE" );
	_group_id = RELEASE_METHOD;

	_dcp = NULL;
	_min_release = MISSING; 
	_max_stage = MISSING;
	_max_discharge = MISSING;
	_maxflow = MISSING;
	_highestFlow = MISSING;
	_criterion = MISSING;

	_next_ds_comp = NULL;
	_inflow_pos = -1;
	_max_iter = 20;
	_n_tstep = 0;

	_sumLag = 0;
	_sumK = 0;
	_hasStates = 0;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxStage_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: MaxStage_initialize.cxx,v 1.4 2006/10/26 15:26:58 hsu Exp $";}
/*  ===================================================  */

}
