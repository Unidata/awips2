//------------------------------------------------------------------------------
// Spillway :: initialize - initialized instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Dec 2002	James R. VanShaar, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Spillway.h"

int Spillway :: initialize()
{
	strcpy( _type, "SPILLWAY" );

	_stor_spill_tbl = new Table;

	_intervals = 0;
	_myValue = 0.0;

//	_group_id = SPILLWAY_METHOD;
	_group_id = RELEASE_METHOD;

	_withVol_ts = NULL;
	_relVol_ts = NULL;
	_stor_ts = NULL;
	_spillVol_ts = NULL;
	_inflVol_ts = NULL;

	_hasStates = 1;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_initialize.cxx,v 1.3 2006/10/26 15:36:16 hsu Exp $";}
/*  ===================================================  */

}
