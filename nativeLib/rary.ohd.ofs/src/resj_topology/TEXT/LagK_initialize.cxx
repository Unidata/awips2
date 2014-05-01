//------------------------------------------------------------------------------
// LagK :: initialize - initialized instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Mar 1998	Daniel Weiler, Riverside Technology, inc
//				Created initial version.
// 12 Nov 2001	James R. VanShaar, RTi	Added _hasStates initialization.
// 19 Nov 2001	JRV, RTi	Added _outflowCO initialization.
// 12 Jan 2003	JRV, RTi	Added _n_OutStorVals
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "LagK.h"

int LagK :: initialize()
{

	strcpy( _type, "LAGK" );

	_lag = 0;
	_kconst = 0;

	_n_kval = 0;
	_n_OutStorVals = 0;

	_co_inflow = NULL; 
	_outflowCO = 0.0; 

	_k_mode = -1;

	_interp = 0;

	_hasStates = 1;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_initialize.cxx,v 1.5 2006/10/26 15:22:49 hsu Exp $";}
/*  ===================================================  */

}
