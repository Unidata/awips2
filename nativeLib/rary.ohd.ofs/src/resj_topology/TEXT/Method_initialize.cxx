//------------------------------------------------------------------------------
// Method :: initialize - initialized instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 26 Jul 2001	James R. VanShaar, RTi	Added _ordered initialization
// 16 Jan 2003	JRV, RTi	Initialized _Active
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Method.h"
#include "ResJSys.h"

int Method :: initialize()
{
	_id[0] = '\0';
	_type[0] = '\0';
	_group_id = 0;
	_is_constructed = 0;

	// Default _Active to 0.  It will be copied or set otherwise, as needed
	_Active = 0;

	_mode = NORMAL;

	_ordered = 0;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Method_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Method_initialize.cxx,v 1.5 2006/10/26 15:27:21 hsu Exp $";}
/*  ===================================================  */

}
int Method :: _t_int=0;
int Method :: _t_mult=0;
int Method :: _units=SI;
double Method :: _t_step_in_sec;
TSDate Method :: _forecast_date1;
TSDate Method :: _forecast_date2;
