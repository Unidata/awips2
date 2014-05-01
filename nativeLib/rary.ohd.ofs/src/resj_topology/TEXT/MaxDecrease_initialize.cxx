//------------------------------------------------------------------------------
// MaxDecrease :: initialize - initialized instance data members.
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
// 18 Nov 2003	James R. VanShaar, RTi	Added _hasStates initialization.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "MaxDecrease.h"

int MaxDecrease :: initialize()
{
	strcpy( _type, "MAXDECREASE" );
	_group_id = RELEASE_METHOD;
	_max_decrease = MISSING;

	_hasStates = 0;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxDecrease_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: MaxDecrease_initialize.cxx,v 1.4 2006/10/26 15:26:05 hsu Exp $";}
/*  ===================================================  */

}
