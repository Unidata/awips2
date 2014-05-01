//------------------------------------------------------------------------------
// MaxIncrease :: initialize - initialized instance data members.
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
#include "MaxIncrease.h"

int MaxIncrease :: initialize()
{
	strcpy( _type, "MAXINCREASE" );
	_group_id = RELEASE_METHOD;
	_max_increase = MISSING;

	_hasStates = 0;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxIncrease_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: MaxIncrease_initialize.cxx,v 1.4 2006/10/26 15:26:31 hsu Exp $";}
/*  ===================================================  */

}
