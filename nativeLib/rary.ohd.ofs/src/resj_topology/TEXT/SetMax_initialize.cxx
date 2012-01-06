//------------------------------------------------------------------------------
// SetMax :: initialize - initialized instance data members.
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
// 12 Nov 2001	James R. VanShaar, RTi	Added _hasStates initialization.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetMax.h"

int SetMax :: initialize()
{
	strcpy( _type, "SETMAX" );

	_hasStates = 0;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetMax_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: SetMax_initialize.cxx,v 1.3 2006/10/26 15:33:31 hsu Exp $";}
/*  ===================================================  */

}
