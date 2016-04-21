//------------------------------------------------------------------------------
// SetMin :: initialize - initialized instance data members.
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
#include "SetMin.h"

int SetMin :: initialize()
{
	strcpy( _type, "SETMIN" );
	_hasStates = 0;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetMin_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: SetMin_initialize.cxx,v 1.3 2006/10/26 15:33:56 hsu Exp $";}
/*  ===================================================  */

}
