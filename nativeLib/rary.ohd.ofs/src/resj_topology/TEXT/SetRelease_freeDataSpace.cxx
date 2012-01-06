//------------------------------------------------------------------------------
// SetRelease::freeDataSpace - delete dynamically allocated memory.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 14 Feb 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 09 Mar 2004	James R. VanShaar, RTi	Added _inWeekScalars deletion.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetRelease.h"

int SetRelease::freeDataSpace()
{
	int i;
	
	if( _release_ctl != NULL ) {
		delete [] _release_ctl;
	}
	if( _inWeekScalars != NULL ) {
		delete [] _inWeekScalars;
	}
	if( _elev != NULL ) {
		delete [] _elev;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetRelease_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: SetRelease_freeDataSpace.cxx,v 1.4 2006/10/26 15:34:22 hsu Exp $";}
/*  ===================================================  */

}
