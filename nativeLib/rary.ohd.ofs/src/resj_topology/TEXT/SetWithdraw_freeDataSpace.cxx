//------------------------------------------------------------------------------
// SetWithdraw :: freeDataSpace - delete dynamically allocated memory.
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
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetWithdraw.h"

int SetWithdraw :: freeDataSpace()
{
	int i;
	
	if( _withdraw_ctl != NULL ) {
		delete [] _withdraw_ctl;
	}
	if( _elev != NULL ) {
		delete [] _elev;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetWithdraw_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: SetWithdraw_freeDataSpace.cxx,v 1.2 2006/10/26 15:35:18 hsu Exp $";}
/*  ===================================================  */

}
