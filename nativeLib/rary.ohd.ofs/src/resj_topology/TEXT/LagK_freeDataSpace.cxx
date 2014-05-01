//------------------------------------------------------------------------------
// LagK :: freeDataSpace - frees memory from data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 07 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "LagK.h"

int LagK :: freeDataSpace()
{
	char routine[]="LagK :: freeDataSpace";

	if( _co_inflow ) {
		delete [] _co_inflow;
	}
 	
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_freeDataSpace.cxx,v 1.3 2006/10/26 15:22:33 hsu Exp $";}
/*  ===================================================  */

}
