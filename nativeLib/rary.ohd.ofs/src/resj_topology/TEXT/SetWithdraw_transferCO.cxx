//------------------------------------------------------------------------------
// SetWithdraw::transferCO - Transfers carry over parameters for a SetWithdraw 
// 				method
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//
//------------------------------------------------------------------------------
// History:
// 
// 18 Dec 2002	James R. VanShaar, Riverside Technology, Inc
//					Created initial version.
// 28 Mar 2008	JRV, RTi	Fixed erroneous indexing of INITIALTRANSFER.
//				Previously, the index did not account for blend
//				counter space.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// @params methOLD Method object from old resj system definition (not used here)
// @params cOLD character string pointing to the portion of the old C array 
//	   containing carryover for the active SetWithdraw method.
// @params cNEW character string pointing to the portion of the new C array 
//	   containing carryover for the active SetWithdraw method.
// ############ Check the rest for applicability
// @returns ierr integer representing errors occuring or significant warnings:
//	   0 = no problems
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "SetWithdraw.h"
#include <string.h>

int SetWithdraw :: transferCO ( Method * methOLD, char * cOLD, char * cNEW,
	int * ipr )

	// Currently (12/02) SetWithdraw carryover data consists of:
	//   1)  The time step counter for the time series blend at the next 
	//   time step for which blending is required.
	//   2)  The time step counter for the table blend at the next time
	//   step for which blending is required
	//   3)  The initial transfer to a component, which is the value 
	//   assigned to _myValue
	//
	//   Of the three, only the initial transfer value is transferred.
	//      This value is transferred directly, with no extra consideration.
	//      It has no meaning if there is no ToComp parameterization on the
	//      new method, and still exists if the old method had no ToComp 
	//      parameterization.

{
	// NOTE: The following variable definitions are based entirely on
	//	only transferring INFLOWS unless otherwise stated.

	int ierr = 0, length;
	char routine[]="SetWithdraw::transferCO", message[256], partA[256], 
		partB[256], *temp;
	int numFloats = 3+3+1+3+3+1+1;
		// number of float positions in the C array used to store
		// 'METHOD', method_ID, the index to the next carryover the
		// method type "SETWITHDRAW", the owning Component id, the time
		// series blend counter and the table blend counter.
	int index = numFloats * sizeof(float); 	// Defines the position
       						// after all the labels and
						// the next index
	
	// Simply copy the INITIALTRANSFER value from the old to the new 
	// carryover array.
	strncpy( &cNEW[index], &cOLD[index], 8 );

	// Handle any errors
	if ( ierr > 0 ) {
		sprintf (message, "SetWithdraw string copy error for %s.",_id);
		length = strlen (message);
		ResJ_fwrite( &length, message, ipr );
	}
	
	return ierr;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetWithdraw_transferCO.cxx,v $";
 static char rcs_id2[] = "$Id: SetWithdraw_transferCO.cxx,v 1.2 2006/10/26 15:35:28 hsu Exp $";}
/*  ===================================================  */

}

