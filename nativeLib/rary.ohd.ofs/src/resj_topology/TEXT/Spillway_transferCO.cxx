//------------------------------------------------------------------------------
// Spillway::transferCO - Transfers carry over parameters for a Spillway 
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
// 15 Jan 2003	James R. VanShaar, Riverside Technology, Inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// @params methOLD Method object from old resj system definition (not used here)
// @params cOLD character string pointing to the portion of the old C array 
//	   containing carryover for the active Spillway method.
// @params cNEW character string pointing to the portion of the new C array 
//	   containing carryover for the active Spillway method.
// ############ Check the rest for applicability
// @returns ierr integer representing errors occuring or significant warnings:
//	   0 = no problems
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "Spillway.h"
#include <string.h>

int Spillway :: transferCO ( Method * methOLD, char * cOLD, char * cNEW,
	int * ipr )

	// Currently (01/03) Spillway carryover data consists of:
	//   1)  The initial spill, which is the value assigned to _myValue
	//
	//   The initial transfer value is transferred directly, with no extra 
	//   consideration.

{
	int ierr = 0, length;
	char routine[]="Spillway::transferCO", message[256], partA[256], 
		partB[256], *temp;
	int numFloats = 3+3+1+3+3; // number of float positions in the C array
				// used to store 'METHOD', method_ID, the index 
				// to the next carryover the method type 
				// "SPILLWAY" and the owning Component id.
	int index = numFloats * sizeof(float); 	// Defines the position
       						// after all the labels and
						// the next index
	
	// Simply copy the INITIALTRANSFER value from the old to the new 
	// carryover array.
	strncpy( &cNEW[index], &cOLD[index], 8 );

	// Handle any errors
	if ( ierr > 0 ) {
		sprintf (message, "Spillway string copy error for %s.",_id);
		length = strlen (message);
		ResJ_fwrite( &length, message, ipr );
	}
	
	return ierr;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_transferCO.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_transferCO.cxx,v 1.2 2006/10/26 15:36:25 hsu Exp $";}
/*  ===================================================  */

}

