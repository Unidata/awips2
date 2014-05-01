//------------------------------------------------------------------------------
// Node::transferCO - Transfers carry over parameters for a Node
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Nov 2001	James R. VanShaar, Riverside Technology, Inc
//					Created initial version.
// 24 Feb 2003	JRV, RTi	Revised parameters passed to be consistent with
// 				Component carryover, despite not being a source
// 				of error.
// 28 Mar 2006  JRV, RTi    Added handling of INFLOW and DIVERSION states.
//                          Certain other reformatting.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// @params compOLD Component object from old resj system definition (not used here)
// @params cOLD character string pointing to the portion of the old C array 
//	   containing carryover for the active Node component.
// @params cNEW character string pointing to the portion of the new C array 
//	   containing carryover for the active Node component.
// ############ Check the rest for applicability
// @returns ierr integer representing errors occuring or significant warnings:
//	   0 = no problems
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "Node.h"
#include <string.h>

int Node :: transferCO ( Component * compOLD, char * cOLD, char * cNEW,
	int * ipr )

	// Previously, (11/01) Node carryover data consisted of:
	//   A) discharge
	//   B) discharge at the time step previous to writing carry over.
	//   C) unused place holders

	// Currently (03/2006) Node carryover data consists of:
	//   A) discharge
	//   B) discharge at the time step previous to writing carry over.
	//   C) initial inflow (at the time of carryover writing).
	//   D) previous inflow (at the time step previous to carryover
	//       writing).
	//   E) initial diversion (at the time of carryover writing).
	//   F) previous diversion (at the time step previous to carryover
	//       writing).
	//   G) one unused place holder

	// The carry over transfer is direct copying of data values.  
	//	discharge(NEW) = discharge(OLD)
	//	prevDischarge(NEW) = prevDischarge(OLD)
	//	inflow(NEW) = inflow(OLD)
	//	prevInflow(NEW) = prevInflow(OLD)
	//	diversion(NEW) = diversion(OLD)
	//	prevDiversion(NEW) = prevDiversion(OLD)

	// If the old carryover array still has old format data, we will not
	// transfer inflows and diversions.
{
	int ierr = 0, length;
	char routine[]="Node::transferCO", message[256], partA[256], 
		partB[256];

	// Set some index values for each carryover section
	// NOTE: These will need changing if anything should modify the
	//	structure of the CO array (ie. include inflows)
	int numKeyFloats = 3;	// Number of floating point values used to
				// hold the Component Keyword within the
				// carryover array

	int numNameFloats = 3;	// Number of floating point values used to
				// hold the Component name within the carryover 
				// array

	int numNextFloats = 1;	// Number of floating point values used to hold
				// the location of the next component within
				// the ResJ carryover array.

	int numDataFloats = 2;	// Number of floating point values used to store
				// one data value.

	int compI = 0;		// Index to the Component Keyword;
	int nameI = numKeyFloats * sizeof(float);
				// Index to the beginning of the Component name
	int nextI = nameI + numNameFloats * sizeof(float);
				// Index to the location of the index for the 
				// next component keyword.

	int dischI = nextI + numNextFloats*sizeof(float);
	int prevDischI = dischI + numDataFloats*sizeof(float);
	int inflI = prevDischI + numDataFloats*sizeof(float);
	int prevInflI = inflI + numDataFloats*sizeof(float);
	int diversI = prevInflI + numDataFloats*sizeof(float);
	int prevDiversI = diversI + numDataFloats*sizeof(float);
	int endI = prevDiversI + numDataFloats*sizeof(float);

	char temp[9];
	temp[8] = '\0';

	// Copy discharge
	if ( strncpy(temp, &cOLD[dischI], (prevDischI-dischI)) == NULL ) {
		// fatal error
		ierr = 1;
	}
	if ( strncpy(&cNEW[dischI], temp, (prevDischI-dischI)) == NULL ) {
		// fatal error
		ierr = 1;
	}
	
	// Copy previous discharge
	if ( strncpy(temp, &cOLD[prevDischI], (inflI-prevDischI)) == NULL ) {
		// fatal error
		ierr = 1;
	}
	if ( strncpy(&cNEW[prevDischI], temp, (inflI-prevDischI)) == NULL ) {
		// fatal error
		ierr = 1;
	}

	// Copying of Inflow and Diversion states depends on whether the OLD
	// carryover is stocked in the NEW fashion.
	if( strncasecmp( &cOLD[inflI], "*FUTURE*", 8 ) )
	{
	    // We didn't match.  We have new carryover in the old array.

	    // Copy inflow
	    if ( strncpy(temp, &cOLD[inflI], (prevInflI-inflI)) == NULL )
	    {
	        // fatal error
	        ierr = 1;
	    }
	    if ( strncpy(&cNEW[inflI], temp, (prevInflI-inflI)) == NULL )
	    {
	        // fatal error
	        ierr = 1;
	    }

	    // Copy previous inflow
	    if ( strncpy(temp, &cOLD[prevInflI], (diversI-prevInflI)) == NULL )
	    {
	        // fatal error
	        ierr = 1;
	    }
	    if ( strncpy(&cNEW[prevInflI], temp, (diversI-prevInflI)) == NULL )
	    {
	        // fatal error
	        ierr = 1;
	    }

	    // Copy diversion
	    if ( strncpy(temp, &cOLD[diversI], (prevDiversI-diversI)) == NULL )
	    {
	        // fatal error
	        ierr = 1;
	    }
	    if ( strncpy(&cNEW[diversI], temp, (prevDiversI-diversI)) == NULL )
	    {
	        // fatal error
	        ierr = 1;
	    }

	    // Copy previous diversion
	    if ( strncpy(temp, &cOLD[prevDiversI], (endI-prevDiversI)) == NULL )
	    {
	        // fatal error
	        ierr = 1;
	    }
	    if ( strncpy(&cNEW[prevDiversI], temp, (endI-prevDiversI)) == NULL )
	    {
	        // fatal error
	        ierr = 1;
	    }
	}
	// Otherwise, we don't have any carryover to transfer, we will keep the
	// values in the new carryover array (which include inflows and
	// diversions).

	// Handle any errors
	if ( ierr > 0 ) {
		sprintf (message, "Node string copy error for %s.",_id);
		PrintError( routine, message );
	}
	
	return ierr;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Node_transferCO.cxx,v $";
 static char rcs_id2[] = "$Id: Node_transferCO.cxx,v 1.4 2006/10/26 15:28:12 hsu Exp $";}
/*  ===================================================  */

}

