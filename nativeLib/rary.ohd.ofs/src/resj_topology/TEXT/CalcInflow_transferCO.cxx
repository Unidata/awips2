//------------------------------------------------------------------------------
// CalcInflow::transferCO - Transfers carry over parameters for a CalcInflow 
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
// 11 Mar 2004	James R. VanShaar, Riverside Technology, Inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// @params methOLD Method object from old resj system definition (not used here)
// @params cOLD character string pointing to the portion of the old C array 
//	   containing carryover for the active CalcInflow method.
// @params cNEW character string pointing to the portion of the new C array 
//	   containing carryover for the active CalcInflow method.
// ############ Check the rest for applicability
// @returns ierr integer representing errors occuring or significant warnings:
//	   0 = no problems
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "CalcInflow.h"
#include <string.h>

int CalcInflow :: transferCO ( Method * methOLD, char * cOLD, char * cNEW,
	int * ipr )

//       _remainingVol, _startInflow, _startPool, _startRelease, _startWithdrawal
	// Currently (03/04) CalcInflow carryover data consists of:
	//   1)  The remaining volume that has not been applied to the mass
	//   balance due to some form of constraint on the calculated inflow.
	//   This value will default to 0 if not specified by the user or 
	//   actually determined to be otherwise during the constraining portion
	//   of solution.
	//   2)  The starting inflow to be used in constraining work.  If no
	//   constraints are applied, we still store this value.  If the inflow
	//   was unable to be calculated, the inflow simulated at the owning 
	//   reservoir is used.
	//   3)  The starting pool, taken from an input timeseries.
	//   4)  The starting release, taken from an input timeseries.  If no 
	//   timeseries exists, this will be MISSING.
	//   5)  The starting withdrawal, taken from an input timeseries.  If no 
	//   timeseries exists, this will be MISSING.
	//
	//   Of the five, the first three are transferred directly, with no
	//   extra consideration.  
	//   The last two will transfer directly, UNLESS, the old 
	//   parameterization does not contain the related timeseries.

{
	// NOTE: The following variable definitions are based entirely on
	//	only transferring INFLOWS unless otherwise stated.

	int ierr = 0, length;
	char routine[]="CalcInflow::transferCO", message[256], partA[256], 
		partB[256], *temp;
	int numFloats = 3+3+1+3+3; // number of float positions in the C array
				// used to store 'METHOD', method_ID, the index 
				// to the next carryover the method type 
				// "SETWITHDRAW" and the owning Component id.
	int numDataFloats = 2;	// Number of floating point values used to store
				// one data value.
			// NOTE: remainingVol requires twice this!

	int remVI = numFloats * sizeof(float); 	// Defines the position
       						// after all the labels and
						// the next index
	int inflI = remVI + 2*numDataFloats*sizeof(float);
	int poolI = inflI + numDataFloats*sizeof(float);
	int relsI = poolI + numDataFloats*sizeof(float);
	int withI = relsI + numDataFloats*sizeof(float);

	
	// Simply copy the REMAININGVOL, STARTINFLOW, STARTPOOL value from the 
	// old to the new carryover array.
	strncpy( &cNEW[remVI], &cOLD[remVI], 16 );
	strncpy( &cNEW[inflI], &cOLD[inflI], 8 );
	strncpy( &cNEW[poolI], &cOLD[poolI], 8 );

	// Check to see if we should copy the STARTRELEASE
	if( ( (CalcInflow *)methOLD )->_release_obs != NULL ) {
		// Transfer
		strncpy( &cNEW[relsI], &cOLD[relsI], 8 );
	}

	// Check to see if we should copy the STARTWITHDRAWAL
	if( ( (CalcInflow *)methOLD )->_withdraw_obs != NULL ) {
		// Transfer
		strncpy( &cNEW[withI], &cOLD[withI], 8 );
	}

	// Handle any errors
	if ( ierr > 0 ) {
		sprintf (message, "CalcInflow string copy error for %s.",_id);
		length = strlen (message);
		ResJ_fwrite( &length, message, ipr );
	}
	
	return ierr;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_transferCO.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_transferCO.cxx,v 1.2 2006/10/26 15:11:57 hsu Exp $";}
/*  ===================================================  */

}

