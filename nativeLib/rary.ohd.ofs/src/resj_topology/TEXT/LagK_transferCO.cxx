//------------------------------------------------------------------------------
// LagK::transferCO - Transfers carry over parameters for a LagK method
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Apr 2001	James R. VanShaar, Riverside Technology, Inc
//					Created initial version.
// 20 Nov 2001	JRV, RTi	Added transfer of INITIALOUTFLOW
// 18 Dec 2002	JRV, RTi	Modified transfer to handle on new carryover 
// 				structure considering the entire method 
// 				identification set (type, owner_id, method_id).
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// @params methOLD Method object from old resj system definition (not used here)
// @params cOLD character string pointing to the portion of the old C array 
//	   containing carryover for the active LagK method.
// @params cNEW character string pointing to the portion of the new C array 
//	   containing carryover for the active LagK method.
// ############ Check the rest for applicability
// @returns ierr integer representing errors occuring or significant warnings:
//	   0 = no problems
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "LagK.h"
#include <string.h>

int LagK :: transferCO ( Method * methOLD, char * cOLD, char * cNEW,
	int * ipr )

	// Currently (11/01) LagK carryover data consists of:
	//   A) (reach lag / computational time interval) + 1 inflows
	//	with the "oldest" inflow value occurring first, proceeding
	//	to the inflow "current" at the time of carryover output.
	//	NOTE: These values are un-lagged inflows to the reach.  Thus,
	//		only a change in Lag time will affect the carryover.
	//   B) Instantaneous outflow at the point of carry over save.
	//   C) Unused place holders

	// Discussion of INFLOW carry over transfer:

	// The carryover transfer is entirely a function of Lag.
	// For each Lag, the number of carryover values required is calculated
	// according to the following:  
	// 	If (reach_lag % computational_time_interval) == 0, then
	//		nVals = (reach lag / computational time interval) + 1
	//		eg: 12/6+1=3
	//	Otherwise, 
	//		nVals = int((reach_lag)/computational_time_interval)+2
	//		eg: int(15/6)+2=4
	//
	// Using nValsNEW and nValsOLD as decision criteria, the following
	// carryover rules apply:
	//	If nValsNEW == nValsOLD, then
	//		Q[i](NEW) = Q[i](OLD) for 0<=i<nValsOLD
	//
	//	If nValsNEW < nValsOLD, then
	//		diffN = nValsOLD - nValsNEW and
	//		Q[i](NEW) = Q[i+diffN](OLD) for all i, 0<=i<nValsNEW
	//		In essence, this trims off the "oldest" inflows that
	//		with the new, shorter lag time, would have already
	//		passed through the reach.
	//
	//	If nValsNEW > nValsOLD, then
	//		diffN = nValsNEW - nValsOLD and
	//		Q[i](NEW) = 0.000000 for all i, 0<=i<diffN, and
	//		Q[i+diffN](NEW) = Q[i](OLD) for all i, 0<i<=nValsOLD 
	//		In essence, this files the "oldest" (unavailable) 
	//		inflows with 0.000000 although these unavailable inflows
	//		would still be traveling through the reach with the new,
	//		longer lag time.

	// Discussion of OUTFLOW carry over transfer:
	
	// The carry over transfer is direct.  No transformation occurs:
	//	Qout(OLD) = Qout(NEW)
	//
	// The simplicity of this carry over is based primarily on two 
	// arguments:
	// 	1) The discharge from a LagK reach is likely to be observable
	//	inasmuch as some type of observation is required to calibrate
	//	the LagK parameterization.  If the discharge is indeed 
	//	observable, the new outflow should probably be overwritten by
	//	an observed value.  This procedure would require transferring
	//	the inflow values during a preliminary resegdef, punching the
	//	new segment's definition, then using INPUT CO within a 
	//	secondary resegdef wherein the observed outflow is defined.
	//
	//	2) Any back routing of the outflow in an attempt to eliminate
	//	the influence of OLD LagK parameters requires a number of
	//	assumptions whose summed uncertainly is greater than or equal to
	//	the uncertainty of assuming Qout(OLD) = Qout(NEW).  For 
	//	illustrative purposes, assume that we do indeed manage to back
	//	route old parameters to their initial inflows.  We transfer the
	//	inflows directly since they (supposedly) are now devoid of any
	//	old parameterization of LagK.  To route these inflows in the
	//	NEW parameterization to achieve "knowledge" of an outflow to
	//	be stored in new carry over, we must make some assumption about
	//	the outflow prior to the desired outflow, since Qout2 is a
	//	function of Qout1.  The only reasonable assumption would be
	//	based on the old value(s) of Qout.  Accepting this assumption,
	//	we have transferred the uncertainty inherent in Qout(OLD) = 
	//	Qout(NEW).  (Incidently, back routing to the initial inflows
	//	would require additional assumptions.  The careful observer
	//	would postulate that additional carry over stored for sake of
	//	carry over transfer might reduce the uncertaintly.  The more
	//	careful observer would realize that doing so would only
	//	push the need for an assumption of equal gravity further and
	//	further into the historical realm, ultimately reaching the 
	//	previous segment definition's initial conditions.  This at the 
	//	cost of additional software development and carry over storage.)

{
	// NOTE: The following variable definitions are based entirely on
	//	only transferring INFLOWS unless otherwise stated.

	int ierr = 0, length;
	char routine[]="LagK::transferCO", message[256], partA[256], 
		partB[256], *temp;
	int numFloatsOLD = 3+3+3+1; // number of float positions in the C array
				// used to store 'REACH', reach name, the index
				// to the next carryover block keyword, and the
				// LagK method name, 
	int numFloatsNEW = 3+3+3+1+3; // number of float positions in the C array
				// used to store 'REACH', reach name, LagK 
				// method name, the index to the next carryover
				// block keyword, and 'LAGK'
	int indexOLD = numFloatsOLD * sizeof(float); 	// Defines the position
       						// after all the name labels and
						// the next index
	int indexNEW = numFloatsNEW * sizeof(float); 	// Defines the position
       						// after all the name labels and
						// the next index, including 
						// 'LAGK'
	int index;		// value of indexOLD or indexNEW used to 
				// reference in cOLD, depending on whether 
				// the NewCO is present in cOLD or not.
	int nValsNEW;		// Number of data values in cNEW
	int nValsOLD;		// Number of data values in cOLD
	int diffN;		// difference in number between nValsNEW and
				// nValsOLD (always a positive value)

	// Determine how many carryover values there should be for the NEW 
	// method definition and how many exist in the old method definition
	nValsNEW = _sizeInflowCO;                               // methNEW
	nValsOLD = ( (LagK *)methOLD )->getSizeInflowCO ( );

	// Check for modified carryover structure We used to have 
	// 3*4 + 3*4 + 4 +3*4 + N*8 characters before "*FUTURE*".  This adds up
	// to be evenly divisible by 8.  The new carryover structure adds 12 
	// characters causing it not to be divisible by 8.
	// Use this information to determine how to index cOLD.
	length = strcspn( cOLD, "*" );
	if( length % 8 == 0) {
		// We have not added modified carryover yet
		index = indexOLD;
	}
	else {
		index = indexNEW;
	}

	// Determine which carryover rule applies and use it to copy data.
	// The strlen function is built into the copy commands to ensure
	// that the expected number of characters was copied.
	if (nValsNEW == nValsOLD) {
		// copy nValsNEW * 8 characters (based on written format;
		//	also equal to 2*sizeof(float))
		strncpy( &cNEW[indexNEW], &cOLD[index], nValsNEW*8 );
	}
	else if ( nValsNEW < nValsOLD ) {
		diffN = nValsOLD - nValsNEW;
		// Skip diffN * 8 characters of cOLD data and copy nValsNEW * 8
		// characters (based on written format) data
		strncpy( &cNEW[indexNEW], &cOLD[index+(diffN*8)], nValsNEW*8 );
	}
	else { 		// nVals(NEW) > nVals(OLD)
		diffN = nValsNEW - nValsOLD;
		// copy the "oldest" (and first) inflow value (8 characters, 
		// based on written format) from cOLD to cNEW a total of diffN 
		// times.
		int i;
		for ( i=0; i<diffN; i++) {
			// Prevent any further work if an error has occurred
			if ( ierr > 0 ) {
				continue;
			}
			// Assign a value of 0.000000 to an extra data space 
			// which will not be filled by cOLD
			strncpy(&cNEW[indexNEW+(i*8)], "0.000000", 8);
			/**** If first value is to be duplicated to fill extra
				spaces, rather than filling with 0.000000
			// copy the first inflow value 
			strncpy(&cNEW[indexNEW+(i*8)], &cOLD[index], 8);
			****/
		}
		// Do not continue copying if an error has occurred
		if ( ierr == 0 ) {
			// copy nValsOLD * 8 characters (based on written 
			// format) of cOLD data beginning at the end of the 
			// padded duplicates (diffN*8 characters copied above)
			strncpy( &cNEW[indexNEW+(diffN*8)], &cOLD[index], 
				nValsOLD*8 );
		}
	}

	// Transfer the outflow.
	// In the new carry over, there are now nValsNEW inflows located
	//	beginning at &cNEW[indexNEW].  We will therefore copy the outflow
	//	values into the array located nValsNEW*8 characters beyond
	//	the beginning.  The copied value exists at nValsOLD*8 characters
	//	beyond its beginning as pointed to by &cOLD[index].
	strncpy( &cNEW[indexNEW+nValsNEW*8], &cOLD[index+nValsOLD*8], 8 );
        nValsNEW++;
        nValsOLD++;

	// Transfer the storage.
	strncpy ( &cNEW[ indexNEW + nValsNEW * 8 ], 
                  &cOLD[ index +  nValsOLD * 8 ], 8 );
        nValsNEW++;
        nValsOLD++;

	// Transfer the lagged inflow.
	strncpy ( &cNEW[indexNEW+nValsNEW*8], 
                  &cOLD[index+nValsOLD*8], 8 );
 
	// Any additional, currently unassigned carry over slots will have
	//	already been initialized appropriately.  No transfer is 
	//	required.
	
	// Handle any errors
	if ( ierr > 0 ) {
		sprintf (message, "LagK string copy error for %s.",_id);
		length = strlen (message);
		ResJ_fwrite( &length, message, ipr );
	}
	
	return ierr;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_transferCO.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_transferCO.cxx,v 1.5 2006/10/26 15:23:05 hsu Exp $";}
/*  ===================================================  */

}

