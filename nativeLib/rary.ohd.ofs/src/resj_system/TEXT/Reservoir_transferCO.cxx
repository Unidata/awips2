//------------------------------------------------------------------------------
// Reservoir::transferCO - Transfers carry over parameters for a Reservoir 
//				component
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Apr 2001	James R. VanShaar, Riverside Technology, Inc
//					Created initial version.
// 20 Nov 2001	JRV, RTi	Added transfer of INITIALINFLOW, 
//				PREVIOUSINFLOW, PREVIOUSRELEASE, PREVIOUSPOOL
//				and PREVIOUSWITHDRAW
// 26 Nov 2001	JRV, RTi	Added check for existence prior to withdraw(al)
//				transfer.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// @params resOLD Component object from old resj system definition (not used 
//	here)
// @params cOLD character string pointing to the portion of the old C array 
//	containing carryover for the active reservoir.
// @params cNEW character string pointing to the portion of the new C array 
//	containing carryover for the active reservoir.
// @returns ierr integer representing errors occuring or significant warnings:
//	0 = no problems
//	1 = Problem with a string copy function during release transfer
//	2 = Problem with a string copy function during pool elev transfer
//	3 = Problem with a string copy function during withdraw(al) transfer
//	4 = Problem with a string copy function during inflow transfer
// @warnings  If pool elevation is outside defined elevation / storage curve.
//	    This is a non-fatal error although it is very unlikely that the
//	    user wishes to define such a condition.
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "Reservoir.h"
#include <string.h>
#include "resj/Table.h"

int Reservoir :: transferCO ( Component * resOLD, char * cOLD, char * cNEW,
	int * ipr )

	// Currently (11/01) reservoir carryover data consists of:
	//	reservoir release
	//	pool elevation
	//	reservoir withdraw(al)
	//	reservoir inflow
	//	previous inflow to the reservoir
	//	previous release from the reservoir
	//	previous withdrawal from the reservoir
	//	previous pool elevation
	//	ten placeholders for future use (no transfer required)

	// Treated as general carryover, (carryover which is independent of 
	// defined methods), only the pool elevation may require
	// transformation, and this only if the new elevation / storage
	// curve is undefined at the old carryover pool elevation.
	// It is highly unlikely that that should ever occur, but we will
	// check for it.  Previous values are handled exactly as the current
	// values are.

	// Therefore, the transfer is according to the following:
	// 	release(new) = release(old)
	//	withdrawal:
	//		If withdraw(al)(new) is not defined, transfer nothing.
	//		Otherwise, if withdraw(al)(old) is not defined, assign
	//			a value of zero (0.000000).
	//		Otherwise, withdraw(al)(new) = withdraw(al)(old)
	//	pool elevation:
	//		if pool(old) > max elev. (new) (as defined in
	//			elevation / storage curve), then
	//			pool(new) = max elev. (new)
	//		if pool(old) < min pool (new) (as defined in the NOTE
	//			below), then
	//			pool(new) = min elev. (new)
 	//			**NOTE: min elev is the greater of the minimum
	//			value on the elevation / storage curve and the
	//			value of MINPOOL (_min_pool, here).  MINPOOL is
	//			 optional input, although Res-J defines it to 
	//			the minimum value on the elevation / storage 
	//			curve if it is not explicitly defined by the 
	//			user.  Also, a check is made elsewhere to ensure
	//			that MINPOOL is defined within the 
	//			elevation / storage curve.)
	//		Otherwise,
	//			pool(new) = pool(old)
	//	inflow(new) = inflow(old)

{
	int ierr = 0, length;
	char routine[]="Reservoir::transferCO", message[256];
	double lfactor = 1.0, ffactor = 1.0, vfactor = 1.0;

	// Set up some conversion variables for warning / error messages
        lfactor = Component::_lfactor;
        vfactor = Component::_vfactor;
        ffactor = Component::_ffactor;

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
	int relsI = nextI + numNextFloats*sizeof(float);
	int poolI = relsI + numDataFloats*sizeof(float);
	int withI = poolI + numDataFloats*sizeof(float);
	int inflI = withI + numDataFloats*sizeof(float);
	int endI  = inflI + numDataFloats*sizeof(float);
				// Note this is not the end of the variables,
				// rather only the end of the current-type
				// variables.

	int stepCurr_Prev = endI - relsI;	
				// Number of characters between the current-
				// type variable and its companion previous-
				// type variable

	int tempStep = 0;	// Temporary variable to allow us to use and
				// not use the value in stepCurr_Prev.

	char temp[9];
	temp[9] = '\0';

	// Loop used to allow error handling before returning and
	//	employed to deal with current and previous type variables
	int i;
	for ( i=0; i<2; i++ ) {
		// Reassign tempStep for transferring previous-type variables
		//	if we are on the second loop.
		tempStep = i * stepCurr_Prev;

		// Copy release (current)
		if ( strncpy(temp, &cOLD[relsI+tempStep], 
			(poolI-relsI)) == NULL ) {
			// fatal error
			ierr = 1 * (i+1);
			continue;
		}
		if ( strncpy(&cNEW[relsI+tempStep], temp, 
			(poolI-relsI)) == NULL ) {
			// fatal error
			ierr = 1 * (i+1);
			continue;
		}
	
		// Gather information required for transfer of pool elevation
		if ( strncpy(temp, &cOLD[poolI+tempStep], 
			(withI-poolI)) == NULL ) {
			// fatal error
			ierr = 2 * (i+1);
			continue;
		}
		double pool = atof (temp);
		Table elevStor = getELEVSTOR ();
		double maxElev = elevStor.getMax(GETCOLUMN_1);
		double minElev = elevStor.getMin(GETCOLUMN_1);
		if ( minElev < _min_pool ) {
			minElev = _min_pool;
		}

		// Check & transfer pool elevation
		if ( pool > maxElev ) {
			// Overwrite temp (pool) with maxElev
 			sprintf( temp, "%f", maxElev );
			if (strncpy(&cNEW[poolI+tempStep],temp,
				(withI-poolI)) == NULL) {
				// fatal error
				ierr = 2 * (i+1);
				continue;
			}
			sprintf (message, "OLD CO poolElev.(%f) > maxElev."  
				"(%f) in NEW %s elevStor. curve.",
				pool/lfactor, maxElev/lfactor, _id);
			PrintWarning ( 1, routine, message );
		} 
		else if ( pool < minElev ) {
			// Overwrite temp (pool) with minElev
 			sprintf( temp, "%f", minElev );
			if (strncpy(&cNEW[poolI+tempStep],temp,
				(withI-poolI)) == NULL) {
				// fatal error
				ierr = 2 * (i+1);
				continue;
			}
			sprintf (message, "OLD CO poolElev.(%f) < minElev."  
				"(%f) in NEW %s elevStor. curve.",
				pool/lfactor, minElev/lfactor, _id);
			PrintWarning ( 1, routine, message );
		}
		else {
			// carryover pool is within newly defined elevation / 
			// storage relationship
			if (strncpy(&cNEW[poolI+tempStep],temp,
				(withI-poolI)) == NULL) {
				// fatal error
				ierr = 2 * (i+1);
				continue;
			}
		}
	
		// Copy withdraw(al)
		// Check to see if we have a new withdrawal.  If not, transfer 
		// nothing.
		strncpy(temp, &cNEW[withI+tempStep], (inflI-withI) );
		if ( atof(temp) < -998.0 ) {
			// Copy nothing
			continue;
		}
		if ( strncpy(temp, &cOLD[withI+tempStep], 
			(inflI-withI)) == NULL ) {
			// fatal error
			ierr = 3 * (i+1);
			continue;
		}
		// If the withdraw(OLD) was not defined (value = -999) assign a 
		// value of zero.
		if ( atof(temp) < -998.0 ) {
			strncpy( &cNEW[withI+tempStep], "0.000000", 
				(inflI-withI) );
			continue;
		}
		if ( strncpy(&cNEW[withI+tempStep], temp, 
			(inflI-withI)) == NULL ) {
			// fatal error
			ierr = 3 * (i+1);
			continue;
		}

		// Copy inflow
		if ( strncpy(temp, &cOLD[inflI+tempStep], 
			(endI-inflI)) == NULL ) {
			// fatal error
			ierr = 4 * (i+1);
			continue;
		}
		if ( strncpy(&cNEW[inflI+tempStep], temp, 
			(endI-inflI)) == NULL ) {
			// fatal error
			ierr = 4 * (i+1);
			continue;
		}
	}

	// Handle any errors
	if ( ierr > 0 ) {
		switch (ierr) {
		     case 1:
			sprintf (message, 
			"Release transfer string copy error for %s.",_id);
			break;
		     case 2:
			sprintf (message, 
			"Pool elev transfer string copy error for %s.",_id);
			break;
		     case 3:
			sprintf (message, 
			"Withdraw(al) transfer string copy error for %s.",_id);
			break;
		     case 4:
			sprintf (message, 
			"Inflow transfer string copy error for %s.",_id);
			break;
		     case 5:
			sprintf (message, 
			"Previous release transfer string copy error for %s.",
				_id);
			break;
		     case 6:
			sprintf (message, 
			"Previous pool elev transfer string copy error for %s.",
				_id);
			break;
		     case 7:
			sprintf (message, 
			"Previous withdraw(al) transfer string copy error for "
				"%s.",_id);
			break;
		     case 8:
			sprintf (message, 
			"Previous inflow transfer string copy error for %s.",
				_id);
			break;
		     default:
			sprintf (message, 
			"Undefined error in %s for %s.",routine,_id);
			break;
		}
		length = strlen (message);
		ResJ_fwrite( &length, message, ipr );
	}
	
	return ierr;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/Reservoir_transferCO.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_transferCO.cxx,v 1.8 2006/10/26 15:32:45 hsu Exp $";}
/*  ===================================================  */

}

