//------------------------------------------------------------------------------
// carryover_transfer58.cc : 
//			It will actuate the transfer of carryover for 
//			redefinition of the ResJ operation within a segment
//------------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 11 Apr 2001  James R. VanShaar, RTi	Initial Version
// 20 Nov 2001	JRV, RTi	Enhanced error handling
// 20 Nov 2001	JRV, RTi	Added handling of Node carry over
// 09 Aug 2002	JRV, RTi	Fixed bug in Reach component name handling.
// 18 Dec 2002	JRV, RTi	Revised method carryover to work with entire
// 				method identification set (type, owner_id,
// 				method_id).
// 24 Feb 2003	JRV, RTi	Fixed bug with Node_transferCO call.
// 19 Nov 2003	JRV, RTi	Bug fix in preparing methType, compName.
// 11 Mar 2004	JRV, RTi	Added CalcInflow work.
// 20 Apr 2004	JRV, RTi	Fixed bugs in handling Spillway, CalcInflow
//				preparation for transfer.
// 28 Mar 2006  JRV, RTi    Added transfer of Lookup3.
//------------------------------------------------------------------------------
// Variables:   I/O     Description
//
//------------------------------------------------------------------------------

#include "Component.h"
#include <iostream.h>
#include "CalcInflow.h"
#include "LagK.h"
#include "Lookup3.h"
#include "Node.h"
#include "Reservoir.h"
#include "ResJSys.h"
#include "SetWithdraw.h"
#include "Spillway.h"

int carryovertransfer58 ( float* COLD, float* CNEW, float* POLD, float* PNEW,
	char* resjFOLD, char* resjFNEW, int* ibug, int* ipr, int* iodebug )

// resjFOLD	name of old resj file (defined in fs5files)
//		eg: /fs/nws/ofs/files/cprrb/fs5files/RESJ.BOCNL.BOCNL
// resjFNEW	name of new resj file (temporary)
//		eg: /fs/nws/ofs/files/cprrb/fs5files/TEMP.RESJ.BOCNL.BOCNL

// WARNING:
// Any change in the structure of the carryover arrays will affect the
// indexing used in this routine!!

{
	int ierr = 0;
	int i,length, NewCO_OLD, typeOK, compOK;
	char error[256], methType[256], tmp_str1[256], tmp_str2[256];
	char *ch_cOLD = NULL, *ch_cNEW = NULL, 
		routine[]="carryover_transfer58";
	ResJSys resjOLD, resjNEW;
	ResJSys::setIPR_IODEBUG (*ipr, *iodebug);

	// set the destinations for the debug (and status) and warning messages 
	// to the appropriate levels...
	ResJSys :: setDebugSaveLevel( *ibug );
	// The use of setWarningSaveLevel is currently required to reach the
	// 	SetWarningLevel(...) function
	ResJSys :: setWarningSaveLevel( 1 + *ibug );
		// value of 2 will cause functions printing the warning to 
		// appear

	// PREPARE FILENAMES

	// Check for validity of resjFOLD
	if( resjFOLD == NULL ) {
		ierr = 1;
		sprintf( error, "OLD Resj fs5file not specified" );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_fwrite( &length, error, ipr ); 
		return ierr;
	}
        // Otherwise, use the length of the file name passed in as file_len
	// to determine the actual file name. We have to do this explicitly
	// to cleanly handle the Fortran to C++ char* interface, which is
	// not a pretty one.
        for( i = 0, length = 0; i < strlen( resjFOLD ); i++ ) {
		if( resjFOLD[i] == ' ') {
			break;
		}
		length++;
	}
	// Trim extra values from resjFOLD by assigning the last value to /0
	resjFOLD[length] = '\0';

	// Repeat as above for resjFNEW
	if( resjFNEW == NULL ) {
		ierr = 1;
		sprintf( error, "NEW Resj fs5file not specified" );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_fwrite( &length, error, ipr ); 
		return ierr;
	}
        for( i = 0, length = 0; i < strlen( resjFNEW ); i++ ) {
		if( resjFNEW[i] == ' ') {
			break;
		}
		length++;
	}
	resjFNEW[length] = '\0';


	// BUILD THE SYSTEMS

	// Instantiate the old ResJSys
	ierr = resjOLD.run ( resjFOLD );
	if ( ierr > 0 ) {
		sprintf ( error, "Unable to instatiate OLD ResJ" );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_fwrite( &length, error, ipr ); 
		return ierr;
	}

	// Instantiate the new ResJSys
	ierr = resjNEW.run ( resjFNEW );
	if ( ierr > 0 ) {
		sprintf ( error, "Unable to instatiate NEW ResJ" );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_fwrite( &length, error, ipr ); 
		return ierr;
	}


	// CONVERT CARRYOVER FROM FLOAT TO CHARACTER ARRAYS

	// Convert incoming carryover arrays from float to char for COLD
	int iusecO = (int)POLD[3];
        ch_cOLD = (char*)malloc( (iusecO)*sizeof(float)+1 );
        if( ch_cOLD == NULL ) {
                ierr = 1;
                sprintf( error, "Troubles allocating memory for COLD array." );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_fwrite( &length, error, ipr ); 
                return ierr;
        }
        memcpy( ch_cOLD, COLD, (iusecO)*sizeof(float) );
        ch_cOLD[ (iusecO)*sizeof(float) ] = '\0';

	// Repeat conversion of incoming carryover arrays for CNEW
	int iusecN = (int)PNEW[3];
        ch_cNEW = (char*)malloc( (iusecN)*sizeof(float)+1 );
        if( ch_cNEW == NULL ) {
                ierr = 1;
                sprintf( error, "Troubles allocating memory for CNEW array." );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_fwrite( &length, error, ipr ); 
                return ierr;
        }
        memcpy( ch_cNEW, CNEW, (iusecN)*sizeof(float) );
        ch_cNEW[ (iusecN)*sizeof(float) ] = '\0';
 

	// To transfer carryover, we will now cycle through COLD to address the
	// components there.  The format required to do this for both reservoirs
	// and LagK reaches is the following:
	//	Keyword: Either RESERVOIR or REACH is the beginning of the
	//		carryover for a given component.  This keyword fills
	//		the memory equal to 3 floating point values (or 12
	//		characters).
	//	Component name: The user defined name of the reservoir or reach.
	//		This name also fills the memeory equal to 3 floating
	//		point values (12 characters).
	//	Next location: The index to the next keyword, signifying the
	//		beginning of the next component's carryover within the
	//		ResJ carryover array.  This integer is assigned to the
	//		memor equal to one floating point value (or 4 
	//		characters, right justified).  This value is equal to
	//		-999 for the final entry to the carryover array.
	//	Following these three portions comes the component specific
	//		carryover data (or further descriptive information).
	//		It will be discussed further in the respective 
	//		component (or method) transferCO() functions.
	// As an example of the above, suppose the first component is a
	// reservoir.  Then the array, as transformed to characters will be 
	// (with the line of numbers below used as reference to array 
	// positions):
	// RESERVOIR  RESNAME        5211.11118222.22283333.338REACH    etc.
	// 0123456789012345678901234567890123456789012345678901234567890
	//	where the 52 found as part of positions 24-27 points to the
	//	first character of REACH.

	// Using the above system, we will identify the beginning and end of
	// each portion of carryover and determine what type of carryover it is.
	// Extracting the necessary name information, we will then proceed to 
	// assign a pointer to the desired component (or method) within each
	// ResJ system (OLD and NEW).  Using the NEW component (or method),
	// execution will then transfer to the transferCO() function for further
	// work.

	// Define useful variables.  Some redundancy may exist, but is useful
	// for readability.

	int numNameFloats = 3;	// Number of floating point values used to
				// hold any string-type information within the
				// carryover array
	
	int maxNameSize = numNameFloats * sizeof(float);
				// Maximum number of characters used in a 
				// string-type information piece within the 
				// carryover array. (likely = 12)

	int numCompFloats = 2 * numNameFloats;	
				// Number of floating point values to identify
				// component type and name.

	int numNextFloats = 1;	// Number of floating point values to define
				// the location of the next component within
				// the ResJ carryover array.

	int numNextSize = numNextFloats * sizeof(float);
				// Number of characters used to hold the index
				// for the next component

	int IndexCompName = numNameFloats * sizeof(float);
				// Index (in the character array version of the
				// carryover array) pointing to the component
				// name.  Indexing begins with 0 at the first
				// character of the component keyword. (likely
				// = 12)

	int IndexDefineNext = numCompFloats * sizeof(float);
				// Index (in the character array version of the
				// carryover array) pointing to the index for
				// the next component keyword.  Indexing begins 
				// with 0 at the first character of the 
				// component keyword.
	
	int IndexLagKMethName = (numCompFloats + numNextFloats) * sizeof(float);
				// Index (in the character array version of the
				// carryover array) of the LagK method name;
				// required for finding the method from within
				// the ResJSys object.  Indexing begins with 0 
				// at the first character of the component 
				// keyword.

	int IndexMethName = numNameFloats * sizeof(float);
				// Index (in the character array version of the
				// carryover array) pointing to the (non-LagK)
				// Method name.  Indexing begins with 0 at the 
				// first character of the component keyword. 
				// (likely = 12)

	int IndexMethType = IndexDefineNext + numNextSize;
				// Index (in the character array version of the
				// carryover array) pointing to the (non-LagK)
				// Method type.  Indexing begins with 0 at the 
				// first character of the component keyword. 
				// (likely = 28)

	int IndexMethComp = IndexMethType + numNameFloats * sizeof(float);
				// Index (in the character array version of the
				// carryover array) pointing to the (non-LagK)
				// Method name.  Indexing begins with 0 at the 
				// first character of the component keyword. 
				// (likely = 40)

	int IndexNext = 0;	// Index (in the character array version of the
				// carryover array) of the beginning of the next
				// component keyword.  Indexing begins at the
				// start of COLD.

	char *currOLD = NULL;	// Pointer to the beginning character of the
				// keyword of the current component whose 
				// carryover is being processed. (OLD refers
				// to the OLD carryover).

	char *currNEW = NULL;	// Pointer to the beginning character of the
				// keyword of the current component whose 
				// carryover is being processed. (NEW refers
				// to the NEW carryover).

	char compName[maxNameSize + 1];
				// Name of component (used in matching with
				// portions of ResJSys object

	char methName[maxNameSize + 1];
				// Name of method (used in matching with 
				// portions of ResJSys object

	char copyOLD[IndexDefineNext + 1];
				// Used as copy of Keyword and component name
				// as seen in COLD, for finding the same
				// component in CNEW.

	char *temp;		// Temporary pointer

	char ch_NextIndex[numNextSize + 1];
				// Temporary pointer

	int match;		// Boolean integer used to ensure a match on
				// Reach methods.

	Component *rootOLD = resjOLD.getRoot();
        Component *rootNEW = resjNEW.getRoot();
        Component *compOLD = NULL;
        Component *compNEW = NULL;
	Method *methOLD = NULL;
	Method *methNEW = NULL;

	// Repeat gather name info, find component (method), transfer carryover
	// process until the next keyword index is not defined (= -999).
	while (IndexNext != -999) {
		// update currOLD pointer to the beginning of the keyword
		currOLD = &ch_cOLD[IndexNext];

		// Make a copy of the Keyword and component name (up to the
		// beginning of the next component index definition)
		strncpy (copyOLD, currOLD, IndexDefineNext+1);
		copyOLD[IndexDefineNext] = '\0';

		// update IndexNext to beginning of next keyword
		strncpy ( ch_NextIndex, &currOLD[IndexDefineNext], 
			numNextSize );
		ch_NextIndex[numNextSize] = '\0';
		IndexNext = atoi(ch_NextIndex);

		// Now branch into component type dependent processing
		// NOTE: the strncmp returns 0 if it matches, hence the NOT '!'
		if ( !strncmp(currOLD, "RESERVOIR", 9) ) {
			// Find matching reservoir component in the NEW 
			// carry-over array (string)
                	if ( (currNEW = strstr(ch_cNEW, copyOLD)) == NULL ) {
                        	// The old reservoir is not defined in the new 
				// system
				sprintf( error, "String \"%s\" not found in "
					"new carry over.", copyOLD );
				PrintWarning(1, routine, error);
                        	continue;
                	}
                	// currOLD and currNEW now represent the start of the 
			// carryover for the same reservoir component

			// find and define the end of the component name
			strncpy ( compName, &currOLD[IndexCompName], 
				maxNameSize+1 );
			temp = strchr ( compName, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }

			// find the reservoir in resjOLD and resjNEW ResJSys 
			// objects
			compOLD = rootOLD->getComponentPtr(compName);
			if ( compOLD == NULL ) {
				// some error if in ch_cOLD but not resjOLD
				continue;
			}
			compNEW = rootNEW->getComponentPtr(compName);
			if ( compNEW == NULL ) {
				// The reservoir present in the old version
				// is no longer present in the new. Print a
				// warning.
				sprintf( error, "RESERVOIR %s not found in the "
					"new version",compName);
				PrintWarning( 1, routine, error);
				continue;
			}
			// We now have the two components to compare and 
			// transfer the related RESERVOIR carryover
			ierr = ((Reservoir*)compNEW)->transferCO(compOLD, 
				currOLD, currNEW, ipr);
			if ( ierr > 0 ) {
				// Handle appropriately
				sprintf( error, "Carryover Transfer failed for "
					"RESERVOIR %s",compName);
				PrintError( routine, error );
				break;
			}
		}
		else if ( !strncmp(currOLD, "REACH", 5) ) {
			// Check for modified carryover structure for OLD CO
			// We used to have 3*4 + 3*4 + 4 +3*4 + N*8 characters 
			// before "*FUTURE*".  This adds up to be evenly 
			// divisible by 8.  The new carryover structure adds
			// 12 characters causing it not to be divisible by 8.
			length = strcspn( currOLD, "*" );
			NewCO_OLD = 1;
			if( length % 8 == 0) {
				// We have not added modified carryover yet
				NewCO_OLD = 0;
				typeOK = 1;
			}

			// find and define the component name
			strncpy ( compName, &currOLD[IndexCompName], 
				maxNameSize+1 );
			temp = strchr ( compName, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }

			// Extract method name
			strncpy ( methName, &currOLD[IndexLagKMethName], 
				maxNameSize+1 );
			methName[maxNameSize] = '\0';

			// If appropriate, extract method type
			if( NewCO_OLD ) {
				sprintf ( methType, "%-12.12s",
					&currOLD[IndexLagKMethName+12] );
			}
			else {
				// We must assume it is LAGK
				sprintf ( methType, "%-12.12s", "LAGK" );
			}


			// Find matching reach component in the NEW carry-over 
			// array (string)
			// NOTE: This requires additional comparison of the
			//	method name (LagK only available for now)
			match = 0;
			currNEW = strstr(ch_cNEW, copyOLD);

			// We don't have to check for modified carryover 
			// structure for the new CO because it will have been
			// created with the new code, resulting in NEW CO

			while (currNEW != NULL && !match) {
				sprintf( tmp_str1, "%-12.12s", 
					&currNEW[24+4+12]);
				typeOK = !strncasecmp(tmp_str1, methType, 12);

				// Get method_ID from carry over
				sprintf( tmp_str1, "%-12.12s", &currOLD[24+4] );
				sprintf( tmp_str2, "%-12.12s", &currNEW[24+4] );

				// Compare method_IDs and type match
				if(typeOK && strstr(tmp_str1, tmp_str2)!=NULL) {
					match = 1;
				}
				else {
					// increment the string to the next 
					// character to try to find the next 
					// match
					currNEW++;
					currNEW = strstr( currNEW, copyOLD );
				}
			}

			// Check to see if we did not find a match
			if ( !match ) {
				// The old reach / method is not defined in the
				// new system. Print a warning.
				sprintf( error, "Method %s not found in the "
					"new version", methName);
				PrintWarning( 1, routine, error);
				continue;
			}

			// currOLD and currNEW now represent the start 
			// of the carryover for the same reach / method 

			// Trim the meth name now for use in the ResJSys
			// object search
			temp = strchr ( methType, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }
			temp = strchr ( compName, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }
			temp = strchr ( methName, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }

			// find the method in resjOLD and resjNEW ResJSys
			// objects
			methOLD = rootOLD->getMethod(methType, compName, 
				methName );
			if ( methOLD == NULL ) {
				// some error if in ch_cOLD but not resjOLD
				continue;
			}
			methNEW = rootNEW->getMethod(methType, compName, 
				methName );
			if ( methNEW == NULL ) {
				// some error if in ch_cNEW but not resjNEW
				continue;
			}

			// Determine method type and call appropriate method
			// transferCO() function
			if ( !strncmp(methOLD->getType(), "LAGK", 
				strlen("LAGK")) ) {
				// We now have the two methods to compare and 
				// transfer the related LagK method carryover
				ierr = ((LagK *)methNEW)->transferCO(methOLD, 
					currOLD, currNEW, ipr);
				if ( ierr > 0 ) {
					// Handle appropriately
					sprintf( error, "Carryover Transfer "
						"failed for LAGK %s",compName);
					PrintError( routine, error );
					break;
				}
	
			}
			else {
				// Currently no other Reach methods defined 
				// which use carryover.
				// Handle ERROR
				continue;
			}
		}
		else if ( !strncmp(currOLD, "NODE", 4) ) {
			// Find matching node component in the NEW 
			// carry-over array (string)
                	if ( (currNEW = strstr(ch_cNEW, copyOLD)) == NULL ) {
                        	// The old Node is not defined in the new 
				// system
				sprintf( error, "String \"%s\" not found in "
					"new carry over.", copyOLD );
				PrintWarning(1, routine, error);
                        	continue;
                	}
                	// currOLD and currNEW now represent the start of the 
			// carryover for the same reservoir component

			// find and define the end of the component name
			strncpy ( compName, &currOLD[IndexCompName], 
				maxNameSize+1 );
			temp = strchr ( compName, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }

			// find the Node in resjOLD and resjNEW ResJSys 
			// objects
			compOLD = rootOLD->getComponentPtr(compName);
			if ( compOLD == NULL ) {
				// some error if in ch_cOLD but not resjOLD
				sprintf( error, "NODE %s from old carry over "
					"not found in the old system.",
					compName);
				PrintError( routine, error);
				continue;
			}
			compNEW = rootNEW->getComponentPtr(compName);
			if ( compNEW == NULL ) {
				// The Node present in the old version
				// is no longer present in the new. Print a
				// warning.
				sprintf( error, "NODE %s not found in the "
					"new version",compName);
				PrintWarning( 1, routine, error);
				continue;
			}
			// We now have the two components to compare and 
			// transfer the related NODE carryover
			ierr = ((Node *)compNEW)->transferCO(compOLD, currOLD, 
				currNEW, ipr);
			if ( ierr > 0 ) {
				// Handle appropriately
				sprintf( error, "Carryover Transfer failed for "
					"NODE %s",compName);
				PrintError( routine, error );
				break;
			}
		}
		else if ( !strncmp(currOLD, "METHOD", 6) ) {
			// Check for modified carryover structure for OLD CO
			// We used to have 3*4 + 3*4 + 4 +3*4 + N*8 characters 
			// before "*FUTURE*".  This adds up to be evenly 
			// divisible by 8.  The new carryover structure adds
			// 12 characters causing it not to be divisible by 8.
			length = strcspn( currOLD, "*" );
			NewCO_OLD = 1;
			if( length % 8 == 0) {
				// We have not added modified carryover yet
				// WE ARE NOT GOING TO TRANSFER ANYTHING 
				// ASSOCIATED WITH THIS BLOCK
				continue;
			}

			// If we get to this point, we have NewCO.

			// Extract method name
			// Note: it coincides with the location of the component
			// name from component carry over.
			strncpy ( methName, &currOLD[IndexCompName], 
				maxNameSize+1 );
			methName[maxNameSize] = '\0';

			// Extract the method type
			sprintf (methType, "%-12.12s", &currOLD[IndexMethType]);
			// Extract the component name
			sprintf (compName, "%-12.12s", &currOLD[IndexMethComp]);

			// Bail out if we don't have transfer needs
			if( strncasecmp( methType, "SPILLWAY    ", 12 ) && 
				strncasecmp( methType, "SETWITHDRAW ", 12 ) && 
				strncasecmp( methType, "CALCINFLOW  ", 12 ) &&
				strncasecmp( methType, "LOOKUP3     ", 12 ) ) {
				// We reach this if methType is NOT in the list
				// of methods above, meaning that we have no 
				// transfer needs.
				continue;
			}

			// Find matching carryover in the NEW carry-over 
			// array (string)
			match = 0;
			currNEW = strstr(ch_cNEW, copyOLD);
			while (currNEW != NULL && !match) {
				// Check method type from carry over
				sprintf( tmp_str1, "%-12.12s", &currNEW[24+4] );
				typeOK = !strncasecmp(tmp_str1, methType, 12);

				// Check owner ID from carry over
				sprintf(tmp_str1,"%-12.12s",&currNEW[24+4+12]);
				compOK = !strncasecmp(tmp_str1, compName, 12);

				// Compare type and owner
				if( typeOK && compOK) {
					match = 1;
				}
				else {
					// increment the string to the next 
					// character to try to find the next 
					// match
					currNEW++;
					currNEW = strstr( currNEW, copyOLD );
				}
			}

			// Check to see if we did not find a match
			if ( !match ) {
				// The old reach / method is not defined in the
				// new system. Print a warning.
				sprintf( error, "Method %s not found in the "
					"new version", methName);
				PrintWarning( 1, routine, error);
				continue;
			}

			// currOLD and currNEW now represent the start 
			// of the carryover for the same reach / method 

			// Trim the meth name now for use in the ResJSys
			// object search
			temp = strchr ( methType, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }
			temp = strchr ( compName, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }
			temp = strchr ( methName, ' ' );
                        if ( temp != NULL ) {
                                temp[0] = '\0';
                        }

			// find the method in resjOLD and resjNEW ResJSys
			// objects
			methOLD = rootOLD->getMethod(methType, compName, 
				methName );
			if ( methOLD == NULL ) {
				// some error if in ch_cOLD but not resjOLD
				continue;
			}
			methNEW = rootNEW->getMethod(methType, compName, 
				methName );
			if ( methNEW == NULL ) {
				// some error if in ch_cNEW but not resjNEW
				continue;
			}

			// Determine method type and call appropriate method
			// transferCO() function
			if ( !strncmp(methOLD->getType(), "SETWITHDRAW", 
				strlen("SETWITHDRAW")) ) {
				// We now have the two methods to compare and 
				// transfer the related SetWithdraw method 
				// carryover
				ierr = ((SetWithdraw *)methNEW)->
					transferCO(methOLD, currOLD, currNEW, 
					ipr);
				if ( ierr > 0 ) {
					// Handle appropriately
					sprintf( error, "Carryover Transfer "
						"failed for SETWITHDRAW %s",
						compName);
					PrintError( routine, error );
					break;
				}
	
			}
			else if ( !strncmp(methOLD->getType(), "SPILLWAY", 
				strlen("SPILLWAY")) ) {
				ierr = ((Spillway *)methNEW)->
					transferCO(methOLD, currOLD, currNEW, 
					ipr);
				if ( ierr > 0 ) {
					// Handle appropriately
					sprintf( error, "Carryover Transfer "
						"failed for SPILLWAY %s",
						compName);
					PrintError( routine, error );
					break;
				}
			}
			else if ( !strncmp(methOLD->getType(), "CALCINFLOW", 
				strlen("CALCINFLOW")) ) {
				ierr = ((CalcInflow *)methNEW)->
					transferCO(methOLD, currOLD, currNEW, 
					ipr);
				if ( ierr > 0 ) {
					// Handle appropriately
					sprintf( error, "Carryover Transfer "
						"failed for CALCINFLOW %s",
						compName);
					PrintError( routine, error );
					break;
				}
			}
			else if ( !strncmp(methOLD->getType(), "LOOKUP3", 
			    strlen("LOOKUP3")) )
			{
			    ierr = ((Lookup3 *)methNEW)->transferCO(methOLD,
			        currOLD, currNEW, ipr);
			    if ( ierr > 0 )
			    {
			        // Handle appropriately
			        sprintf( error, "Carryover Transfer failed for "
			            "LOOKUP3 %s", compName);
			        PrintError( routine, error );
			        break;
			    }
			}
			else {
				// Currently no other  methods defined which use
				// carryover.  
				// Handle ERROR
				continue;
			}
		}
		else {
			// Currently no other options
			// Handle ERROR
		}
	}

	// Convert outgoing carryover arrays from char to float for ch_cNEW
	// All changes have been made to ch_cNEW by virtue of currNEW 
	// pointing to the same locations in memory.
	length = strlen (ch_cNEW);
	memcpy ( CNEW, ch_cNEW, length );

	return ierr;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/carryovertransfer58.cxx,v $";
 static char rcs_id2[] = "$Id: carryovertransfer58.cxx,v 1.13 2006/10/26 15:37:27 hsu Exp $";}
/*  ===================================================  */

}
