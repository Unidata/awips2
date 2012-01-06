//------------------------------------------------------------------------------
// Spillway::construct - Reads a stringlist and constructs precip and/or
//				evap timeseries data.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 24 Dec 2002	James R. VanShaar, Riverside Technology, inc
//					Created initial version.
// 14 Jan 2003	JRV, RTi	Added handling of INITIALSPILL.
// 16 Jan 2003	JRV, RTi	Added consideration of _Active state by looking
// 				at INITIALSPILL value.
// 2003-11-21 Luiz Teixeira, RTi - Added list = FreeStringList( list ) at the 
//  				end of the main for loop in the construct and 
//                              elevToStor methods.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "Spillway.h"
#include "resj/Table.h"
#include "TSUtil.h"

int Spillway::construct ( char** re_list, int n_items )  
{
	char routine[]="Spillway::construct", **value_list = NULL,
		**list = NULL, temp[MAXC], ts_id[MAXC] = "", *ownerID, *methID,
		**sub_list = NULL;
	int	i, j, n_value = 0, nlist = 0, elev_found = 0, totErrs = 0,
		WarnedOnce = 0, nsublist = 0, table_found = 0;
	double lfactor = 1.0, ffactor = 1.0;

	if( Method::getUnitType() == ENGLISH ) {
		lfactor = 0.3048;
		ffactor = 0.028317;
	}

	ownerID = _owner->getID();

	for( i = 0; i < n_items; i++ ) {
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( re_list[i], " \n\t",
			DELIM_SKIP_BLANKS, &nlist );
		if( list == NULL || nlist == 0 ) { 
			totErrs++;
			PrintError( routine, "Troubles getting elevation "
				"spill data for Spillway %s %s.", ownerID,
				methID );
			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword TABLE
		if( !strcasecmp( list[0], "TABLE" ) ) {
			// Extract table sublist
			sub_list = GetSubStringList( &re_list[i], n_items-i, 
				"TABLE", &nsublist, RETURN_NO_KEYWORDS );
			if( sub_list == NULL || nsublist == 0 ) {
				totErrs++;
				PrintError( routine, 
					"Troubles forming sub_list for state "
					"table values for Spillway %s on %s.  "
					"Does \"ENDTABLE\" exist?", methID,
					 ownerID );
				list = FreeStringList( list );
				continue;
			}
			
			// Test for enough fields on TABLE identifying line
			if ( nlist < 2 ) {
				totErrs++;
				PrintError( routine,
					"Keyword ELEV_SPILL required "
					"immediately after %s for  Spillway %s "
					"on %s.", list[0], list[1], ownerID );
				list = FreeStringList( list );
				// Increment beyond TABLE sub_list (even though
				// this table was not properly defined)
				i += (nsublist + 1);
				continue;
			}

			// Check for keyword "ELEV_SPILL"
			if( !strcasecmp( list[1], "ELEV_SPILL" ) ) {
				if ( table_found == 1 ) {
					PrintWarning( 1, routine,
						"Duplicate TABLE ELEV_SPILL for "
						"Spillway %s on %s.  Last "
						"definition used.", methID );
				}
				// Process the sub_list containing the
				// elevation / spill information to convert it
				// to a storage / spill table.  This will
				// make the solve function more efficient.
				// Note this will take pool elevation in the
				// parameterized units, convert them to SI
				// units (if necessary), and look up the
				// corresponding storage on the reservoir
				// elevation / storage table (which exists in
				// SI units).  This storage, in SI units, will
				// replace the pool in parameterized units.
				// We also convert the spill to SI units.
				char **new_list = (char **)NULL;
				int k, nnewlist = 0;
				double pool, storage, spill;
				Table elevStor = _owner->getELEVSTOR();
				for( k = 0; k < nsublist; k++ ) {
					list = FreeStringList( list );
					list = BreakStringList( sub_list[k], 
						" \t\n", DELIM_SKIP_BLANKS, 
						&nlist);
					if( list == NULL || nlist != 2 ) { 
						list = FreeStringList( list );
						continue;
					}
			
					// Convert to modeling units
					pool = atof( list[0] ) * lfactor;
					spill = atof( list[1] ) * ffactor;
			
					// Determine the corresponding storage 
					// from the Reservoir pool elevation / 
					// storage table.
					storage = elevStor.lookup( pool, 
						GETCOLUMN_2, ALLOW_BOUNDS );
			
					// Create the line
					sprintf(temp, "%f %f", storage, spill);
					new_list = AddToStringList( new_list, 
						temp, &nnewlist );	
				}
				
				// Populate the table.  Remember that the 
				// storage and spill have been converted to 
				// appropriate units, as necessary.
				if( _stor_spill_tbl->populate( new_list, 
					nsublist, 1.000, 1.000 ) ) {
					totErrs++;
					PrintError( routine, 
						"Troubles filling Elevation-"
						"Spill table for  Spillway %s "
						"on %s.", methID, ownerID );
					list = FreeStringList( list );
					sub_list = FreeStringList( sub_list );
					totErrs++;
				}
				i += (nsublist + 1);
				table_found = 1;
				new_list = FreeStringList( new_list );
			}
			else {
				totErrs++;
				PrintError( routine,
					"Keyword ELEV_SPILL required "
					"immediately after %s for Spillway %s "
					"on %s.", list[0], methID, ownerID );
				sub_list = FreeStringList( sub_list ); 
				// Increment beyond TABLE sub_list (even though
				// this table was not properly defined)
				i += (nsublist + 1);
				continue;
			}
			list = FreeStringList( list );
			sub_list = FreeStringList( sub_list );
			continue;
		}

 		// Handle keyword INTERVALS
		if( !strcasecmp( list[0], "INTERVALS" ) ) {
			// Test for enough fields on TABLE identifying line
			if( nlist > 1 ) {
				if( !IsInteger( list[1] ) ) {
					totErrs++;
					PrintError( routine, "%s value '%s' is "
						"not an integer.", list[0], 
						list[1] );
					list = FreeStringList( list );
					continue;
				}
				_intervals = atoi( list[1] );
			}
			else {
				totErrs++;
				PrintError( routine, "Value required "
					"immediately after %s method "
					"keyword %s.", _type, list[0] );
				list = FreeStringList( list );
				continue;
			}
			list = FreeStringList( list );
			sub_list = FreeStringList( sub_list );
			continue;
		}

		if( !strcasecmp( list[0], "INITIALSPILL" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find INITIALPOOL
				// later.  We check near bottom of this 
				// function.
				PrintWarning(1, routine, "Value required with "
					"SPILLWAY keyword %s for Method "
					"\"%s %s\". ", list[0], _owner->_id, 
					_id );
				_myValue = MISSING;

				// Ensure _Active = 0, we may have activated
				// it previously.
				_Active = 0;

				list = FreeStringList( list );
				continue;
			}
			_myValue = atof( list[1] ) * ffactor;
			// Check to see if the Spillway was active by seeing if
			// the last spill was non-zero.  Granted, it could have
			// been active and spilled nothing, but it is 
			// acceptable to assume it was inactive in that case.
			// The method defaults to inactive.
			if( _myValue > 0 ) {
				_Active = 1;
			}
			list = FreeStringList( list );
			continue;
		}

		// If we reach this point, we don't recognize the keyword
		if ( !WarnedOnce ) {
				WarnedOnce++;
				PrintWarning( 1, routine, "'%s' is an "
					"unrecognized (or misplaced) keyword.  "
					"If there are other errors or warnings "
					"for SPILLWAY %s, this may "
					"disappear following their correction.",
					list[0], _id );
		}
		
		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}
	}

	
	// Check for required values
	if( _intervals == 0 ) {
		totErrs++;
		PrintError( routine, "SPILLWAY keyword \"INTERVALS\" not "
			"defined for %s on %s.", methID, ownerID );
	}
	if( !table_found ) {
		totErrs++;
		PrintError( routine, "SPILLWAY table for %s on %s "
			"not successfully read.", methID, ownerID );
	} 

	// The following defaults to 0.0, but will be reassigned to MISSING if
	//	there are errors in input syntax
	if( _myValue == MISSING ) {
		totErrs++;
		PrintError( routine, "SPILLWAY keyword \"INITIALSPILL\" and "
			"Value required but not defined for %s on %s.", methID,
			ownerID );
	}

	// If so far so good, allocate memory for timeseries required for
	// solution of water balance.
	if ( totErrs == 0 ) {
		_withVol_ts = new double [_intervals+1];
		_relVol_ts = new double [_intervals+1];
		_stor_ts = new double [_intervals+1];
		_spillVol_ts = new double [_intervals+1];
		_inflVol_ts = new double [_intervals+1];
	}

	// Extract the storage associated with 0 spill
	// We will not use the old Table::lookup function because it requires
	// that the values be increasing.  We will do it by hand here.
	_minSpillStorage = _stor_spill_tbl->lookup( 0.0001, GETCOLUMN_1, 
		ALLOW_BOUNDS );
	if( _minSpillStorage == MISSING ) {
		// This may occur if we have the same spill value (even zero) 
		// for consecutive elevations
		totErrs++;
	}

	if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}

	// We have to write the carryover to the CO array at the System
	// level...
	setCOstring();

	_is_constructed = 1;
	return( STATUS_SUCCESS );
}

char** Spillway::elevToStor ( char** super_list, int super_size, double lfactor,
	double ffactor )
{
	char 	**list=NULL, **sub_list = NULL, temp[256]; 
	int i, start = -1, end = -1, nlist=0, count = 0, start_found = 0,
		end_found = 0, nsublist = 0;
	double pool, storage, spill;

	Table elevStor = _owner->getELEVSTOR();

	// Modify the list 
	for( i = 0; i <= super_size; i++ ) {
		list = BreakStringList( super_list[i], " \t\n", 
			DELIM_SKIP_BLANKS, &nlist );
		if( list == NULL || nlist != 2 ) { 
			list = FreeStringList( list );
			continue;
		}

		// Convert to modeling units
		pool = atof( list[0] ) * lfactor;
		spill = atof( list[1] ) * ffactor;

		// Determine the corresponding storage from the Reservoir
		// pool elevation / storage table.
		storage = elevStor.lookup( pool, GETCOLUMN_2, ALLOW_BOUNDS );

		// Revise the line
		sprintf( temp, "%f %f", storage, spill );
		super_list[i] = temp;

		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}
	}

	return( super_list );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_construct.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_construct.cxx,v 1.5 2006/10/26 15:36:04 hsu Exp $";}
/*  ===================================================  */

}
