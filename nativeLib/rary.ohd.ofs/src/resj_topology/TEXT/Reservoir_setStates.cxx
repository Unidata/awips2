//------------------------------------------------------------------------------
// Reservoir_setStates routines.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Apr 1998  Daniel Weiler, RTi	Created initial version / broke out 
//					from Reservoir set/get.
// 08 Oct 1998	DKW, RTi		Polish/data management.
// 16 Oct 1998	DKW, RTi		Added withdraw carryover.
// 04 May 2001  James R. VanShaar, RTi	Added further check of initial 
//					conditions against maxes / mins.
// 17 May 2001	JRV, RTi	Added message about future states in carryover
// 10 Jul 2001	JRV, RTi	Corrected issues from misnamed time series and
//				use of full identifiers
// 13 Nov 2001	JRV, RTi	Caused INITIALINFLOW and PREVIOUSPOOL to be
//				required parameters as they are now part of
//				carry over.
// 13 Nov 2001	JRV, RTi	Added PREVIOUSINFLOW, PREVIOUSWITHDRAW,
//				PREVIOUSRELEASE as required parameters due to
//				their existence in carry over.
// 27 Nov 2001	JRV, RTi	Revised usage of INFLOW, WITHDRAW, and RELEASE
//				parameters (INITIAL and PREVIOUS) and 
//				PREVIOUSPOOL to handle using defaults if they
//				are not explicity defined.
// 06 Jun 2002	JRV, RTi	Added _startInflow, _prevInflow, _startPool, 
//				_prevPool, _startWithdraw, _prevWithdraw,
//				_startRelease, _prevRelease.
// 31 Oct 2003	JRV, RTi	Modified to free memory completely.
// 12 Mar 2004	JRV, RTi	Added TSOutput of Inflow timeseries.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reservoir.h"
#include "ResJSys.h"
#include "TSList.h"

int Reservoir::setStates( char** states, int n_states )
{
	char routine[] = "Reservoir::setStates", **sub_list = NULL, 
		**list = NULL, ts_id[MAXC]="", message[1000];
	int i, j, nlist=0, nsublist=0, table_found=0, n_out=0, totErrs=0;
	double lfactor = 1.0, vfactor = 1.0, ffactor = 1.0;
/*
	if( Method::getUnitType() == ENGLISH ) {
		lfactor = 0.3048;
		vfactor = 1233.5;
		ffactor = 0.028317;
	}
*/
// printf("\n 2!!!!! lfactor= %10f  ffactor= %10f \n", lfactor, ffactor);
        lfactor = Component::_lfactor;
        vfactor = Component::_vfactor;
        ffactor = Component::_ffactor;
// printf("\n 3!!!!! lfactor= %10f  ffactor= %10f \n", lfactor, ffactor);

	for( i = 0; i < n_states; i++ ) {  	//Skip empty lines / comments
		if( strlen( states[i] ) == 0 || states[i][0] == '#' ){
			continue;
		}

		list = BreakStringList( states[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist );  
		if( list == NULL ) {
			PrintWarning( 1, routine, "Troubles breaking "
				"stringlist for RESERVOIR- \"%s\". State data "
				"values \"%s\" malformed.", getID(), 
				states[i] );
			list = FreeStringList( list );
			continue;
		}


 		// Handle keyword TABLE
		if( !strcasecmp( list[0], "TABLE" ) ) { 
			// Extract table sublist
			sub_list = GetSubStringList( &states[i], n_states-i, 
				"TABLE", &nsublist, RETURN_KEYWORDS );
			if( sub_list == NULL || nsublist <= 2 ) {
				totErrs++;
				PrintError( routine, 
					"Troubles forming sub_list for state "
					"table values for reservoir %s.  Does "
					"\"ENDTABLE\" exist?", getID() );
				list = FreeStringList( list );
				continue;
			}
			
			// Test for enough fields on TABLE identifying line
			if ( nlist < 2 ) {
				totErrs++;
				PrintError( routine,
					"Keyword ELEV_STOR required "
					"immediately after %s for reservoir "
					"%s.", list[0], list[1] );
				list = FreeStringList( list );
				// Increment beyond TABLE sub_list (even though
				// this table was not properly defined)
				i += (nsublist - 1);
				continue;
			}

			// Check for keyword "ELEV_STOR"
			if( !strcasecmp( list[1], "ELEV_STOR" ) ) {
				if ( table_found == 1 ) {
					PrintWarning( 1, routine,
						"Duplicate TABLE ELEV_STOR for "
						"Reservoir %s.  Last "
						"definition used.", getID() );
				}
				if( _elev_stor_tbl.populate( &sub_list[1], 
					nsublist-2, lfactor, vfactor ) ) {
					totErrs++;
					PrintError( routine, 
						"Troubles filling Elevation-"
						"Storage table on Reservoir "
						"%s.", getID() );
					list = FreeStringList( list );
					sub_list = FreeStringList( sub_list );
					totErrs++;
				}
				i += (nsublist - 1);
				table_found = 1;
			}
			else {	
				totErrs++;
				PrintError( routine,
					"Keyword ELEV_STOR required "
					"immediately after %s for reservoir "
					"%s.", list[0], list[1] );
				sub_list = FreeStringList( sub_list ); 
				// Increment beyond TABLE sub_list (even though
				// this table was not properly defined)
				i += (nsublist - 1);
				list = FreeStringList( list );
				continue;
			}
			list = FreeStringList( list );
			sub_list = FreeStringList( sub_list );
			continue;
		}

 		// Handle keyword TSINPUT
		else if( !strcasecmp( list[0], "TSINPUT" ) ) {
			if ( nlist < 3 ) {
				totErrs++;
				PrintError( routine, 
					"Keyword INFLOW and timeseries "
					"identifier required immediately "
					"after reservoir keyword %s.", 
					list[0] );
				list = FreeStringList( list );
				continue;
			}

			// Read either full identifiers or alias 
			strcpy( ts_id, list[2] );
			for( j = 3; j < nlist; j++ ) {
				strcat( ts_id, " " );
				strcat( ts_id, list[j] );
			}

			// Figure out what kind of TS it is and then set the
			// appropriate TS* data member (actually, the only valid
			// input TS for a Reservoir is "inflow"
			if( !strcasecmp( list[1], "INFLOW" ) ) {
				// Check for TSINPUT INFLOW in addition to 
				// inflow from an upstream component
				if( _n_son != 0 ) {
					PrintWarning( 1, routine, 
					"Applying 'Lateral-type' flow \"%s\" "
					"at head of reservoir \"%s\". This is "
					"in addition to the inflow(s) from "
					"component(s) topologically upstream. ",
					list[2], getID() );
				}

                                // Check for non-primary inflow at upstream-most
				// component
                                if( _n_son == 0 && _in_count > 0 ) {
                                        PrintWarning( 1, routine,
                                                "Applying non-primary flow "
						"\"%s\" at head of reach "
						"\"%s\".  At least one other "
						"flow has been defined as "
						"inflow to the reach.", list[2],
						 getID() );
                                }

				if( setInflowTS( (HourTS*)TSList::
					getTSFromList( ts_id ) ) ) { 
					totErrs++;
					PrintError( routine, "Troubles setting "
						"inflow timeseries \"%s\" on "
						"%s.", states[i], _id );
					list = FreeStringList( list );
					continue;
				}
			}
			else {
				totErrs++;
				PrintError( routine, 
					"Keyword INFLOW required immediately "
					"after reservoir keyword %s.  %s is "
					"not valid.", list[0], list[1] );
				list = FreeStringList( list );
				continue;
			}
			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword TSOUTPUT
		else if( !strcasecmp( list[0], "TSOUTPUT" ) ) {
			if ( nlist < 3 ) {
				totErrs++;
				PrintError( routine, 
					"Keyword OUTFLOW and timeseries "
					"identifier required immediately "
					"after reservoir keyword %s.", 
					list[0] );
				list = FreeStringList( list );
				continue;
			}

			// Read either full identifiers or alias 
			strcpy( ts_id, list[2] );
			for( j = 3; j < nlist; j++ ) {
				strcat( ts_id, " " );
				strcat( ts_id, list[j] );
			}

			// Build the list of output Timeseries
			if( strcasecmp( list[1], "POOL" ) &&
			    strcasecmp( list[1], "RELEASE" ) &&
			    strcasecmp( list[1], "WITHDRAW" ) &&
			    strcasecmp( list[1], "STORAGE" ) &&
			    strcasecmp( list[1], "INFLOW" ) ) {
				totErrs++;
				PrintError( routine,
					"Keyword POOL, RELEASE, STORAGE, "
					"WITHDRAW, INFLOW required "
					"immediately after reservoir keyword "
					"%s. %s is not valid.", list[0], 
					list[1] );
				list = FreeStringList( list );
				continue;
			}
			else {
				if( !strcasecmp( list[1], "POOL" ) ) {
					_output_ts[n_out] = _pool_ts;
				}
				else if( !strcasecmp( list[1], "RELEASE" ) ) {
					_output_ts[n_out] = _release_ts; 
				}
				else if( !strcasecmp( list[1], "SPILLWAY" ) ) {
					_output_ts[n_out] = _spill_ts; 
				}
				else if( !strcasecmp( list[1], "STORAGE" ) ) {
					_output_ts[n_out] = &_storage_ts; 
				}
				else if( !strcasecmp( list[1], "INFLOW" ) ) {
					_output_ts[n_out] = &_totalInflow; 
				}
				else if( !strcasecmp( list[1], "WITHDRAW" ) ) {
					_output_ts[n_out] = _withdraw_ts; 
				}
				if( _output_ts[n_out] == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
						"%s as a valid reservoir "
						"output time series.", ts_id );
					list = FreeStringList( list );
					continue;
				}
				HourTS *tempTS = (HourTS*)TSList::
					getTSFromList( ts_id );
				if ( !tempTS ) {
					totErrs++;
					PrintError( routine, "Unable to find "
						"TSOUTPUT '%s' in time series "
						"listing.", ts_id );
					list = FreeStringList( list );
					continue;
				}
				TSIdent temp = tempTS->getIdentifier();
				_output_ts[n_out]->setIdentifier( temp );
				n_out++;
			}
			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword INITIALPOOL
		else if( !strcasecmp( list[0], "INITIALPOOL" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find INITIALPOOL
				// later.  We check near bottom of this 
				// function.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
				_pool = MISSING;
				list = FreeStringList( list );
				continue;
			}
			_pool = atof( list[1] ) * lfactor;
			_poolCO = _pool;
			_startPool = _pool;
			// If not already reassigned, set default 
			//	_prevPoolCO.
			if ( _prevPoolCO == MISSING ) {
				_prevPoolCO = _poolCO;
				_prevPool = _prevPoolCO;
			}
			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword INITIALRELEASE
		else if( !strcasecmp( list[0], "INITIALRELEASE" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find 
				// INITIALRELEASE later.  We check near bottom 
				// of this function.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
				_release = MISSING;
				list = FreeStringList( list );
				continue;
			}
			_release = atof( list[1] ) * ffactor;
			_releaseCO = _release;
			_startRelease = _release;
			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword INITIALWITHDRAW
		else if( !strcasecmp( list[0], "INITIALWITHDRAW" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find 
				// INITIALWITHDRAW later.  We check near bottom 
				// of this function.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
				_withdraw = MISSING;
				_withdrawCO = _withdraw;
				_startWithdraw = _withdraw;
				list = FreeStringList( list );
				continue;
			} 
			_withdraw = atof( list[1] ) * ffactor; 
			_withdrawCO = _withdraw;
			_startWithdraw = _withdraw;
			list = FreeStringList( list );
			continue;
		} 

 		// Handle keyword INITIALINFLOW
		else if( !strcasecmp( list[0], "INITIALINFLOW" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find 
				// INITIALINFLOW later.  We check near bottom 
				// of this function.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
				_inflowCO = MISSING;
				list = FreeStringList( list );
				continue;
			} 
			_inflowCO = atof( list[1] ) * ffactor; 
			_startInflow = _inflowCO;
			list = FreeStringList( list );
			continue;
		} 

 		// Handle keyword PREVIOUSPOOL
		else if( !strcasecmp( list[0], "PREVIOUSPOOL" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find PREVIOUSPOOL
				// later.  We check near bottom of this 
				// function.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
				_prevPoolCO = MISSING;
				list = FreeStringList( list );
				continue;
			}
			_prevPoolCO = atof( list[1] ) * lfactor;
			_prevPool = _prevPoolCO;

			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword PREVIOUSRELEASE
		else if( !strcasecmp( list[0], "PREVIOUSRELEASE" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find 
				// PREVIOUSRELEASE later.  We check near bottom 
				// of this function.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
				_prevReleaseCO = MISSING;
				list = FreeStringList( list );
				continue;
			}
			_prevReleaseCO = atof( list[1] ) * ffactor;
			_prevRelease = _prevReleaseCO;
			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword PREVIOUSWITHDRAW
		else if( !strcasecmp( list[0], "PREVIOUSWITHDRAW" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find 
				// PREVIOUSWITHDRAW later.  We check near bottom 
				// of this function.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
				_prevWithdrawCO = _withdraw;
				_prevWithdraw = _prevWithdrawCO;
				list = FreeStringList( list );
				continue;
			} 
			_prevWithdrawCO = atof( list[1] ) * ffactor; 
			_prevWithdraw = _prevWithdrawCO;
			list = FreeStringList( list );
			continue;
		} 

 		// Handle keyword PREVIOUSINFLOW
		else if( !strcasecmp( list[0], "PREVIOUSINFLOW" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find 
				// PREVIOUSINFLOW later.  We check near bottom 
				// of this function.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
				_prevInflowCO = MISSING;
				list = FreeStringList( list );
				continue;
			} 
			_prevInflowCO = atof( list[1] ) * ffactor; 
			_prevInflow = _prevInflowCO;
			list = FreeStringList( list );
			continue;
		} 

 		// Handle keyword MINRELEASE
		else if( !strcasecmp( list[0], "MINRELEASE" ) || 
			!strcasecmp( list[0], "MINIMUMRELEASE" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find MINRELEASE 
				// later, or we can use default value.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
                		_min_release = MISSING;
				list = FreeStringList( list );
				continue;
			}
			else {
				_min_release = atof( list[1] ) * ffactor; 
			}
			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword MINPOOL
		else if( !strcasecmp( list[0], "MINPOOL" ) ||
			!strcasecmp( list[0], "MINIMUMPOOL" ) ) {
			if( nlist < 2 ) {
				// Not error because we might find MINPOOL 
				// later, or we can use default value.
				PrintWarning(1, routine, "Value required with "
					"keyword %s for RESERVOIR \"%s\". "
					"(Line: '%s')", list[0],_id, 
					states[i] );
                		_min_pool = MISSING;
				list = FreeStringList( list );
				continue;
			}
			else {
				_min_pool = atof( list[1] ) * lfactor; 
			}
			list = FreeStringList( list );
			continue;
		}

 		// Handle keyword CONSTANT
                // CONSTANTs are handled by parseConstant, so skip over
                else if( !strcasecmp( list[0], "CONSTANT" ) ) {
			list = FreeStringList( list );
                        continue;
                }

 		// Handle unknown keyword
		else {
			PrintWarning(1, routine, "Unrecognized keyword \"%s\" "
				"for Reservoir %s line \"%s\".", list[0], _id, 
				states[i] );
		}
		list = FreeStringList( list );
		continue;
	}

	// We have completed the read in portion.  Now review values and ensure
	// that we have a working component.

	// Check to see that if this is the most upstream reservoir that we
	// have an inflow TS
	if( _n_son == 0 && _inflow_ts == NULL ) {
		totErrs++;
		PrintError( routine, "Upstream reservoir %s must have an "
			"inflow time series.", _id );
	}

	// Check for missing input values that are required.
	// If they are MISSING, write error 
	if( _pool == MISSING ) {
		totErrs++;
		PrintError( routine, 
			"Keyword INITIALPOOL and Value required but not found "
			"in RESERVOIR %s.", _id );
	}
	else {
		// _prevPoolCO begins as MISSING, is assigned to _pool, but will
		//	be reassigned to MISSING if there are errors in 
		//	PREVIOUSPOOL input syntax.  
		if( _prevPoolCO == MISSING ) {
			// We only print an error if _pool was defined 
			//	(successfully defining _prevPoolCO default) but 
			//	_prevPoolCO was reassigned to MISSING by an 
			//	errors in the PREVIOUSPOOL syntax.
			totErrs++;
			PrintError( routine,
				"Keyword PREVIOUSPOOL and Value required but "
				"not found in RESERVOIR %s.", _id );
		}
	}

	// The following defaults to 0.0, but will be reassigned to MISSING if
	//	there are errors in input syntax
	if( _release == MISSING ) {
		totErrs++;
		PrintError( routine,
			"Keyword INITIALRELEASE and Value required but not "
			"found in reservoir %s ", _id );
	}
	if( _withdraw == MISSING ) {
		totErrs++;
		PrintError( routine,
			"Value required with keyword INITIALWITHDRAW but not "
			"found in reservoir %s.", _id );
	}
	if( _inflowCO == MISSING ) {
		totErrs++;
		PrintError( routine,
			"Keyword INITIALINFLOW and Value required but not "
			"found in RESERVOIR %s.", _id );
	}
	if( _prevReleaseCO == MISSING ) {
		totErrs++;
		PrintError( routine,
			"Keyword PREVIOUSRELEASE and Value required but not "
			"found in reservoir %s ", _id );
	}
	if( _prevWithdrawCO == MISSING ) {
		totErrs++;
		PrintError( routine,
			"Value required with keyword PREVIOUSWITHDRAW but not "
			"found in reservoir %s.", _id );
	}
	if( _prevInflowCO == MISSING ) {
		totErrs++;
		PrintError( routine,
			"Keyword PREVIOUSINFLOW and Value required but not "
			"found in RESERVOIR %s.", _id );
	}

	// Check for missing input values that are not required.
	// If they are MISSING, write a message and assign to appropriate 
	// default values.  NOTE: These values are initialized as MISSING in 
	// Reservoir::initialize() but may have been reassigned (appropriately)
	// above.  They are handled separately from the INITIAL* and PREVIOUS* 
	// values because they have no relationship to states (carry over).
	if( _min_pool == MISSING ) {
		if( table_found ) {
			_min_pool = _elev_stor_tbl.getMin( GETCOLUMN_1 );
			sprintf( message, "**NOTE** MINPOOL and Value not "
				"specified for reservoir \"%s\" - set to "
				"minimum value from Elevation-Storage table "
				"(%d).", _id, _min_pool );
			int length = strlen( message );
			int ipr = ResJSys::getIPR();
			ResJ_ccwrite( &length, message, &ipr );
		}
		else {
			totErrs++;
			PrintError( routine, 
				"Elevation-Storage table for reservoir %s has "
				"not been set.", _id );
			PrintWarning( 1, routine, "MINPOOL and Value not "
				"specified for reservoir \"%s\" - unable to "
				"set to minimum value from Elevation-Storage "
				"table.", _id );
		}
	}
	if( _min_release == MISSING ) {
		sprintf( message, "**NOTE** MINRELEASE and Value not "
			"specified for reservoir \"%s\" - set to 0.0", _id );
		int length = strlen( message );
		int ipr = ResJSys::getIPR();
		ResJ_ccwrite( &length, message, &ipr );
		_min_release = 0;
	}

	// Now we have everything input, return FAILURE if we have had errors
	if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}

	// Check up front to make sure the release, minimum pool, initial pool,
	// and release don't violate the minimum (and maximum) constraints.  
	// Actual failure only occurs if MINPOOL is not within the 
	// storage / elevation curve (or if the storage / elevation curve was 
	// not defined).  
	// NOTE: If MINPOOL was not explicitly defined by the user, it is set
	// above as the minimum value on the storage / elevation curve.
	// Release:
	if( _release < _min_release ) {
		PrintWarning( 1, routine, "Initial release is less than the "
			"minimum release specified for %s (%f < %f). Setting "
			"initial to minimum.", _id, _release / ffactor, 
			_min_release / ffactor );
		_release = _min_release;
	}

	// Pool elevation:

	// Address problems for which we return failure
	if ( !table_found ) {
		PrintError( routine, 
			"Elevation-Storage table for reservoir \"%s\" has not "
			"been set.", _id );
		return( STATUS_FAILURE );
	}
	// If _min_pool was defined by the user outside limits on the
	// storage / elevation curve:
	if ( _min_pool < _elev_stor_tbl.getMin( GETCOLUMN_1 ) ) {
		totErrs++;
		PrintError( routine, "MINPOOL is less than the minimum elev. "
			"specified in the elev. / storage table for %s (%f < "
			"%f).", _id, _min_pool / lfactor, 
			_elev_stor_tbl.getMin( GETCOLUMN_1) / lfactor );
		return( STATUS_FAILURE ) ;
	}
	if ( _min_pool > _elev_stor_tbl.getMax( GETCOLUMN_1 ) ) {
		PrintError( routine, "MINPOOL is more than the maximum elev. "
			"specified in the elev. / storage table for %s (%f < "
			"%f).", _id, _min_pool / lfactor, 
			_elev_stor_tbl.getMax( GETCOLUMN_1) / lfactor );
		return( STATUS_FAILURE );
	}

	// Address problems for which we reassign to an acceptable value.
	// INITIALPOOL values (_pool):
	if ( _pool < _min_pool ) {
		PrintWarning( 1, routine, "Initial pool is less than the "
			"minimum pool allowable for %s (%f < %f). Set initial "
			"to minimum.", _id, _pool / lfactor, 
		 	_min_pool / lfactor );
		_pool = _min_pool;
		_poolCO = _pool;
		_startPool = _pool;
	}
	if ( _pool > _elev_stor_tbl.getMax( GETCOLUMN_1 ) ) {
		PrintWarning( 1, routine, "Initial pool is more than the "
			"maximum elev. specified in the elev. / storage table "
			"for %s (%f < %f). Set initial to maximum.", _id, 
			_pool / lfactor, 
			_elev_stor_tbl.getMax( GETCOLUMN_1) / lfactor );
		_pool = _elev_stor_tbl.getMax( GETCOLUMN_1);
		_poolCO = _pool;
		_startPool = _pool;
	}
	if ( _prevPoolCO < _min_pool ) {
		PrintWarning( 1, routine, "Previous pool is less than the "
			"minimum pool allowable for %s (%f < %f). Set previous "
			"to minimum.", _id, _prevPoolCO / lfactor, 
		 	_min_pool / lfactor );
		_prevPoolCO = _min_pool;
		_prevPool = _prevPoolCO;
	}
	if ( _prevPoolCO > _elev_stor_tbl.getMax( GETCOLUMN_1 ) ) {
		PrintWarning( 1, routine, "Previous pool is more than the "
			"maximum elev. specified in the elev. / storage table "
			"for %s (%f < %f). Set Previous to maximum.", _id, 
			_prevPoolCO / lfactor, 
			_elev_stor_tbl.getMax( GETCOLUMN_1) / lfactor );
		_prevPoolCO = _elev_stor_tbl.getMax( GETCOLUMN_1);
		_prevPool = _prevPoolCO;
	}

	// Set the storage variables from pool parameters
	_storage = _elev_stor_tbl.lookup( _pool, GETCOLUMN_2, ALLOW_BOUNDS );
	_prevStorage = _elev_stor_tbl.lookup( _prevPool, GETCOLUMN_2, 
		ALLOW_BOUNDS );
	_startStorage = _storage;

	_n_output_ts = n_out;

	// Finally, need to put the carryover values of pool, release, and
	// inflow into the list needed by the pin58 FORTRAN interface to ResJ.
	setCOstring();

	// Below is the old method of doing so
/***************
	char temp_str[49];
	char rel_str[9];
	char pool_str[9];
	char with_str[9];
	sprintf( rel_str, "%f", _release ); 
	sprintf( pool_str, "%f", _pool ); 
	sprintf( with_str, "%f", _withdraw ); 
	sprintf( temp_str, "%-12.12s%-12.12s%4.4s%8.8s%8.8s%8.8s",
		_type, _id, "-999", rel_str, pool_str, with_str );
	if( ResJSys::addCOString( temp_str ) ) {
		PrintError( routine, "Troubles adding carryover data to CO "
			"array." );
		return( STATUS_FAILURE );
	}
***************/
	
	return( STATUS_SUCCESS ); 

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reservoir_setStates.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_setStates.cxx,v 1.16 2006/10/26 15:32:38 hsu Exp $";}
/*  ===================================================  */

}
