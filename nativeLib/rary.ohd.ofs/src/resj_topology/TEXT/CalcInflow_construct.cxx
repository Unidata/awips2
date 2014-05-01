//------------------------------------------------------------------------------
// CalcInflow::construct - Reads a stringlist and constructs release ts. 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Feb 2004	James R. VanShaar, RTi	Created initial version.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "CalcInflow.h"
#include "ResJSys.h"		// Required to use ResJSys::getIPR to call 
				// ResJ_ccwrite()

int CalcInflow::construct ( char** re_list, int n_items )  
{
	char routine[]="CalcInflow::construct", **list=NULL, temp[MAXC],
		**sub_list = NULL, ts_id[MAXC] = "";
	int i, j, nlist=0, nsublist = 0, table_found = 0, totErrs = 0, 
		WarnedOnce = 0;
	double ffactor=1,lfactor=1,vfactor=1,tempVal;

	if( Method::getUnitType() == ENGLISH ) {
		ffactor = 0.028317;
		lfactor = 0.3048;
		vfactor = 1233.5;
	}

	// If we get here, we have found (and removed) matching CALCINFLOW and 
	//	ENDCALCINFLOW keywords.
	for( i = 0; i < n_items; i++ ) {
		if( list ) {
			list = FreeStringList( list );
		}

		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( re_list[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist );
		if( nlist == 0 || list == NULL ) {
			totErrs++;
			PrintError( routine, "Troubles getting data "
				"for %s %s %s.", _type, _owner->getID(), _id );
			continue;
		}

		// Check for input time series
		if( !strcasecmp( list[0], "TSINPUT" ) ) {
			if ( nlist < 3 ) {
				totErrs++;
				PrintError( routine, "Keyword "
					"and timeseries identifier required "
					"immediately after %s method "
					"keyword %s.", _type, list[0] );
				continue;
			}

			// Read either full identifiers or alias 
			strcpy( ts_id, list[2] );
			for( j = 3; j < nlist; j++ ) {
				strcat( ts_id, list[j] );
				strcat( ts_id, " " );
			}
			if( !strcasecmp( list[1], "POOL" ) ) {
				_pool_obs = (HourTS*)TSList::getTSFromList( 
					ts_id );
				if( _pool_obs == NULL ) {
					totErrs++;
					PrintError( routine, "Troubles setting "
						"%s timeseries \"%s\" on "
						"%s %s %s.", list[1], list[2],
						_type, _owner->_id, _id );
					continue;
				}
			}
			else if( !strcasecmp( list[1], "RELEASE" ) ) {
				_release_obs = (HourTS*)TSList::getTSFromList( 
					ts_id );
				if( _release_obs == NULL ) {
					totErrs++;
					PrintError( routine, "Troubles setting "
						"%s timeseries \"%s\" on "
						"%s %s %s.", list[1], list[2],
						_type, _owner->_id, _id );
					continue;
				}
			}
			else if( !strcasecmp( list[1], "WITHDRAW" ) ) {
				_withdraw_obs = (HourTS*)TSList::getTSFromList( 
					ts_id );
				if( _withdraw_obs == NULL ) {
					totErrs++;
					PrintError( routine, "Troubles setting "
						"%s timeseries \"%s\" on "
						"%s %s %s.", list[1], list[2],
						_type, _owner->_id, _id );
					continue;
				}
			}
			else {
				totErrs++;
				PrintError( routine, "Unrecognized %s keyword "
					"\"%s\" for %s %s %s.", list[0],
					list[1], _type, _owner->_id, _id );
				continue;
			}
			continue;
			//NOTE: The only way out of this "if" is with a 
			//      "continue".
		}

		// Check for output time series
		if( !strcasecmp( list[0], "TSOUTPUT" ) ) {
			if ( nlist < 3 ) {
				totErrs++;
				PrintError( routine, "Keyword "
					"and timeseries identifier required "
					"immediately after %s method "
					"keyword %s.", _type, list[0] );
				continue;
			}

			// Read either full identifiers or alias 
			strcpy( ts_id, list[2] );
			for( j = 3; j < nlist; j++ ) {
				strcat( ts_id, list[j] );
				strcat( ts_id, " " );
			}
			if( !strcasecmp( list[1], "INFLOW" ) ) {
				// We need to assign the pointer to 
				// _inflow_calc to the Component's list of
				// output timeseries.  See _output_ts in
				// Component.h, and Reservoir::setStates for
				// more background.
				j=0;
				// Find the first non-used HourTS pointer in the
				// _output_ts array.
				while(j < 15 && _owner->_output_ts[j] != NULL){
					// NOTE: the 15 is a hard coded size of
					// the _output_ts array (see 
					// Component.h)
					j++;
				}
				if( j == 15 ) {
					totErrs++;
					PrintError( routine, "Exceeded the "
						"limit of 15 output timeseries "
						"from Component %s while "
						"working with %s %s.", 
						_owner->_id, _type, _id );
					list = FreeStringList( list );
					continue;
				}
				_owner->_output_ts[j] = _inflow_calc;

				if( _owner->_output_ts[j] == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
						"%s as a valid output time "
						"series.", ts_id );
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
				_owner->_output_ts[j]->setIdentifier( temp );

				_owner->_n_output_ts++;
			}
			else {
				totErrs++;
				PrintError( routine, "Unrecognized %s keyword "
					"\"%s\" for %s %s %s.", list[0],
					list[1], _type, _owner->_id, _id );
				continue;
			}
			_outputCalcTS = 1;

			continue;
			//NOTE: The only way out of this "if" is with a 
			//      "continue".
		}

		else if( !strncasecmp( list[0], "START", 5 ) ) {
			// We have a carryover START value
			if( !strcasecmp( list[0], "STARTPOOL" ) ) {
				tempVal = (double)atof( list[1] );
				// Check for near MISSING values, which we will
				// handle as MISSING values.
				if( tempVal != MISSING ) {
					tempVal *= lfactor;
					if( tempVal < -997.9 && 
						tempVal > -999.1 ) {
						tempVal = MISSING;
					}
				}
				_startPool=tempVal;
				continue; 
			}

			tempVal = (double)atof( list[1] );
			if( tempVal != MISSING ) {
				tempVal *= ffactor;
				// Check for near MISSING values, which we will
				// handle as MISSING values.
				if( tempVal < -997.9 && 
					tempVal > -999.1 ) {
					tempVal = MISSING;
				}
			}
	 		// Handle keyword START*
			if( !strcasecmp( list[0], "STARTRELEASE" ) ) {
				_startRelease=tempVal;
			}
			else if( !strcasecmp( list[0], "STARTWITHDRAWAL" ) ||
				!strcasecmp( list[0], "STARTWITHDRAW" ) ) {
				_startWithdrawal=tempVal;
			}
			else if( !strcasecmp( list[0], "STARTINFLOW" ) ) {
				_myValue=tempVal;
				_startInflow=tempVal;
			}
			// We continue back for the next line.
		}

		// Check for USEFORSIM
		else if( !strcasecmp( list[0], "USEFORSIM" ) ) {
			if ( nlist == 1 ) {
				sprintf( temp, "**NOTE** %s: "
					"%s instruction is undefined "
					"for %s on %s.  Defaulting to "
					"USE calculated inflow in "
					"simulation.", _type, 
					list[0], _owner->_id, _id );
				int length = strlen( temp );
				int ipr = ResJSys::getIPR(); 
				ResJ_ccwrite( &length, temp, &ipr );
				_useForSim=1;
				continue;
			}
			if ( !strcasecmp( list[1], "YES" ) ) {
				_useForSim = 1;
				continue;
			}
			else if ( !strcasecmp( list[1], "NO" ) ) {
				_useForSim = 0;
				continue;
			}
			else {
				PrintError( routine, "Unrecognized %s keyword "
					"\"%s\" for %s %s %s.", list[0], 
					list[1], _type, _owner->_id, _id );
				continue;
			}
		}

 		// Handle keyword TABLE
		else if( !strcasecmp( list[0], "TABLE" ) ) {
			// Extract table sublist
			sub_list = GetSubStringList( &re_list[i], n_items-i, 
				"TABLE", &nsublist, RETURN_NO_KEYWORDS );
			if( sub_list == NULL || nsublist == 0 ) {
				totErrs++;
				PrintError( routine, 
					"Troubles forming sub_list for "
					"table values for %s %s %s.  Does "
					"\"ENDTABLE\" exist?", _type, 
					_owner->_id, _id );
				if( sub_list != NULL ) {
					sub_list = FreeStringList( sub_list );
				}
				else {
					// We found nothing between TABLE and
					// ENDTABLE.  Skip the ENDTABLE line.
					i++;
				}
				continue;
			}
			
			// Test for enough fields on TABLE identifying line
			if ( nlist < 2 ) {
				totErrs++;
				PrintError( routine,
					"Keyword MAXCHANGE required "
					"immediately after %s for %s %s %s.",
					list[0], _type, _owner->_id, _id );
			}

			// Check for keyword "MAXCHANGE"
			else if( !strcasecmp( list[1], "MAXCHANGE" ) ) {
				if ( table_found == 1 ) {
					PrintWarning( 1, routine,
						"Duplicate %s %s for %s %s %s. "
						"Last definition used.", 
						list[0], list[1], _type, 
						_owner->_id, _id );
				}
				_maxDecrease.allocateDataSpace(nsublist);
				_maxIncrease.allocateDataSpace(nsublist);
				if( buildTables( &sub_list[0], nsublist,
					ffactor) ) {
					totErrs++;
					PrintError( routine, 
						"Troubles filling %s %s for %s "
						"%s %s.", list[0], list[1], 
						_type, _owner->_id, _id );
					_maxDecrease.freeDataSpace();
					_maxIncrease.freeDataSpace();
					table_found = 0;
				}
				else {
					table_found = 1;
					_constrainChange = 1;
				}
			}
			else {	
				totErrs++;
				PrintError( routine,
					"Keyword MAXCHANGE required "
					"immediately after %s for %s %s %s.", 
					list[0], _type, _owner->_id, _id );
			}

			// Increment beyond TABLE sub_list
			i += (nsublist + 1);
			sub_list = FreeStringList( sub_list );
			continue;
		}

 		// Handle keyword INTERPOLATE
		else if( !strcasecmp( list[0], "INTERPOLATE" ) ) {
			_mode = INTERPOLATE; 
			continue; 
		} 

 		// Handle keyword MININFLOW
		else if( !strcasecmp( list[0], "MININFLOW" ) ) {
			_minInflow = (double)atof( list[1] );
			if( _minInflow != MISSING ) {
				_minInflow *= ffactor;
				if( _minInflow < -997.9 && 
					_minInflow > -999.1 ) {
					_minInflow = MISSING;
				}
			}
			if( _minInflow == MISSING ) {
				// We cannot have this value
				totErrs++;
				PrintError( routine,
					"%s cannot handle special cases based "
					"on MISSING or near-MISSING values. "
					"\"%s\" causes this problem for %s %s "
					"%s.", _type, re_list[i],  _owner->_id,
				       	_id );
			}
			continue; 
		}

 		// Handle keyword REMAININGVOL
		else if( !strcasecmp( list[0], "REMAININGVOL" ) ) {
			_remainingVol = (double)atof( list[1] );
			_remainingVol *= vfactor;
			continue; 
		}

		else if ( !strcasecmp( list[0], "UseLoss" ) ) {
			_useLoss = 1;
		}

		else if ( !WarnedOnce ) {
				WarnedOnce++;
				PrintWarning( 1, routine, "'%s' is an "
					"unrecognized (or misplaced) keyword.  "
					"If there are other errors or warnings "
					"for CalcInflow %s, this may disappear "
					"following their correction.", 
					list[0], _id );
		}
		
	}
	// Freeing memory
	if ( list ) {
		list = FreeStringList( list );
	}

	if( _startInflow == MISSING ) {
		// We prefer to never be missing the value for _startInflow.
		// Therefore, we will assign the value defined on the owning 
		// reservoir.
		_startInflow= _owner->_inflowCO;
	}

	if( _pool_obs == NULL ) {
		// We MUST have an observed pool timeseries for us to do any
		// good.
		totErrs++;
		PrintError( routine, "Pool observation timeseries required "
			"(but not parameterized) for %s %s %s.", _type, 
			_owner->getID(), _id );
	}
	if( _release_obs == NULL && _startRelease != MISSING ) {
		// It makes no sense to have parameterized a startRelease
		// without intention of using releases from a release 
		// timeseries.
		sprintf( temp, "**NOTE** Parameterized startRelease value will "
			"be ignored because no release timeseries has been "
			"parameterized for %s %s %s.", _type, _owner->_id, 
			_id );
		int length = strlen( temp );
		int ipr = ResJSys::getIPR(); 
		ResJ_ccwrite( &length, temp, &ipr );
	//	PrintWarning( 1, routine, "Parameterized startRelease value "
	//		"will be ignored because no release timeseries has "
	//		"been parameterized for %s %s %s.", _type, 
	//		_owner->getID(), _id );
		_startRelease = MISSING;
	}
	if( _withdraw_obs == NULL && _startWithdrawal != MISSING ) {
		// It makes no sense to have parameterized a startWithdrawal
		// without intention of using withdrawals from a withdrawal
		// timeseries.
		sprintf( temp, "**NOTE** Parameterized startWithdrawal value "
			"will be ignored because no release timeseries has "
			"been parameterized for %s %s %s.", _type, _owner->_id, 
			_id );
		int length = strlen( temp );
		int ipr = ResJSys::getIPR(); 
		ResJ_ccwrite( &length, temp, &ipr );
	//	PrintWarning( 1, routine, "Parameterized startWithdrawal value "
	//		"will be ignored because no withdrawal timeseries has "
	//		"been parameterized for %s %s %s.", _type, 
	//		_owner->getID(), _id );
		_startWithdrawal = MISSING;
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


int CalcInflow::buildTables( char** infl, int ninfl, double ffactor )
{
	// This function will populate two tables (already sized) based on the
	// data strings passed in.

	char routine[]="CalcInflow::buildTables", **list=NULL, temp[MAXC];
	int i, nlist=0;
	double LastInflow, value, lastRowVal;

	for( i = 0; i < ninfl; i++ ) {
		list = BreakStringList( infl[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist );
		if( nlist == 0 || list == NULL ) {
			PrintWarning( 1, routine, "Troubles getting "
				"MaxChange data for %s %s %s.",
				_type, _owner->_id, _id );
			if( list ) {
				list = FreeStringList( list );
			}
			return( STATUS_FAILURE );
		}
		else if( nlist != 3 ) {
			PrintError( routine, "Incorrect number of "
				"values on CALCINFLOW method \"%s\" line "
				"\"%s\".", _id, infl[i] );
			list = FreeStringList( list );
			return(STATUS_FAILURE);
		}
		

		// Populate the flow value in both tables
		LastInflow = (double)atof( list[0] ) * ffactor;
		if( i != 0 ) {
			// Check for assending order in the flow column
			if( LastInflow <= lastRowVal ) {
				// We cannot use this table
				PrintError( routine, "MAXCHANGE table must "
					"contain ascending values in the first "
					"column.  (%s %s %s)", _type, 
					_owner->_id, _id );
				list = FreeStringList( list );
				return(STATUS_FAILURE);
			}
		}
		lastRowVal = LastInflow;
		_maxIncrease.populate( i, 0, LastInflow );
		_maxDecrease.populate( i, 0, LastInflow );

		// Populate the change value in the maxIncrease table
		value = (double)atof( list[1] ) * ffactor;
		_maxIncrease.populate( i, 1, value );

		// Populate the change value in the maxDecrease table
		value = (double)atof( list[2] ) * ffactor;
		_maxDecrease.populate( i, 1, value );


		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );
		}

	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_construct.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_construct.cxx,v 1.2 2006/10/26 15:11:39 hsu Exp $";}
/*  ===================================================  */

}
