//------------------------------------------------------------------------------
// SetRelease::construct - Reads a stringlist and constructs release ts. 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 15 May 2001	James R. VanShaar, RTi	Improved error / warning handling in
//					SetRelease::construct, 
//					SetRelease::buildReleaseTS.
// 16 May 2001	JRV, RTi	Enhanced INTERPOLATE function input.
// 24 Nov 2001	JRV, RTi	Allowed for carry over blendtbl (& blendts) step 
//				values
// 27 Nov 2001	JRV, RTi	Revised usage of BLENDTBL and BLENDTS paramters to
//				handle using defaults for _tbl_step and _ts_step
//				if they are not explicity defined.
// 01 Aug 2002  KSH, HRL        Added BLENDTBL keyword to indicate table data
//                              blending. BLEND is retained for old datasets.
// 2003-11-21 Luiz Teixeira, RTi - Added list = FreeStringList( list ) at the 
//  				end of the main for loop in the construct method
// 09 Mar 2004	JRV, RTi	Added _inWeekScalars.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "SetRelease.h"
#include "TSUtil.h"
#include "ResJSys.h"		// Required to use ResJSys::getIPR to call 
				// ResJ_ccwrite()

int SetRelease::construct ( char** re_list, int n_items )  
{
	char routine[]="SetRelease::construct", **list=NULL, temp[MAXC],
		**value_list = NULL, ts_id[MAXC] = "";
	int i, j, nlist=0, n_value = 0, n_ts = 0, list_counter = n_items, 
		d_count = 0, ts_count = 0, rel_found = 0, totErrs = 0, 
		WarnedOnce = 0;
	TSDate	date;

	// If we get here, we have found (and removed) matching SETRELEASE and 
	//	ENDSETRELEASE keywords.
	for( i = 0; i < n_items; i++ ) { 	
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( re_list[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist );
		if( nlist == 0 || list == NULL ) {
			totErrs++;
			PrintError( routine, "Troubles getting release data "
				"for SetRelease %s %s.", _owner->getID(), _id );
			if( list ) {
				list = FreeStringList( list );
			}
			continue;
		}

		// Check for input time series - the only applicable input for
		//  this method would be an observed release time series.
		if( !strcasecmp( list[0], "TSINPUT" ) ) {
			if ( nlist < 3 ) {
				totErrs++;
				PrintError( routine, "Keyword OBSERVEDRELEASE "
					"and timeseries identifier required "
					"immediately after SETRELEASE method "
					"keyword %s.", list[0] );
				list = FreeStringList( list );
				continue;
			}

			// Read either full identifiers or alias 
			strcpy( ts_id, list[2] );
			for( j = 3; j < nlist; j++ ) {
				strcat( ts_id, list[j] );
				strcat( ts_id, " " );
			}
			if( !strcasecmp( list[1], "OBSERVEDRELEASE" ) ) {
				_release_obs = (HourTS*)TSList::getTSFromList( 
					ts_id );
				if( _release_obs == NULL ) {
					totErrs++;
					PrintError( routine, "Troubles setting "
						"release timeseries \"%s\" on "
						"SETRELEASE '%s.'", list[2], 
						_id );
					list = FreeStringList( list );
					continue;
				}
			}
			else {
				totErrs++;
				PrintError( routine, "Keyword OBSERVEDRELEASE "
					"and timeseries identifier required "
					"immediately after SETRELEASE method "
					"keyword %s.", list[0] );
				// Even though the time series keyword failed, 
				// we'll test for the alias name and the ability
				// to set the time series.
				_release_obs = (HourTS*)TSList::getTSFromList( 
					ts_id );
				if( _release_obs == NULL ) {
					totErrs++;
					PrintError( routine, "Troubles setting "
						"release timeseries \"%s\" on "
						"SETRELEASE '%s.'", list[2], 
						_id );
				}
				list = FreeStringList( list );
				continue;
			}
			list = FreeStringList( list );
			continue;
		}

		// Check to see if the mode has been specified as INTERPOLATE.
		// If it has not, then use the default - NORMAL
		else if( !strcasecmp( list[0], "INTERPOLATE" ) ) {
			if ( nlist == 1 ) {
				sprintf( temp, "**NOTE** SETRELEASE: "
					"Interpolate 'DIRECTION' is undefined "
					"for \"%s\".  Defaulting to "
					"interpolation in TIME.", _id );
				int length = strlen( temp );
				int ipr = ResJSys::getIPR(); 
				ResJ_ccwrite( &length, temp, &ipr );
				_mode = INTERPOLATE_TIME;
				list = FreeStringList( list );
				continue;
			}
			if ( !strcasecmp( list[1], "TIME" ) ) {
				_mode = INTERPOLATE_TIME;
				list = FreeStringList( list );
				continue;
			}
			else if ( !strcasecmp( list[1], "ELEV" ) ) {
				_mode = INTERPOLATE_ELEV;
				list = FreeStringList( list );
				continue;
			}
			else if ( !strcasecmp( list[1], "ALL" ) ||
				!strcasecmp( list[1], "BOTH" ) ) {
				_mode = INTERPOLATE_ALL;
				list = FreeStringList( list );
				continue;
			}
			else {
				PrintError( routine, "Unrecognized %s keyword "
					"\"%s\" for SetRelease %s.", list[0], 
					list[1], _id );
				list = FreeStringList( list );
				continue;
			}
		}

		// Check for BLENDTBL
		else if( !strcasecmp( list[0], "BLEND" ) ||
		         !strcasecmp( list[0], "BLENDTBL" ) ) {
			if( nlist > 1 ) {
				if( !IsInteger( list[1] ) ) {
					totErrs++;
					PrintError( routine, "%s value '%s' is "
						"not an integer.", list[0], 
						list[1] );
					list = FreeStringList( list );
					continue;
				}
				_n_blend_tbl = atoi( list[1] );
				_tbl_step = 1;
			}
			else {
				totErrs++;
				PrintError( routine, "Value required "
					"immediately after %s method "
					"keyword %s.", _type, list[0] );
				list = FreeStringList( list );
				continue;
			}
			if( nlist > 2 ) {
				if( !IsInteger( list[2] ) ) {
					totErrs++;
					PrintError( routine, "%s value '%s' is "
						"not an integer.", list[0], 
						list[2] );
					list = FreeStringList( list );
					continue;
				}
				_tbl_step = atoi( list[2] );
			}
			list = FreeStringList( list );
			continue;
		}

		// Check for BLENDTS
		else if( !strcasecmp( list[0], "BLENDTS" ) ) {
			if( nlist > 1 ) {
				if( !IsInteger( list[1] ) ) {
					totErrs++;
					PrintError( routine, "%s value '%s' is "
						"not an integer.", list[0], 
						list[1] );
					list = FreeStringList( list );
					continue;
				}
				_n_blend_ts = atoi( list[1] );
				_ts_step = _n_blend_ts + 1;
			}
			else {
				totErrs++;
				PrintError( routine, "Value required "
					"immediately after %s method "
					"keyword %s.", _type, list[0] );
				list = FreeStringList( list );
				continue;
			}
			if( nlist > 2 ) {
				if( !IsInteger( list[2] ) ) {
					totErrs++;
					PrintError( routine, "%s value '%s' is "
						"not an integer.", list[0], 
						list[2] );
					list = FreeStringList( list );
					continue;
				}
				_ts_step = atoi( list[2] );
			}
			list = FreeStringList( list );
			continue;
		}

		// Get sub lists as we go through the file. Each sub
		// list must start with "ELEV" and end with "ENDELEV". If
		// it doesn't, then the control file is malformed. Using
		// the sublist method prevents a lot of hassle and inefficiency
		// associated with reading the control file line-by-line.
		else if( !strcasecmp( list[0], "VALUES" ) ) {
			rel_found = 1;
			value_list = GetSubStringList( re_list, n_items, 
				"VALUES", &n_value, RETURN_NO_KEYWORDS );
			if( value_list == NULL || n_value == 0 ) {
				totErrs++;
				PrintError( routine,
					"Troubles forming sub_list beginning "
					"with %s for SETRELEASE method \"%s\". "
					" Does \"END%s\" exist?", list[0], 
					getID(), list[0] );
				list = FreeStringList( list );
				if ( value_list ) {
					value_list = 
						FreeStringList( value_list );
				}
				i += n_value + 1; 
				continue;
			}

			if( buildReleaseTS( value_list, n_value ) ){
				// if we get here the release table build
				// failed.  Due to the central importance of
				// the table, to anything done in this method,
				// there is no sense in continuing.
				PrintError( routine, "SetRelease VALUES table "
					"for %s on %s not successfully read.", 
					_id, _owner->_id );
				list = FreeStringList( list );
				value_list = FreeStringList( value_list );
				return( STATUS_FAILURE );
			}
			value_list = FreeStringList( value_list );
			list = FreeStringList( list );
			i += n_value + 1; 
			continue;
		}

		// Handle weekly variation.
		else if( !strcasecmp( list[0], "WEEKLYVARIATION" ) ) {
			// Determine how many values should be given
			int requiredSize = ( 24 / _t_mult ) * 7;

			// Compare required number of values with the number
			// provided.  Note: The "- 1" below removes the keyword
			// included in the count.
			if( nlist -1  != requiredSize ) {
				PrintError( routine, 
					"Keyword %s must preceed %d scaling "
					"values instead of %d. (%s %s %s)",
					list[0], requiredSize, nlist-1, _type, 
					_owner->_id, _id );
				list = FreeStringList( list );
				return( STATUS_FAILURE ); 
			}

			// Size the array.
			_inWeekScalars =  new double[ requiredSize ];

			// Populate the array.
			for( j = 0; j < requiredSize; j++ ) {
				_inWeekScalars[j]=atof( list[j+1] );
			}

			list = FreeStringList( list );
			continue;
		}


		else if ( !WarnedOnce ) {
				WarnedOnce++;
				PrintWarning( 1, routine, "'%s' is an "
					"unrecognized (or misplaced) keyword.  "
					"If there are other errors or warnings "
					"for SetRelease %s, this may disappear "
					"following their correction.", 
					list[0], _id );
		}
		
		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}
	}
	
	if( rel_found == 0 ) {
		totErrs++;
		PrintError( routine, "SetRelease keyword 'VALUES' not found "
			"for %s on %s.", _id, _owner->_id );
	}

	if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}

        if( _n_blend_tbl < 0 ) {
                _n_blend_tbl = 1;
        }
        if( _n_blend_ts < 0 ) {
                _n_blend_ts = 1;
        }

	// We have to write the carryover to the CO array at the System
	// level...
	setCOstring();

	_is_constructed = 1;
	return( STATUS_SUCCESS );
}		


int SetRelease::buildReleaseTS( char** rel, int nrel )
{
	// This function will create an array of Distributed timeseries',
	// each timeseries will correspond to the releases for a given 
	// elevation.

	char routine[]="SetRelease::buildReleaseTS", **list=NULL, temp[MAXC];
	int i, k, nlist=0, n_value = 0, n_ts = 0, d_count = 0, ts_count = 0; 
	TSDate	date, date1, date2;
	float value, dist_tally = 0.0, *dist = NULL;
	double lfactor = 1.0, ffactor = 1.0;

	if( Method::getUnitType() == ENGLISH ) {
		lfactor = 0.3048;
		ffactor = 0.028317;
	}

	for( i = 0; i < nrel; i++ ) {
		list = BreakStringList( rel[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist );
		if( nlist == 0 || list == NULL ) {
			PrintWarning( 1, routine, "Troubles getting"
				" release data for SetRelease '%s'.",
				_id );
			if( list ) {
				list = FreeStringList( list );
			}
			return( STATUS_FAILURE );
		}
		// Do this stuff only if we are at the first line...
		if( i == 0 ) {
			if( strcasecmp( list[nlist - 1], "endelev" ) ||
				strcasecmp( list[0], "elev") ) { 
				PrintError( routine, 
					"First line in SetRelease VALUES table "
					"must begin with 'ELEV' and end with "
					"'ENDELEV' for SetRelease %s.", _id );
				list = FreeStringList( list );
				return( STATUS_FAILURE ); 
			}

			// Set the number of time series we are 
			// dealing with... (one for each elevation defined.)
			n_ts = nlist - 2;
			_n_elev = n_ts;
			if ( n_ts <= 1 ) {
				PrintError( routine,
					"At least 2 elevation points must be "
					"defined for SetRelease %s VALUES.",
					_id );
				list = FreeStringList( list );
				return( STATUS_FAILURE ); 
			}

			// We can dynamically allocate memory here 
			// for the _release_ctl and _elev arrays...
			_elev =  new double[ n_ts ];
			_release_ctl = new DistributedTS [ n_ts ];
			for( k = 0; k < n_ts; k++ ) {
				_release_ctl[k].setRelativeFlag( 1 );
			}
			double lastval = -900;
			double thisval;
			for( k = 1; k < nlist - 1; k++ ) {
				// Fill the _elev array while checking that
				// they were input in ascending order
				thisval = atof( list[k] );
				if ( thisval <= lastval ) {
					PrintError( routine,
						"SetRelease (%s) ELEV values "
						"must be in ascending order. "
						"(%s >= %s)", _id, list[k-1],
						list[k] );
					list = FreeStringList( list );
					return( STATUS_FAILURE );
				}
				lastval = thisval;
				_elev[k - 1] = thisval * lfactor;
			}
			list = FreeStringList( list );
			continue;
		}

		// If we are here, we have allocated the necessary memory 
		// and we are ready to start reading in the time series.

		// Check # of fields in the current line of the table (nlist)
		//	vs. # of elevations (or timeseries)
		if( nlist > n_ts + 1 ) {
			dist_tally = 0.0;

			// If the user did not specify a dist- 
			// ribution value for every timestep, 
			// then exit.  
			if( nlist != n_ts + 1 + ( 24 / _t_mult ) ){ 
				PrintError( routine, 
					"%s %s SetRelease must have one and "
					"only one distribution value "
					"for each timestep.",
					_owner->_id, _id );
				list = FreeStringList( list );
				return( STATUS_FAILURE ); 
			}

			sprintf( temp, "%s", list[0] );
			date = TSUtil::getTSDateFromString(
				temp);
			if( i == 1 ) {
				date1 = date;
			}
			if( i == nrel - 1 ) {
				date2 = date;
			}

			// Temp array to store dist
			// values - then stored on the
			// TSDistData object.
			dist = new float[nlist-n_ts+1];
			d_count = 0;
			for( k = n_ts+1; k<nlist; k++, d_count++ ){
				dist[d_count] = atof( list[k] );
				dist_tally += atof( list[k] );
			}
			for( k = 1, ts_count = 0; k <= n_ts; 
			k++, ts_count++ ) {
				// Get the data value. 
				value = (double)(atof( list[k] ) ) * ffactor;

				// The setDataValue will 
				// store the distribution 
				// values in dist in the
				// TSData object.
				_release_ctl[ts_count].setDataValue(
				   date, value, dist, d_count ); 
			}

			// Distribution must sum to 1.0.
			if ( dist_tally > 1.00001 || 
				dist_tally < 0.99999 ) {
				PrintError( routine,
 					"Distribution on %s %s SetRelease "
					"does not sum to 1.0.",
					_owner->_id, _id );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
		}

		else if( nlist == n_ts + 1 ) {
			// No distribution here. Must assume a
			// half and half distributution.
			date = TSUtil::getTSDateFromString( 
				list[0] );
		
			// If there is no distribution, then handle
			// the default situation down at the
			// time series level.
			for( k = 1, ts_count = 0; k <= n_ts; k++,
				ts_count++ ) {
				value = (double)atof( list[k] ) * ffactor;
				_release_ctl[ts_count].setDataValue( 
					date, value, NULL, 24/_t_mult );
			}

			if( i == 1 ) {
				date1 = date;
			}
			if( i == nrel - 1 ) {
				date2 = date;
			}
		}

		else {
			PrintError( routine, "incorrect number of "
				"values on SETRELEASE method \"%s\" line "
				"\"%s\".", _id, rel[i] );
			list = FreeStringList( list );
			return(STATUS_FAILURE);
		}
		
		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}

		continue;
	}

	// Set the date1 and date2 on all of the time series in _release_ctl
	for( i = 0; i < _n_elev; i++ ) {
		_release_ctl[i].setDate1( date1 );
		_release_ctl[i].setDate2( date2 );
	}

	// Delete the dist array and the list.
	if( dist ) {
		delete [] dist;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetRelease_construct.cxx,v $";
 static char rcs_id2[] = "$Id: SetRelease_construct.cxx,v 1.11 2006/10/26 15:34:16 hsu Exp $";}
/*  ===================================================  */

}
