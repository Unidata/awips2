//------------------------------------------------------------------------------
// RainEvap::construct - Reads a stringlist and constructs precip and/or
//				evap timeseries data.
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
// 24 Sep 1998	DKW,		Moved input (obs) time series storage to the 
// 				Method level from the Reservoir level.
// 11 Jul 2003	James R. VanShaar, RTi	Revised structure for error trapping and
// 					to be more consistent with other methods
// 2003-11-21 Luiz Teixeira, RTi - Added code to free memory of precip_list and 
//				after closing the main loop.
// MR1911 r24-51 Kuang Hsu, OHD - no PRECIP-ENDPRECIP needed,
//                                no VALUES-ENDVALUES needed in PRECIP-ENDPRECIP
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "RainEvap.h"
#include "TSUtil.h"
#include "TSList.h"
#include <stdio.h>

int RainEvap::construct ( char** re_list, int n_items )  
{
	char routine[]="RainEvap::construct", **list=NULL, temp[MAXC],
		**precip_list=NULL, **evap_list = NULL, ts_ident[MAXC],
		**p_val_list = NULL, **e_val_list = NULL, 
		**junk_val_list = NULL, ts_id[MAXC] = "";	
	int i, j, k, nlist=0, n_precip = 0, n_evap_list = 0, n_junk = 0,
		d_count = 0, precipTbl_found = 0, evapTbl_found = 0,
		WarnedOnce = 0, totErrs = 0, n_precip_val = 0, n_evap_val = 0,
		lastGroup = 0, n_piece = 0;
	TSDate date, date1, date2;
	float value, dist_tally = 0, *dist = NULL;
	double factor = 1.0;


	if( Method::getUnitType() == ENGLISH ) {
		// Convert inches to mm
		factor = 25.4;
	}

	// NOTES:  Unfortunately, the input format has not been uniquely
	//         defined, in terms of the ordering of the terms.  For
	// example, the documentation suggests that you can have TSINPUT
	// anywhere in the syntax, including within the PRECIP and ENDPRECIP
	// (EVAP and ENDEVAP) block.  This means we have to be more careful
	// in how we handle the parameters
	// In the end, we need to look through each line for a PRECIP (EVAP)
	// or TSINPUT keywords.
	for( i = 0; i < n_items; i++ ) { 
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}
		if( list ) {
			list = FreeStringList( list );
		}
		list = BreakStringList( re_list[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist );
		if( list == NULL || nlist == 0 ) {
			totErrs++;
			PrintError( routine, "Troubles getting data for "
				"RainEvap %s on %s.", _id, _owner->_id );
			continue;
		}
		
		// Look for observed observed time series.
		if( !strcasecmp( list[0], "TSINPUT" ) ) {
			// Read either full identifiers or alias 
			strcpy( ts_id, list[2] );
			for( j = 3; j < nlist; j++ ) {
				strcat( ts_id, list[j] );
				strcat( ts_id, " " );
			}
			if( !strcasecmp( list[1], "OBSERVEDPRECIP" ) ) {
				_precip_obs = (HourTS*)TSList::getTSFromList( 
					ts_id );
				if( _precip_obs == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
						"%s as a valid time series.",
						ts_id );
					continue;
				}
			}
			else if( !strcasecmp( list[1], "OBSERVEDEVAP" ) ) {
				_evap_obs = (HourTS*)TSList::getTSFromList( 
					ts_id );
				if( _evap_obs == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
						"%s as a valid time series.",
						ts_id );
					continue;
				}
			}
			else {
				totErrs++;
				PrintError( routine,"%s not a recognized "
					"input time series for RainEvap "
					"method.", list[1] );
				continue;
			}
			continue;
		}


		// Look for PRECIP SECTION
		if( !strcasecmp( list[0], "PRECIP" ) ) {
			PrintWarning( 1, routine, "PRECIP keyword found, but not "
					"needed in %s %s on %s. ", 
					_type, _id, _owner->_id );
/*
			// Break out the PRECIP -- ENDPRECIP list
			// But first check to see if we alreay have one.
			if( precip_list ) {
				// Print an error.  We can't make sense out of
				// what might happen with two sections.
				totErrs++;
				PrintError( routine, "It appears you have "
					"two PRECIP sections for %s %s on %s. "
					"Confused.", _type, _id, 
					_owner->getID() );
				continue;
			}
			precip_list = GetSubStringList( re_list, n_items, 
				"PRECIP", &n_precip, RETURN_NO_KEYWORDS );
			if( precip_list == NULL || n_precip == 0 ) {
				// We have no list!
				totErrs++;
				PrintError( routine, "PRECIP -- ENDPRECIP "
					"section in %s %s on %s malformed. ", 
					_type, _id, _owner->_id );
				if( precip_list ) {
					precip_list = FreeStringList( 
						precip_list );
				}
				continue;
			}
			// We have a PRECIP -- ENDPRECIP list

			// Break out the VALUE -- ENDVALUE list from the PRECIP
			// list.
			p_val_list = GetSubStringList( precip_list, n_precip, 
				"VALUES", &n_precip_val, RETURN_NO_KEYWORDS );
			if( p_val_list == NULL || n_precip_val == 0 ) {
				// We have no list!
				totErrs++;
				PrintError( routine, "PRECIP -- ENDPRECIP "
					"section should contain a VALUES -- "
					"ENDVALUES section but does not.  Look "
					"for both keywords in your input.  %s "
					"%s on %s malformed.", _type, _id,
					_owner->_id );
				if( p_val_list ) {
					p_val_list = FreeStringList(p_val_list);
				}
				continue;
			}
*/
			// We have a VALUE -- ENDVALUE list.
			// We will actually handle it outside of this region.
			precipTbl_found = 1;	
			lastGroup = 1;
			continue;
		}


		// Look for EVAP SECTION
		if( !strcasecmp( list[0], "EVAP" ) ) {
			// Break out the EVAP -- ENDEVAP list
			// But first check to see if we alreay have one.
			if( evap_list ) {
				// Print an error.  We can't make sense out of
				// what might happen with two sections.
				totErrs++;
				PrintError( routine, "It appears you have "
					"two EVAP sections for %s %s on %s. "
					"Confused.", _type, _id, 
					_owner->getID() );
				continue;
			}
			evap_list = GetSubStringList( re_list, n_items, 
				"EVAP", &n_evap_list, RETURN_NO_KEYWORDS );
			if( evap_list == NULL || n_evap_list == 0 ) {
				// We have no list!
				totErrs++;
				PrintError( routine, "EVAP -- ENDEVAP "
					"section in %s %s on %s malformed. ", 
					_type, _id, _owner->_id );
				if( evap_list ) {
					evap_list = FreeStringList( evap_list );
				}
				continue;
			}
			// We have a EVAP -- ENDEVAP list

			// Break out the VALUE -- ENDVALUE list from the EVAP
			// list.
			e_val_list = GetSubStringList( evap_list, n_evap_list, 
				"VALUES", &n_evap_val, RETURN_NO_KEYWORDS );
			if( e_val_list == NULL || n_evap_val == 0 ) {
				// We have no list!
				totErrs++;
				PrintError( routine, "EVAP -- ENDEVAP "
					"section should contain a VALUES -- "
					"ENDVALUES section but does not.  Look "
					"for both keywords in your input.  %s "
					"%s on %s malformed.", _type, _id,
					_owner->_id );
				if( e_val_list ) {
					e_val_list = FreeStringList(e_val_list);
				}
				continue;
			}
			// We have a VALUE -- ENDVALUE list
			// We will actually handle it outside of this region.
			evapTbl_found = 1;	
			lastGroup = 2;
			continue;
		}

		// Look for VALUES.  It is handled within the PRECIP or EVAP
		// sections, so we can skip over the VALUE -- ENDVALUE section
		if( !strcasecmp( list[0], "VALUES" ) ) {
			if( lastGroup == 1 ) {
/*
				// We are likely within a PRECIP -- ENDPRECIP
				// section
				junk_val_list = GetSubStringList( precip_list, 
					n_precip, "VALUES", &n_junk, 
					RETURN_NO_KEYWORDS );
*/
			}
			else if (lastGroup == 2) {
				// We are likely within a EVAP -- ENDEVAP
				// section
				junk_val_list = GetSubStringList( evap_list, 
					n_evap_list, "VALUES", &n_junk, 
					RETURN_NO_KEYWORDS );
			}
			else {
				// We aren't in either section, print a warning
				totErrs++;
				PrintError( routine, "VALUES -- ENDVALUES "
					"section should ONLY be within "
					"an EVAP -- ENDEVAP "
					"section but is not.  %s %s on %s "
					"malformed.", _type, _id, _owner->_id );
/*
				PrintError( routine, "VALUES -- ENDVALUES "
					"section should be within a PRECIP -- "
					"ENDPRECIP or an EVAP -- ENDEVAP "
					"section but is not.  %s %s on %s "
					"malformed.", _type, _id, _owner->_id );
*/
				continue;

			}
			if( junk_val_list != NULL ) {
				// Increment i over the VALUE -- ENDVALUE list,
				// including the ENDVALUE row.
				i += n_junk + 1;
				junk_val_list = FreeStringList( junk_val_list );
			}
			continue;
		}
		if( !strcasecmp( list[0], "ENDPRECIP" ) ) {
			PrintWarning( 1, routine, "ENDPRECIP keyword found, but not "
					"needed in %s %s on %s. ", 
					_type, _id, _owner->_id );
		}
		if( !strcasecmp( list[0], "ENDPRECIP" ) ||
			!strcasecmp( list[0], "ENDEVAP" ) ) {
			// We have arrived at the end of the group
			lastGroup = 0;
			continue;
		}
/*
		// If we are not dealing with either a PRECIP or EVAP section,
		// print a warning and move on to the next line.
		if ( !WarnedOnce ) {
				WarnedOnce++;
				PrintWarning( 1, routine, "'%s' is an "
					"unrecognized (or misplaced) keyword.  "
					"If there are other errors or warnings "
					"for %s %s on %s, this may disappear "
					"following their correction.", list[0], 
					_type, _id, _owner->getID() );
		}
*/	
	}
	
	// Freeing memory
	if( list ) {
		list = FreeStringList( list );
	}
	
	// Freeing memory - 2003-11-21 LT
	if( precip_list ) {
		precip_list = FreeStringList( precip_list );
	}
	
	if( evap_list ) {
		evap_list = FreeStringList( evap_list );
	}
	

	// Now we may have two separate lists (e_val_list and p_val_list)
	// that contain all of the data for the average value tables.  
	// We now need to loop through the individual lists to convert them 
	// into timeseries
/*
	// First, lets do the precip TS... 
	if( precipTbl_found ) {
		_n_precip = n_precip_val;
		for( i = 0; i < n_precip_val; i++ ) {
			if( strlen( p_val_list[i] ) == 0 || 
				p_val_list[i][0] == '#' ){
				continue;
			}
			list = BreakStringList( p_val_list[i], " \n\t", 
				DELIM_SKIP_BLANKS, &nlist );
			if( list == NULL || nlist == 0 ) {
				if( list ) {
					list = FreeStringList( list );
				}
				continue;
			}
	
			if( nlist > 2 ) {
				dist_tally = 0.0;
	
				// If the user did not specify a dist-
				// ribution value for every timestep,
				// then exit.
				if( nlist != 2 + ( 24 / _t_mult) ) {
					totErrs++;
					PrintError( routine, "%s %s "
						"RainEvap must have one and "
						"only one distribution value "
						"for each timestep.", 
						_owner->getID(), _id );
					list = FreeStringList( list );
					continue;
				}
	
				// Construct a temporary array to hold
				// distribution values in...
				dist = new float[nlist - 2];
	
				// Read the distribution values
				// into dist
				d_count = 0;
				for( k = 2; k<nlist; k++, d_count++ ){
					dist[d_count] = atof( list[k] );
					dist_tally += atof( list[k] );
				}
	
				// Check to make sure that the number of 
				// distribution values equals 24/time step
				if( d_count != 24/_t_mult ) {
					totErrs++;
					PrintError( routine, "Number of "
						"distribution values must "
						"equal number of time steps "
						"per day. Problem on %s %s "
						"RainEvap.", _owner->getID(), 
						_id );
					list = FreeStringList( list );
					continue;
				}
	
				// The distribution of data for a given
				// time step must sum to 1.0
				if( dist_tally > 1.00001
					|| dist_tally < 0.99999 ){
					totErrs++;
					PrintError( routine, "Distribution on "
						"%s %s RainEvap does not sum "
						"to 1.0.", _owner->getID(), 
						_id );
					list = FreeStringList( list );
					continue;
				}	
	
				// Construct the TSDate...
				sprintf( temp, "%s",list[0] );
				date = TSUtil::getTSDateFromString( temp);
				value = atof( list[1] ) * factor;		
	
				_precip_ctl.setDataValue( date, value,
					dist, d_count );
	
				if( i == 0 ) {
					date1 = date;
				}
				if( i == n_precip_val - 1 ) {
					date2 = date;
				}
	
				PrintDebug( 20, routine,
				"Date: %s  Precip: %f", temp, 
				_precip_ctl.getDataValue( date, 1 ) );
			}
			else {
				// If we get here then there is no
				// distrubution for this time step.
				date = TSUtil::getTSDateFromString(
					list[0] );
				value = atof( list[1] ) * factor;	
				_precip_ctl.setDataValue( date, value,
					NULL, 24/_t_mult );
				if( i == 0 ) {
					date1 = date;
				}
				if( i == n_precip_val - 1 ) {
					date2 = date;
				}
			}
	
			PrintDebug( 20, routine,"Date: %s "
				"Precip Value: %4.2f",
				list[0], _precip_ctl.getDataValue(
				date, 1 ) );
	
			list = FreeStringList( list );
			continue;
		}
		// Set the date1 and date2 on the DistributedTS we just 
		// constructed...
		_precip_ctl.setDate1( date1 );
		_precip_ctl.setDate2( date2 );

		// Free the memory
		p_val_list = FreeStringList( p_val_list );
	}
*/
	// Now do the Evap TS...
	if( evapTbl_found ) {
		_n_evap = n_evap_val;
		for( i = 0; i < n_evap_val; i++ ) {
			if( strlen( e_val_list[i] ) == 0 || 
				e_val_list[i][0] == '#' ){
				continue;
			}
			list = BreakStringList( e_val_list[i], " \n\t", 
				DELIM_SKIP_BLANKS, &nlist );
			if( list == NULL || nlist == 0 ) {
				if( list ) {
					list = FreeStringList( list );
				}
				continue;
			}
	
			if( nlist > 2 ) {
				dist_tally = 0.0;
	
				// If the user did not specify a dist-
				// ribution value for every timestep,
				// then exit.
				if( nlist != 2 + ( 24 / _t_mult ) ) {
					totErrs++;
					PrintError( routine, "%s %s "
						"RainEvap must have one and "
						"only one distribution value "
						"for each timestep.", 
						_owner->getID(), _id );
					list = FreeStringList( list );
					continue;
				}
	
				// Construct a temporary array to hold
				// distribution values in...
				dist = new float[nlist - 2];
	
				// Read the distribution values
				// into dist
				d_count = 0;
				for( k = 2; k<nlist; k++, d_count++ ){
					dist[d_count] = atof( list[k] );
					dist_tally += atof( list[k] );
				}
	
				// Check to make sure that the number of distribution
				// values equals 24/time step
				if( d_count != 24/_t_mult ) {
					totErrs++;
					PrintError( routine, "Number of "
						"distribution values must "
						"equal number of time steps "
						"per day. Problem on %s %s "
						"RainEvap.", _owner->getID(), 
						_id );
					list = FreeStringList( list );
					continue;
				}
	
				// The distribution of data for a given
				// time step must sum to 1.0
				if( dist_tally > 1.00001
					|| dist_tally < 0.99999 ) {
					totErrs++;
					PrintError( routine, "Distribution on "
						"%s %s RainEvap does not sum "
						"to 1.0.", _owner->getID(), 
						_id );
					list = FreeStringList( list );
					continue;
				}	
				// Construct the TSDate...
				sprintf( temp, "%s",list[0] );
				date = TSUtil::getTSDateFromString( temp);
				value = atof( list[1] ) * factor;
	
				_evap_ctl.setDataValue( date, value,
					dist, d_count );
	
				if( i == 0 ) {
					date1 = date;
				}
				if( i == _n_evap - 1 ) {
					date2 = date;
				}
			}
			else {
				date = TSUtil::getTSDateFromString(
					list[0] );
				value = atof( list[1] ) * factor;	
				_evap_ctl.setDataValue( date, value,
					NULL, 24/_t_mult );
				if( i == 0 ) {
					date1 = date;
				}
				if( i == _n_evap - 1 ) {
					date2 = date;
				}
			}
	
			PrintDebug( 20, routine,"Date: %s "
				"Evap Value: %4.2f",
				list[0], _evap_ctl.getDataValue(
				date, 1 ) );
			list = FreeStringList( list );
			continue;
		}
		// Set the date1 and date2 on the DistributedTS we just 
		// constructed...
		_evap_ctl.setDate1( date1 );
		_evap_ctl.setDate2( date2 );

		// Freeing memory
		e_val_list = FreeStringList( e_val_list );
	}

	// Delete the dist array...
	if( dist ) {
		delete [] dist;
	}

	// Now check to ensure we have something worthwhile from this method.
	//
	// If we have an observed timeseries, we should have a table to fall
	// back on if there are missing values in the timeseries.
/*
	if( _precip_obs ) {
		// If we don't have a precip table, print a warning 
		if( _n_precip == 0 ) {
			PrintWarning( 1, routine, "%s %s on %s has no "
				"companion table for use when observed precip "
				"timeseries value is MISSING.  Precip of 0.0 "
				"will be used as needed.", _type, _id, 
				_owner->_id );
		}
	}
*/	
	if( _evap_obs ) {
		// If we don't have a evap table, print a warning 
		if( _n_precip == 0 ) {
			PrintWarning( 1, routine, "%s %s on %s has no "
				"companion table for use when observed evap "
				"timeseries value is MISSING.  Precip of 0.0 "
				"will be used as needed.", _type, _id, 
				_owner->_id );
		}
	}

	// We must have at least one source of precip or evap.
	// We will ask in a way that is easier to understand.
	if( _precip_obs || _evap_obs || _n_precip > 0 || _n_evap > 0 ) {
		// We have one or more defined
	}
	else {
		totErrs++;
		PrintError( routine, "%s %s on %s has no source of precip or "
			"evap.", _type, _id, _owner->_id );
	}

	// Now check for errors
	if( totErrs ) {
		return( STATUS_FAILURE );
	}
	

	// Constructed successfully...
	_is_constructed = 1;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/RainEvap_construct.cxx,v $";
 static char rcs_id2[] = "$Id: RainEvap_construct.cxx,v 1.7 2006/10/26 15:29:21 hsu Exp $";}
/*  ===================================================  */

}		
