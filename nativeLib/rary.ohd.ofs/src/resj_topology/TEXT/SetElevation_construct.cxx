//------------------------------------------------------------------------------
// SetElevation::construct - Reads a stringlist and constructs precip and/or
//				evap timeseries data.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 14 Apr 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 24 Sep 1998  DKW, RTi	Added _pool_obs from the Reservoir level. 
// 18 May 2001	James R. VanShaar, RTi	Improved error / warning handling
// 24 Nov 2001	JRV, RTi	Allowed for carry over blendtbl (& blendts) step 
//				values
// 27 Nov 2001	JRV, RTi	Revised usage of BLENDTBL and BLENDTS paramters to
//				handle using defaults for _tbl_step and _ts_step
//				if they are not explicity defined.
// 01 Aug 2002  KSH, HRL        Added BLENDTBL keyword to indicate table data
//                              blending. BLEND is retained for old datasets.
// 2003-11-21 Luiz Teixeira, RTi - Added list = FreeStringList( list ) at the 
//  				end of the main for loop in the construct method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "SetElevation.h"
#include "TSUtil.h"

int SetElevation::construct ( char** re_list, int n_items )  
{
	char routine[]="SetElevation::construct", **value_list = NULL,
		**list = NULL, temp[MAXC], ts_id[MAXC] = "";
	int	i, j, n_value = 0, nlist = 0, elev_found = 0, totErrs = 0,
		WarnedOnce = 0;


	for( i = 0; i < n_items; i++ ) {
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( re_list[i], " \n\t",
			DELIM_SKIP_BLANKS, &nlist );
		if( list == NULL || nlist == 0 ) { 
			totErrs++;
			PrintError( routine, "Troubles getting elevation data "
				"for SetElevation %s %s.", _owner->getID(), 
				_id );
			list = FreeStringList( list );
			continue;
			}

		// Check for input time series - the only applicable input for
		//  this method would be an observed withdraw time series.
		if( !strcasecmp( list[0], "TSINPUT" ) ) {
			if ( nlist < 3 ) {
				totErrs++;
				PrintError( routine, "Keyword OBSERVEDPOOL "
					"and timeseries identifier required "
					"immediately after SETELEVATION method "
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
			if( !strcasecmp( list[1], "OBSERVEDPOOL" ) ) {
				_pool_obs = (HourTS*)TSList::getTSFromList( 
					ts_id );
				if( _pool_obs == NULL ) {
					totErrs++;
					PrintError( routine, "Troubles setting "
						"pool timeseries \"%s\" on "
						"%s.", re_list[i], _id );
					list = FreeStringList( list );
					continue;
				}
			}
			else {
				totErrs++;
				PrintError( routine, "Keyword OBSERVEDPOOL "
					"and timeseries identifier required "
					"immediately after SETELEVATION "
					"method keyword %s.", list[0] );
				list = FreeStringList( list );
				continue;
			}
			list = FreeStringList( list );
			continue;
		}

		else if( !strcasecmp( list[0], "VALUES" ) ) {
			value_list = GetSubStringList( re_list, n_items, 
				"VALUES", &n_value, RETURN_NO_KEYWORDS );
			if( value_list == NULL || n_value == 0 ) {
				totErrs++;
				PrintError( routine,
					"Troubles forming sub_list beginning "
					"with %s for SETELEVATION method "
					"\"%s\".  Does \"END%s\" exist?", 
					list[0], getID(), list[0] );
				list = FreeStringList( list );
				if ( value_list ) {
					value_list = 
						FreeStringList( value_list );
				}
				i += n_value + 1; 
				continue;
			}

			if( buildElevationTS( value_list, n_value ) ){
				totErrs++;
				PrintError( routine, "Troubles converting "
					"SETELEVATION table to time series "
					"for %s.", _owner->_id );
				list = FreeStringList( list );
				value_list = FreeStringList( value_list );
				i += n_value + 1; 
				continue;
			}
			elev_found = 1;
			value_list = FreeStringList( value_list );
			list = FreeStringList( list );
			i += n_value + 1; 
			continue;
		}

		else if( !strcasecmp( list[0], "INTERPOLATE" ) ) { 
			_mode = INTERPOLATE; 
			list = FreeStringList( list ); 
			continue; 
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

		else if ( !WarnedOnce ) {
				WarnedOnce++;
				PrintWarning( 1, routine, "'%s' is an "
					"unrecognized (or misplaced) keyword.  "
					"If there are other errors or warnings "
					"for SetElevation %s, this may "
					"disappear following their correction.",
					list[0], _id );
		}
		
		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}
		
	}
	
	if( elev_found == 0 ) {
		totErrs++;
		PrintError( routine, "SetElevation VALUES table for %s on %s "
			"not successfully read.", _id, _owner->_id );
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

int SetElevation::buildElevationTS( char** elev, int nelev )
{
	char routine[]="SetElevation::buildElevationTS", 
		**list = NULL, temp[MAXC];
	TSDate  date, date1, date2;
	double	value;
	int	i, k, nlist = 0;
	double factor = 1.0;

	if( Method::getUnitType() == ENGLISH ) {
		factor = 0.3048;
	}

	// This is a list of the time series data. Loop through and build
	// it up.
	for( i = 0; i < nelev; i++ ) {
		list = BreakStringList( elev[i], " \n\t",
			DELIM_SKIP_BLANKS, &nlist );
		if( list == NULL || nlist == 0 ) {
			PrintWarning( 1, routine, "Troubles getting elevation"
				" time series for SetElevation %s on %s.", _id,
				_owner->_id );
			return( STATUS_FAILURE );
		}

		if ( nlist < 2 ) {
			PrintError( routine, "incorrect number of "
				"values on SETELEVATION method \"%s\" line "
				"\"%s\".", getID(), elev[i] );
			list = FreeStringList( list );
			return(STATUS_FAILURE);
		}

		date = TSUtil::getTSDateFromString( list[0] );
		if( !IsDouble( list[1] ) ) {
			PrintError( routine, "Elevation value '%s' not in a "
				"recognizable format for SetElevation %s.", 
				list[1], _id );
			list = FreeStringList( list );
			continue;
		}
		value = atof( list[1] ) * factor;
		_elev_ctl.setDataValue( date, value, NULL, 1 );

		PrintDebug( 20, routine,"Date: %s Precip Value: %4.2f", list[0],
			 _elev_ctl.getDataValue( date, NORMAL ) );

		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}

		if( i == 0 ) {
			date1 = date;
		}
		if( i == nelev - 1 ) {
			date2 = date;
		}
		continue;
	}

	// Last thing we want to do is set the date1 and date2 on the
	// DistributedTS we just constructed...
	_elev_ctl.setDate1( date1 );
	_elev_ctl.setDate2( date2 );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetElevation_construct.cxx,v $";
 static char rcs_id2[] = "$Id: SetElevation_construct.cxx,v 1.8 2006/10/26 15:32:57 hsu Exp $";}
/*  ===================================================  */

}
