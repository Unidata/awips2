//------------------------------------------------------------------------------
// MaxStage::construct - Reads a stringlist and constructs precip and/or
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
// 21 May 2001	James R. VanShaar, RTi	Improved error / warning handling
// 14 Aug 2007  Darrin Sharp, RTi	Added MAXIMUMDISCHARGE capability for 
// 					Reservoir Tools Enhancement
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "MaxStage.h"

int MaxStage :: construct ( char** re_list, int n_items )  
{
	char routine[] = "MaxStage :: construct", **list = NULL, 
		**sub_list = NULL;
	int i, nlist = 0, nsub = 0, table_found = 0, WarnedOnce = 0,
		num_tbl_rows = 0;
	double lfactor = 1.0, ffactor = 1.0;
	int missingVital = 0;

	if( Method::getUnitType() == ENGLISH ) {
		lfactor = 0.3048;
		ffactor = 0.028317;
	}

	for( i = 0; i < n_items; i++ ) {
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}

		list = BreakStringList( re_list[i], " \n\t", DELIM_SKIP_BLANKS,
			&nlist );
		if( list == NULL || nlist == 0) {
			PrintError( routine, "Troubles getting data for "
				"MaxStage %s %s.", _owner->getID(), _id);
			list = FreeStringList( list );
			return( STATUS_FAILURE );
		}

		// Handle rating curve TABLE
		if( !strcasecmp( list[0], "TABLE" ) ) {
			// Are there enough parts to list?
			if ( nlist < 2 ) {
                                PrintError( routine, "Keyword RATING_CURVE "
                                        "required immediately after MAXSTAGE "
					"method keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			// Check for keyword RATING_CURVE
			if( strcasecmp( list[1], "RATING_CURVE" ) ) {
                                PrintError( routine, "Keyword RATING_CURVE "
                                        "required immediately after MAXSTAGE "
					"method keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}

			// Break out TABLE information
			sub_list = GetSubStringList( &re_list[i], n_items-i,
				"TABLE", &nsub, RETURN_KEYWORDS );
			if( sub_list == NULL || nsub < 3 ) {
				PrintError( routine, "Problems creating "
					"sub-list beginning with line \"%s\".  "
					"Perhaps no values or no 'END%s'?", 
					re_list[i], "TABLE" );
				list = FreeStringList( list );
				if ( sub_list ) {
					sub_list = FreeStringList( sub_list );
				}
				return( STATUS_FAILURE );
			}
			// Populate Table
			if( _stage_flow_tbl.populate( &sub_list[1],
				nsub - 2, lfactor, ffactor ) ) {
				PrintError( routine, "Troubles "
					"filling rating curve for %s on %s.",
					_id, _owner->_id );
				list = FreeStringList( list );
				sub_list = FreeStringList( sub_list );
				return( STATUS_FAILURE );
			}
			table_found = 1;
			list = FreeStringList( list );
			sub_list = FreeStringList( sub_list );
			i += nsub - 1;
			continue;
		}
	
		// Handle keyword MAXIMUMSTAGE
		else if( !strcasecmp( list[0], "MAXIMUMSTAGE" ) ) {
			if( nlist < 2 ) {
				PrintError( routine, "Value required "
					"immediately after MAXSTAGE method "
					"keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			_max_stage = atof( list[1] ) * lfactor;
		}
		// Handle keyword MAXIMUMDISCHARGE
		else if( !strcasecmp( list[0], "MAXIMUMDISCHARGE" ) ) {
			if( nlist < 2 ) {
				PrintError( routine, "Value required "
					"immediately after MAXSTAGE method "
					"keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			_max_discharge = atof( list[1] ) * ffactor;
		}
		else if( !strcasecmp( list[0], "MINRELEASE" ) ) {
			if( nlist < 2 ) {
				PrintError( routine, "Value required "
					"immediately after MAXSTAGE method "
					"keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			_min_release = atof( list[1] ) * ffactor;
		}
		else if( !strcasecmp( list[0], "CRITERION" ) ) {
			if( nlist < 2 ) {
				PrintError( routine, "Value required "
					"immediately after MAXSTAGE method "
					"keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			_criterion = atof( list[1] );
		}
		else if( !strcasecmp( list[0], "MAXITERATIONS" ) ) {
			if( nlist < 2 ) {
				PrintError( routine, "Value required "
					"immediately after MAXSTAGE method "
					"keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			_max_iter = atoi( list[1] );
		}
		else if( !strcasecmp( list[0], "DSCONTROL" ) ) {
			if( nlist < 2 ) {
				PrintError( routine, "Node identifier required "
					"immediately after MAXSTAGE method "
					"keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			Component* temp = NULL;
			temp = _owner->findRoot() -> getComponentPtr( list[1] );
			if( temp == NULL ) {
				PrintError( routine, "Troubles getting "
					"downstream control NODE %s for %s "
					"on %s.", list[1], _id, _owner->_id );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			if( strcasecmp( temp->getType(), "NODE" ) ) {
				PrintError( routine, "NODE identifier required "
					"immediately after MAXSTAGE method "
					"keyword %s.", list[0] );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			_dcp = (Node*)temp;
		}
		else if ( !WarnedOnce ) {
				WarnedOnce++;
				PrintWarning( 1, routine, "'%s' is an "
					"unrecognized (or misplaced) keyword.  "
					"If there are other errors or warnings "
					"for MaxStage %s, this may disappear "
					"following their correction.", list[0],
					_id );
		}
		
		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );
		}
		
		continue;
	}

	// Check to see that we got the vitals
	if( _min_release == MISSING ) {
		missingVital++;
		PrintError( routine, "Must specify a minimum release value"
			" for %s on %s.", _id, _owner->_id );
	}
	if( _criterion == MISSING ) {
		missingVital++;
//		_criterion = 1.0;
		PrintError( routine, "Must specify a convergence criterion" 
			" for %s on %s.", _id, _owner->_id );
	}
	if( !_dcp ) {
		missingVital++;
		PrintError( routine, "Must specify a downstream control point "
			"for %s on %s.", _id, _owner->_id );
	}
	if( (_max_stage == MISSING) && (_max_discharge == MISSING) ) {
		missingVital++;
		PrintError( routine, "Neither MAXIMUMSTAGE nor "
			"MAXIMUMDISCHARGE is "
			"specified for %s on %s.", _id, _owner->_id );
	}

	if( missingVital ) {
		return( STATUS_FAILURE );
	}

	// Check to see if both MAXIMUMSTAGE and MAXIMUMDISCHARGE are set - 
	// this is invalid
	if( (_max_stage != MISSING) && (_max_discharge != MISSING) ) {
		PrintError( routine, "Both MAXIMUMSTAGE and "
			"MAXIMUMDISCHARGE are specified for "
			"%s on %s.", _id, _owner->_id );
		return ( STATUS_FAILURE );
	}

	// if MAXIMUMDISCHARGE is not defined, there _must_ be a Rating Table or
	// Rating Curve ID defined somewhere (either in the method or at the
	// node); if MAXIMUMDISCHARGE is defined, no table or curve is required (but
	// it won't hurt if one is present, the table/curve will be ignored)
	if (_max_discharge == MISSING) {
		// If no TABLE is present in this method, then a TABLE or RATINGCURVEID
		// must be defined at the downstream control point (NODE).
		if( !table_found ) {
			if ( (!strcmp(_dcp->_rating_curve_id, "NULL")) &&
			     (_dcp->_rating_table.getNRows() == 0) ) {
				// at this point, there is no table in the method, 
				// no table at the NODE, and no RCID at the NODE either
				PrintError( routine, "Stage constraint used but no"
				" rating curve ID or table found at node or defined"
				" in method for %s on %s.", _id, _owner->_id );
				missingVital++;
				PrintError( routine, "Must specify a rating curve"
					" for %s on %s.", _id, _owner->_id );
			}
			else if ( (_dcp->_rating_table.getNRows() > 0) ) {
				// No TABLE at the method, but one is defined at the node;
				// populate the stage_flow_table from the node data
				_stage_flow_tbl.operator=( _dcp->_rating_table );
				// note that if both a table and a RCID are defined at the 
				// NODE, there will have been an earlier error and execution
				// will never reach this point
			}
			// else there is a RCID defined at the node; the process db
			// will be queried for stage/discharge relationships
		}
		else {
			// TABLE is present in the method; make sure that a TABLE or 
			// RATINGCURVEID is not defined at the NODE
			if ( (strcmp(_dcp->_rating_curve_id,"NULL")) ||
		             (_dcp->_rating_table.getNRows() > 0) ) {
				PrintError( routine, "MAXSTAGE defines a Rating"
				" Table, and a Rating Table or Curve ID is specified"
				" at the Node as well. NODE and MAXSTAGE are"
				" for %s on %s.", _id, _owner->_id );
				return( STATUS_FAILURE );
			}
		}
	}

	_is_constructed = 1;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxStage_construct.cxx,v $";
 static char rcs_id2[] = "$Id: MaxStage_construct.cxx,v 1.7 2006/10/26 15:26:51 hsu Exp $";}
/*  ===================================================  */

}		


