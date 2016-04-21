//------------------------------------------------------------------------------
// Node Set/Get routines.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 19 Oct 1998  Daniel Weiler, RTI	Added all of the setStates functionality 
// 15 May 2001	James R. VanShaar, RTi	Improved error / warning handling in 
//					setStates
// 15 May 2001	JRV, RTi	Adjusted setStates to allow input of non-
//				topological inflow timeseries--enabling lateral
//				inflows.
// 10 Jul 2001	JRV, RTi	Corrected issues from misnamed time series and
//				use of full identifiers
// 13 Nov 2001	JRV, RTi	Added setCOstring
// 20 Nov 2001	JRV, RTi	Added DISCHARGE and PREVIOUSDISCHARGE to 
//				setStates
// 27 Nov 2001	JRV, RTi	Revised usage of  DISCHARGE and 
//				PREVIOUSDISCHARGE to handle using defaults if 
//				they are not explicity defined.
// 10 Jun 2002	JRV, RTi	Created initial version of setEndInflow and
//				setEndOfTimestepStates.
//				Also, added handling of inflow and discharge
//				rule states in setStates, and 
//				getInternalValuePtr.
// 19 Feb 2004	JRV, RTi	Modified setEndInflow and added an overloaded
// 				version of it.
// 12 Mar 2004	JRV, RTi	Added TSOutput of Inflow timeseries.
// 16 Feb 2006	JRV, RTi    Significantly revised setStates and setCOstring
//                          methods consistent with major enhancement of
//                          diversion and minimum remainder discharge
//                          functionality.
//			    Cleaned up some commented test output.
//			    Deleted unused stage work.
//			    Added valueToMethod method for diversion ToComp
//                          work.
// 10 Mar 2006  JRV, RTi    Added buildMinimumTS.
//                          Revised setCOstate( TSDate & date ) and
//                          setCOstate().
// 17 Aug 2007	DJS, RTi    For Res. Tools Enhancement, add capability to define
//			    a rating table or rating curve id at the node.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Node.h"
#include "ResJSys.h"
#include "TSUtil.h"

//------------------------------------------------------------------------------
// Node :: buildMinimumTS - Creates (puts data into) the minimum remainder time
//                          seris
//------------------------------------------------------------------------------
// This routine, called by the Node::setStates, builds the minimum remainder
// time series.  The return is whether the method was successful or not.
//------------------------------------------------------------------------------
// Return: 
//     Integer defining successful construction of the minimum remainder TS.
// Calls:
//     Method::getUnitType()
//     BreakStringList
//     PrintError
//     TSUtil::getTSDateFromString( char* )
//     IsDouble( char * )
//     atof( * )
//     DistributedTS::setDataValue( date, value, NULL, 1 );
//     FreeStringList( char** );
//     DistributedTS::setDate1( TSDate );
//     DistributedTS::setDate2( TSDate );
// Errors:
//     Several related to improper syntax in the user input file, part of which
//     enters through the parameters.
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Builds the minimum remainder discharge time series.
@return Integer flag of completion success
@param min Character string array containing related user input
@param nmin The number of strings in min (rows in the user input)
*/

int Node::buildMinimumTS( char** min, int nmin )
{
    char routine[]="Node::buildMinimumTS", **list = NULL, temp[MAXC];
    TSDate date, date1, date2, prevDate;
    double value;
    int    i, k, totErrs = 0, nlist = 0;
    double factor = 1.0;

    if( Method::getUnitType() == ENGLISH )
    {
        factor = 0.3048;
    }

    // This is a list of the time series data. Loop through and build it up.
    for( i = 0; i < nmin; i++ )
    {
        list = BreakStringList( min[i], " \n\t", DELIM_SKIP_BLANKS, &nlist );
        if( list == NULL || nlist == 0 )
        {
            totErrs++;
            PrintError( routine, "Troubles getting minimum flow time series "
                "for %s  %s.", getType(),_id );
        }

        if ( nlist != 2 )
        {
            totErrs++;
            PrintError( routine, "Two fields expected in MINDISCHARGE section "
                "of %s %s. (Line: %s).", getType(), getID(), min[i] );
        }
        else
        {

            // Handle first field--a date.
            date = TSUtil::getTSDateFromString( list[0] );
            // Check for chronological input
            if( i > 0 )
            {
                if( date <= prevDate )
                {
                    totErrs++;
                    PrintError( routine, "MinDischarge dates must be "
                         "chrological. (%s %s).", getType(), _id );
                }
            }
            prevDate = date;

            if( !IsDouble( list[1] ) )
            {
                totErrs++;
                PrintError( routine, "MinDischarge value '%s' not in a "
                    "recognizable format. (%s %s).", list[1], getType(), _id );
                // We continue as if it were a valid number, but we will return
                // a failure status code.
            }

            value = atof( list[1] ) * factor;
            if( value < 0 )
            {
                // We cannot have negative values
                totErrs++;
                PrintError( routine, "MinDischarge value '%s' cannot be "
                    "negative. (%s %s).", list[1], getType(), _id );
            }

            _min_ctl.setDataValue( date, value, NULL, 1 );

            // Deal with special considerations for first and last date
            if( i == 0 )
            {
                date1 = date;
            }
            else if( i == nmin - 1 )
            {
                date2 = date;
            }
    
            // Freeing memory
            if ( list ) {
                list = FreeStringList( list );
            }
    
            continue;
        }
    }

    // Last thing we want to do is set the date1 and date2 on the
    // DistributedTS we just constructed...
    _min_ctl.setDate1( date1 );
    _min_ctl.setDate2( date2 );

    return( totErrs );
}

double* Node :: getInternalValuePtr( char* id )
{
	char	routine[]="Node :: getInternalValuePtr";

	// Basically we just have to loop through the reserved key-words and 
	// check if we have one.

	// It must be ensured that the variable is in capitals
	ToUpper( id );
	
	// The following is ordered as estimated to be most efficient.
	if( NULL != strstr( id, "INFLOW" ) ) {
		if( !strcmp( id, "ENDINGINFLOW" ) ) {
			return( &_endInflow );
		}
		if( !strcmp( id, "STARTINGINFLOW" ) ) {
			return( &_startInflow );
		}
		if( !strcmp( id, "PREVIOUSINFLOW" ) ) {
			return( &_prevInflow );
		}
	}

	if( NULL != strstr( id, "DISCHARGE" ) ) {
		if( !strcmp( id, "STARTINGDISCHARGE" ) ) {
			return( &_startDischarge );
		}
		if( !strcmp( id, "ENDINGDISCHARGE" ) ) {
			return( &_endDischarge );
		}
		if( !strcmp( id, "PREVIOUSDISCHARGE" ) ) {
			return( &_prevDischarge );
		}
	}

	if( NULL != strstr( id, "DIVERSION" ) ) {
		if( !strcmp( id, "STARTINGDIVERSION" ) ) {
			return( &_startDiversion );
		}
		if( !strcmp( id, "ENDINGDIVERSION" ) ) {
			return( &_endDiversion );
		}
		if( !strcmp( id, "PREVIOUSDIVERSION" ) ) {
			return( &_prevDiversion );
		}
	}

	return( NULL ); 
}

int Node :: setCOstring( )
{
    char routine[]="Node::setCOstring";
    char discharge_str[17],
        prevDischarge_str[17],
        inflow_str[17], prevInflow_str[17],
        diversion_str[17], prevDiversion_str[17],
        future_str[41], 
        value_str[2*8+1];
    char future[9]="*FUTURE*";
    char temp_str[28+16+40+1];    // Labels, values, future space, end

    sprintf( discharge_str, "%f", _startDischarge );
    discharge_str[8] = '\0';
    sprintf( prevDischarge_str, "%f", _prevDischarge );
    prevDischarge_str[8] = '\0';

    sprintf( inflow_str, "%f", _startInflow );
    inflow_str[8] = '\0';
    sprintf( prevInflow_str, "%f", _prevInflow );
    prevInflow_str[8] = '\0';

    // Handle diversion.
    // NOTE that it is possible to have a valid value beginning with -999.
    // This will result in problems in constructing the carryover array.
    // Make special provision for that case.
    sprintf( diversion_str, "%f", _startDiversion );
    diversion_str[8] = '\0';
    if( !strncmp( diversion_str, "-999", 4 ) )
    {
        // Replace with a keyword.
        strncpy( diversion_str, "MISS", 4 );
    }
    sprintf( prevDiversion_str, "%f", _prevDiversion );
    prevDiversion_str[8] = '\0';
    if( !strncmp( prevDiversion_str, "-999", 4 ) )
    {
        // Replace with a keyword.
        strncpy( prevDiversion_str, "MISS", 4 );
    }

    // Prepare value portion of the string
    // Note: Late-coming diversion values will be in the future string.
    sprintf(value_str, "%8.8s%8.8s", discharge_str, prevDischarge_str);

    // Prepare future place holders
    // Note: Late-coming inflow and diversion values will be in the future
    //       string.
    sprintf(future_str, "%8.8s%8.8s%8.8s%8.8s%8.8s", inflow_str,
        prevInflow_str, diversion_str, prevDiversion_str, future);
    // ***** NOTE *****
    // The last future place is reserved for a trigger to signal a revised
    // carryover array structure.  If additional revision is required at
    // some time future, this should be replace with a trigger and the
    // additional carryover will be added up.
    // Therefore, when the carryover is handled, the trigger will be
    // referenced.  If it is still "*FUTURE*" we deal with old carryover.
    // Otherwise, we proceed to look for additional information added
    // beyond this last position.

    // Prepare the full variable string
    sprintf( temp_str, "%-12.12s%-12.12s%4.4s%16.16s%40.40s", _type, _id,
        "-999", value_str, future_str );

    // Write the string to the system CO string
    if( ResJSys::addCOString( temp_str ) )
    {
        PrintWarning( 1, routine, "Troubles adding carryover data to "
            "CO array." );
        return( STATUS_FAILURE );
    }

    return(STATUS_SUCCESS);
}

int Node :: setCOstring( TSDate & date )
{
    // NOTE: This function should be called after the setEndOfTimestepStates
    //  function to ensure that the correct state variables are assigned
    //  to the carryover string.

    // CO is date independent.  Therefore simply call the non-dated 
    //    setCOstring.
    int success;
    success = setCOstring( );

    return( success );
}

int Node :: setStates( char** states, int n_items )
{
	char routine[]="Node :: setStates", **list = NULL, ts_id[MAXC]="",
        	**sub_list = NULL;
	int i, j, idlen, nlist = 0, n_out = 0, totErrs = 0, nsub = 0;
 	int rating_curve_id_found = 0, table_found = 0;
	double lfactor = 1.0, vfactor = 1.0, ffactor = 1.0;
        if( Method::getUnitType() == ENGLISH ) {
                lfactor = 0.3048;
                vfactor = 1233.5;
                ffactor = 0.028317;
        }

	for( i = 0; i < n_items; i++ ) {
                if( strlen( states[i] ) == 0 || states[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( states[i], " \n\t",
			DELIM_SKIP_BLANKS, &nlist );

                if( list == NULL ) {
			totErrs++;
			PrintError( routine, "Troubles getting %s state "
				"data values for %s (Line: \'%s\').", _type, 
				_id);
			if( list ) {
				list = FreeStringList( list );
			}
			continue;
		}

                // Handle rating curve TABLE
                if( !strcasecmp( list[0], "TABLE" ) ) {
                        // Are there enough parts to list?
                        table_found = 1;
                        if ( nlist < 2 ) {
				totErrs++;
                                PrintError( routine, 
					"Keyword RATING_CURVE required "
                                        "immediately after MAXSTAGE "
                                        "method keyword %s (Node %s).", 
					list[0], _id );
                                list = FreeStringList( list );
                                continue;
                        }
                        // Check for keyword RATING_CURVE
                        if( strcasecmp( list[1], "RATING_CURVE" ) ) {
				totErrs++;
                                PrintError( routine, "Keyword " 
					"RATING_CURVE required "
					"immediately after MAXSTAGE "
                                        "method keyword %s (Node %s).", 
					list[0], _id );
                                list = FreeStringList( list );
                                continue;
                        }

                        // Break out TABLE information
                        sub_list = GetSubStringList( &states[i], 
				n_items-i, "TABLE", &nsub, 
				RETURN_KEYWORDS );
                        if( sub_list == NULL || nsub < 3 ) {
				totErrs++;
                                PrintError( routine, "Problems creating "
                                        "sub-list beginning with "
					"line \"%s\".  Perhaps "
                                        "no values or no 'END%s'? (Node %s).",
                                        states[i], "TABLE", _id );
                                list = FreeStringList( list );
                                if ( sub_list ) {
                                        sub_list = FreeStringList(sub_list);
                                }
                                continue;
                        }

                        // Populate Table
                        if( _rating_table.populate( &sub_list[1],
                                nsub - 2, lfactor, ffactor ) ) {
				totErrs++;
                                PrintError( routine, "Troubles "
                                        "filling rating curve for Node %s.",
                                        _id );
                                list = FreeStringList( list );
                                sub_list = FreeStringList( sub_list );
                                continue;
                        }
                        if ( rating_curve_id_found == 1) {
				totErrs++;
				PrintError( routine, 
					"Rating TABLE found, as well"
					" as a RATINGCURVEID; both cannot"
					" be defined at the same node." );
                        	list = FreeStringList( list );
                        	sub_list = FreeStringList( sub_list );
                                continue;
			}
			_has_rating_table = TRUE;
                        list = FreeStringList( list );
                        sub_list = FreeStringList( sub_list );
                        i += nsub - 1;
                        continue;
                }

                // Handle keyword RATINGCURVEID
		if( !strcasecmp( list[0], "RATINGCURVEID" ) ) {
                        rating_curve_id_found = 1;
			if ( nlist < 2 ) {
				totErrs++;
				PrintError( routine, 
					"Rating Curve identifier"
					"required immediately "
					"after NODE keyword %s. (Node %s).", 
					list[0], _id );
				list = FreeStringList( list );
                                continue;
			}
                        // the fortran code that is eventually called to load
			// the rating curve can only handle an id 8 chars
			// or less in length
			idlen = strlen(list[1]);
                        if ( idlen > 8 ) {
				totErrs++;
                                PrintError(routine, "RATINGCURVEID string is longer "
                                        "than 8 characters.");  
                                continue;
                        }
			else {
				// copy into an array so we can pass it to the fortran code later
				strcpy(_rating_curve_id,list[1]);
			}

                        if ( table_found == 1) {
				totErrs++;
				PrintError( routine, 
					"RATINGCURVEID found, as well"
					" as a Rating TABLE; both cannot"
					" be defined at the same node." );
				list = FreeStringList( list );
                                continue;
			}
			list = FreeStringList( list );
			continue;
 		}

		// Handle keyword TSINPUT
		if( !strcasecmp( list[0], "TSINPUT" ) ) {
			if ( nlist < 3 ) {
				totErrs++;
				PrintError( routine, 
					"Keyword INFLOW and timeseries "
					"identifier required immediately "
					"after reach keyword %s.", 
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

			// Keyword must be "INFLOW"
			if( strcasecmp( list[1], "INFLOW" ) ) {
				totErrs++;
				PrintError( routine,
					"Keyword INFLOW required "
					"immediately after reach keyword %s. "
					"%s is not valid.", list[0], list[1] );
				list = FreeStringList( list );
				continue;
			}

			// Check for TSINPUT INFLOW in addition to 
			// inflow from an upstream component
			if( _n_son != 0 ) {
				PrintWarning( 1, routine, 
					"Applying 'Lateral-type' flow \"%s\" "
					"to Node \"%s\". This is in addition "
					"to the inflow(s) from component(s) "
					"topologically upstream. ", list[2],
					getID() );
			}

			// Check for non-primary inflow at upstream-most
			// component
			if( _n_son == 0 && _in_count > 0 ) {
				PrintWarning( 1, routine, 
					"Applying non-primary flow \"%s\" at "
					"Node \"%s\".  At least one other flow "
					"has been defined as inflow to the "
					"node.", list[2], getID() );
			}

			if( setInflowTS( (HourTS*)TSList::
				getTSFromList( ts_id ) ) ) { 
				totErrs++;
				PrintError( routine, "Troubles setting inflow "
					"timeseries \"%s\" on %s.", states[i], 
					_id );
				list = FreeStringList( list );
				continue;
			}

			// Check to see if it is an instantaneous data 
			// type.	
			if( !_inflow_ts[_in_count-1]->
				getInstantaneous() ) {
				PrintWarning( 2, routine, 
					"Inflow time series %s for %s is not "
					"set as instantaneous.", ts_id, _id );
			}
		list = FreeStringList( list );
		continue;
		}

		// Handle keyword TSOUTPUT
		else if( !strcasecmp( list[0], "TSOUTPUT" ) ) {
			 if ( nlist < 3 ) {
                                totErrs++;
                                PrintError( routine,
                                        "Keyword (OUTFLOW, INFLOW or "
					"DIVERSION) and timeseries identifier "
					"required immediately after Node "
					"keyword %s.", list[0] );
                                list = FreeStringList( list );
                                continue;
                        }

                        // Read either full identifiers or alias
			strcpy( ts_id, list[2] );
			for( j = 3; j < nlist; j++ ) {
				strcat( ts_id, " " );
				strcat( ts_id, list[j] );
			}
			if( !strcasecmp( list[1], "OUTFLOW" ) ) {
				_output_ts[n_out] = &_outflow_ts; 
				if( _output_ts[n_out] == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
					"%s as a valid Node %s "
					"timeseries.", list[0], ts_id );
					list = FreeStringList( list );
					continue;
				}
			}
			else if( !strcasecmp( list[1], "INFLOW" ) ) {
				_output_ts[n_out] = &_totalInflow; 
				if( _output_ts[n_out] == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
					"%s as a valid Node %s "
					"timeseries.", list[0], ts_id );
					list = FreeStringList( list );
					continue;
				}
			}
			else if( !strcasecmp( list[1], "DIVERSION" ) ) {
				_output_ts[n_out] = _diversion_ts; 
				if( _output_ts[n_out] == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
					"%s as a valid Node %s "
					"timeseries.", list[0], ts_id );
					list = FreeStringList( list );
					continue;
				}
			}
			else {
				totErrs++;
				PrintError( routine, 
					"Keyword INFLOW or OUTFLOW required "
                                        "immediately after Node keyword %s. "
                                        "%s is not valid.", list[0], list[1] );
				list = FreeStringList( list );
				continue;
			}
			HourTS *tempTS = (HourTS*)TSList:: getTSFromList(ts_id);
			if ( !tempTS ) {
				totErrs++;
				PrintError( routine, "Unable to find TSOUTPUT "
					"'%s' in time series listing.", ts_id );
				list = FreeStringList( list );
				continue;
			}
			TSIdent temp = tempTS->getIdentifier();
			_output_ts[n_out]->setIdentifier( temp );
			n_out++;
		}
//------------------------------------------------------------------------------
                // Handle discharge states
                else if( NULL != strstr( list[0], "DISCHARGE" ) ) {
                    // Handle keyword PREVIOUSDISCHARGE
                    if( !strcasecmp( list[0], "PREVIOUSDISCHARGE" ) ) {
                        if( nlist < 2 ) {
                            // Not error because we might find 
                            // PREVIOUSDISCHARGE later.  We check near 
                            // bottom of this function.
                            PrintWarning(1, routine, "Value required with "
                                "keyword %s for %s \"%s\". ", list[0], 
                                _type, _id );
                            _prevDischarge = MISSING;
                        }
                        else {
                            _prevDischarge = atof( list[1] ) * ffactor;
                            if( _prevDischarge < 0 )
                            {
                                totErrs++;
                                PrintError( routine, "%s value must be "
                                    "non-negative. (%s %s)", list[0], _type,
                                    _id );
                            }
                        }
                    }

                    // Handle keyword DISCHARGE
                    else if( !strcasecmp( list[0], "DISCHARGE" ) ||
                             !strcasecmp( list[0], "INITIALDISCHARGE" ) ) {
                        if( nlist < 2 ) {
                            // Not error because we might find 
                            // DISCHARGE later.  We check near bottom 
                            // of this function.
                            PrintWarning(1, routine, "Value required with "
                                "keyword %s for %s \"%s\". ", list[0], 
                                _type, _id );
                            _startDischarge = MISSING;
                        }
                        else {
                            _startDischarge = atof( list[1] ) * ffactor;
                            if( _startDischarge < 0 )
                            {
                                totErrs++;
                                PrintError( routine, "%s value must be "
                                    "non-negative. (%s %s)", list[0], _type,
                                    _id );
                            }
                        }
                    }
                    // Handle other
                    else {
                        PrintWarning(1, routine, "Unrecognized keyword \"%s\" "
                            "for Node line \"%s\".", list[0], states[i] );
                        if( !strncasecmp( list[0], "P", 1 ) )
                        {
                            // Assume this was supposed to be PREVIOUS
                            _prevDischarge = MISSING;
                        }
                        else if( !strncasecmp( list[0], "I", 1 ) )
                        {
                            // Assume this was supposed to be INITIAL
                            _startDischarge = MISSING;
                        }
                    }
                }

                // Handle diversion states
                else if( NULL != strstr( list[0], "DIVERSION" ) )
                {
                    // Handle keyword PREVIOUSDIVERSION
                    if( !strcasecmp( list[0], "PREVIOUSDIVERSION" ) )
                    {
                        if( nlist < 2 ) {
                            // Error.  If the keyword is used, a value must
                            // follow.
                            totErrs++;
                            PrintError( routine, "Keyword '%s' must have a "
                                "value. (%s %s)", list[0], _type, _id );
                        }
                        else
                        {
                            _prevDiversion = atof( list[1] ) * ffactor;
                        }
                    }

                    // Handle keyword DIVERSION
                    else if( !strcasecmp( list[0], "DIVERSION" ) ||
                             !strcasecmp( list[0], "INITIALDIVERSION" ) )
                    {
                        if( nlist < 2 )
                        {
                            // Error.  If the keyword is used, a value must
                            // follow.
                            totErrs++;
                            PrintError( routine, "Keyword '%s' must have a "
                                "value. (%s %s)", list[0], _type, _id );
                        }
                        else
                        {
                            _startDiversion = atof( list[1] ) * ffactor;
                        }
                    }
                    // Handle other
                    else {
                        PrintWarning(1, routine, "Unrecognized keyword \"%s\" "
                            "for Node line \"%s\".", list[0], states[i] );
                    }
                }

                // Handle inflow states
                else if( NULL != strstr( list[0], "INFLOW" ) )
                {
                    // Handle keyword PREVIOUSINFLOW
                    if( !strcasecmp( list[0], "PREVIOUSINFLOW" ) )
                    {
                        if( nlist < 2 ) {
                            // Error.  If the keyword is used, a value must
                            // follow.
                            totErrs++;
                            PrintError( routine, "Keyword '%s' must have a "
                                "value. (%s %s)", list[0], _type, _id );
                        }
                        else
                        {
                            _prevInflow = atof( list[1] ) * ffactor;
                            if( _prevInflow < 0 )
                            {
                                totErrs++;
                                PrintError( routine, "%s value must be "
                                    "non-negative. (%s %s)", list[0], _type,
                                    _id );
                            }
                        }
                    }

                    // Handle keyword INITIALINFLOW
                    else if( !strcasecmp( list[0], "INFLOW" ) ||
                             !strcasecmp( list[0], "INITIALINFLOW" ) )
                    {
                        if( nlist < 2 )
                        {
                            // Error.  If the keyword is used, a value must
                            // follow.
                            totErrs++;
                            PrintError( routine, "Keyword '%s' must have a "
                                "value. (%s %s)", list[0], _type, _id );
                        }
                        else
                        {
                            _startInflow = atof( list[1] ) * ffactor;
                            if( _startInflow < 0 )
                            {
                                totErrs++;
                                PrintError( routine, "%s value must be "
                                    "non-negative. (%s %s)", list[0], _type,
                                    _id );
                            }
                        }
                    }
                    // Handle other
                    else {
                        PrintWarning(1, routine, "Unrecognized keyword \"%s\" "
                            "for Node line \"%s\".", list[0], states[i] );
                    }
                }

                // Handle keyword CONSTANT
                // CONSTANTs are handled by parseConstant, so skip over
                else if( !strcasecmp( list[0], "MINREMAINDER" ) )
                {
                    // We need to parse out the parameters to the end of this
                    // section.
                    char **value_list = NULL;
                    int n_value;
                    value_list = GetSubStringList( states, n_items, 
                        "MINREMAINDER", &n_value, RETURN_NO_KEYWORDS );
                    if( value_list == NULL || n_value == 0 )
                    {
                        // We had trouble getting the minremainder block.
                        totErrs++;
                        PrintError( routine, "Troubles handling %s %s.  Does "
                            "\"END%s\" exist?", _id, list[0], list[0] );
                    }
                    else if( buildMinimumTS( value_list, n_value ) ){
                        // We had trouble converting the block to a time series.
                        totErrs++;
                        PrintError( routine, "Troubles building %S time series "
                            "for %s %s.", list[0], getType(), _id );
                    }
                    else
                    {
                        // We were successful.
                        _min = 1;
                    }

		    // Now clean up
		    if ( value_list )
		    {
		        value_list = FreeStringList( value_list );
		    }
		    // Now increment the counter beyond this section.
		    i += n_value + 1; 
		}

		else if( !strcasecmp( list[0], "INTERPOLATE" ) )
		{
		    _mode = INTERPOLATE;
		}

		// Handle keyword CONSTANT
		// CONSTANTs are handled by parseConstant, so skip over
		else if( !strcasecmp( list[0], "CONSTANT" ) ) {
			// Do nothing.
		}

		// Handle other
		else {
			 PrintWarning(1, routine, "Unrecognized keyword \"%s\" "
                                "for Node line \"%s\".", list[0], states[i] );
		}

		list = FreeStringList( list );
		continue;
	}

	// Test for required parameters.  The following default to 0.0 but are
	//	reassigned to MISSING if a syntactical error occured in 
	//	parameterization.
	if( _startDischarge == MISSING ) {
		totErrs++;
		PrintError( routine,
			"Keyword INITIALDISCHARGE and Value required but not "
			"found in %s \"%s\".", _type, _id );
	}
	if( _prevDischarge == MISSING ) {
		totErrs++;
		PrintError( routine,
			"Keyword PREVIOUSDISCHARGE and Value required but not "
			"found in %s \"%s\".", _type, _id );
	}
        // Test for new variables.  They default to MISSING.  If they are not
        // specified we will compute them from continuity.
        // Note that the diversions default to 0.
        if( _startInflow == MISSING ) {
            // We will recompute it according to continuity.
            _startInflow = _startDischarge + _startDiversion;
            if( _startInflow < 0 )
            {
                // This won't propogate downstream, so just provide a warning.
                PrintWarning(1, routine, "Calculated STARTINGINFLOW is "
                    "negative.  Revising to 0. (%s %s)", getType(), _id );
                _startInflow = 0.0;
            }
        }
        if( _prevInflow == MISSING ) {
            // We will recompute it according to continuity.
            _prevInflow = _prevDischarge + _prevDiversion;
            if( _prevInflow < 0 )
            {
                // This won't propogate downstream, so just provide a warning.
                PrintWarning(1, routine, "Calculated PREVIOUSINFLOW is "
                    "negative.  Revising to 0. (%s %s)", getType(), _id );
                _prevInflow = 0.0;
            }
        }


	if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}

/*****************************
// This section is unnecessary since as of 2006-02-16 no stage calculations
// are done.
//	// Initialize the stage_ts time series to be all zeros, but only if the 
//	// Forecast dates have been set...
//	TSDate t;
//	TSDate t1 = Method :: getForecastDate1();
//	TSDate t2 = Method :: getForecastDate2();
//	if( t1 < t2 ) {
//		int base = Method :: getTimeInterval();
//		int mult = Method :: getTimeMult();
//		for( t = t1; t <= t2; t.addInterval( base, mult ) ) {
//			_stage_ts->setDataValue( t, 0.0 );
//		}
//	}
//
*****************************/
	_n_output_ts = n_out;

	setCOstring( );

	return( STATUS_SUCCESS );
}

// Sums inflows at the end of the current timestep and sets the value of 
// _endInflow.
void Node :: setEndInflow( TSDate& cur_date )
{
	_endInflow = sumInflow(cur_date);
	_totalInflow.setDataValue( cur_date, _endInflow );
	return;
}

void Node :: setEndInflow ( TSDate& date, double value )
{
	_endInflow = value;
	_totalInflow.setDataValue( date, value );
	return;
}

// Reset states for next timestep and output carryover, if necessary.
int Node :: setEndOfTimestepStates( TSDate& date )
{
	char	routine[]="Node :: setEndOfTimestepStates";
	int	i=0;

	// We have to recursively call the function on the 
	// sons...
	for( i=0; i<_n_son; i++ ){
		if( _son_list[i]->setEndOfTimestepStates( date ) ){
			return( STATUS_FAILURE );
		}
	}

	// Shift PREVIOUS states
	_prevInflow = _startInflow;
	_prevDischarge = _startDischarge;
	_prevDiversion = _startDiversion;

	// Shift STARTING states
	_startInflow = _endInflow;
	_startDischarge = _endDischarge;
	_startDiversion = _endDiversion;

	// Reinitialize ENDING states
	_endInflow = MISSING;
	_endDischarge = MISSING;
	_endDiversion = MISSING;

	// Check for the need to write carryover
	if( _is_co_date ) {
		// write Component carryover
		setCOstring(date);
		// write method carryover
		for( i=0; i<_n_meth; i++ ) {
			if( _method_list[i]->_hasStates ) {
				_method_list[i]->setCOstring(date);
			}
		}
	}

	// Set the previous date on this component for use at the next time 
	//	step.
	setPrevDate( date );
	
	return( STATUS_SUCCESS );
}

//------------------------------------------------------------------------------
// Node :: valueToMethod - Handle diversion value transfer to another component
//                         and / or reduction due to minimums.
//------------------------------------------------------------------------------
// Depending on the input flag, this routine will simply pass planned diversions
// on to their respective destinations (as defined by diversion methods), or it
// will work to reduce the diversions then send them on.
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//    Method::sendToTS( TSDate );
//    Method::setInactiveState( );
//    getTotalInflow( date );
//    Method::getMyValueAsOut( double *, double * )
//    DistributedTS::getDataValue( TSDate, int )
//    Method::sendToTS( TSDate, int );
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Handle diversion value transfer to another component and / or reduction due to
minimums.
@return none
@param date TSDate of current solution time step.
@param reduceDiv Integer flag specifying whether _diversion needs to be reduced
for compliance with minimum discharges.
*/

void Node :: valueToMethod ( TSDate date, int reduceDiv )
{
    int i, foundActive = 0;
    Method *meth;

    // Check if we need to reduce the planned diversion, and handle the
    // no-reduction case first (since it is simpler).
    if( !reduceDiv )
    {
        // No reduction.  Each method can divert (transfer) as much as they
        // wanted.
        for( i=_n_meth-1; i>=0; i-- )
        {
            meth = _method_list[i];
            // Ensure the method is a "top level" method by checking for
            // an expression.  Also check to make sure we are working with
            // a diversion-type method (currently Lookup3 only).
            if( _expr_list[i] != NULL && meth->_group_id == DIVERS_METHOD )
            {
                // If this method is active, then it was executed.
                if ( _method_list[i]->_Active && !foundActive )
                {
                    // Transfer, if appropriate.
                    _method_list[i]-> sendToTS( date );

                    // We have found the last active method.
                    foundActive = 1;
                }
                else if( foundActive )
                {
                    // We already found the governing (last active) method.
                    // Deactivate this method (and any under it if it is a
                    // ComboMethod).
                    // We do this to avoid bad carryover values for
                    // INITIALTRANSFER on methods that were overwritten by
                    // subsequent methods due to their execution order defined
                    // by the RULES section.
                    _method_list[i]->setInactiveState( );
                }
            }
        }
    }
    else
    {
        // We need to reduce the diversions.
        // To do this we need to define specifically inflows (augmentations)
        // and outflows (diversions).
        double availIN = getTotalInflow( date );
        double planOUT = 0.0;
        double methValIN, methValOUT;
        for( i=_n_meth-1; i>=0; i-- )
        {
            meth = _method_list[i];
            // Ensure the method is a "top level" method by checking for
            // an expression.  Also check to make sure we are working with
            // a diversion-type method (currently Lookup3 only).
            if( _expr_list[i] != NULL && meth->_group_id == DIVERS_METHOD )
            {
                // If this method is active, then it was executed.
                if ( _method_list[i]->_Active && !foundActive )
                {
                    // Request the method's value as a diversion
                    methValIN = 0.0;
                    methValOUT = 0.0;
                    _method_list[i]->getMyValueAsOut( &methValIN, &methValOUT );
                    availIN += methValIN;
                    planOUT += methValOUT;

                    // We have found the last active method.
                    foundActive = 1;
                }
                else if( foundActive )
                {
                    // We already found the governing (last active) method.
                    // Deactivate the method (and any under it if it is a
                    // ComboMethod).
                    // We do this to avoid bad carryover values for
                    // INITIALTRANSFER on methods that were overwritten by
                    // subsequent methods due to their execution order defined
                    // by the RULES section.
                    _method_list[i]->setInactiveState( );
                    // This also eliminates them from further consideration
                    // later in this method.
                }
            }
        }
        // Subtract the minimum remainder from the available inflow and
        // constrain it to be non-negative.
        if( _min )
        {
            availIN -= _min_ctl.getDataValue( date, _mode );
        }
        if( availIN < 0 )
        {
            availIN = 0;
        }

        // Available inflow is now applicable to diversion.
        // We already know there is insufficient to meet diversion demaind,
        // however.
        double scalar;
        if( planOUT > 0 )
        {
            scalar = availIN / planOUT;
        }
        else
        {
            scalar = 0;
        }

        // Reduce the diversions by multiplying by the scaling factor and send
        // the scaled diversions ToComp (as appropriate).
        for( i=_n_meth-1; i>=0; i-- )
        {
            meth = _method_list[i];
            // Ensure the method is a "top level" method by checking for
            // an expression.  Also check to make sure we are working with
            // a diversion-type method (currently Lookup3 only).
            if( _expr_list[i] != NULL && meth->_group_id == DIVERS_METHOD )
            {
                // If this method is active, then it was executed.
                if ( _method_list[i]->_Active )
                {
                    // Transfer, if appropriate.
                    _method_list[i]->sendToTS( date, scalar );

                    // We have found the active method (it may be Combo or
                    // simple), but we don't need to continue to look.
                    i = -1;
                }
            }
        }

        // Now, lets retraverse the methods and regather the inflows
        // (augmentations) and outflows (diversions) so we can revise our net
        // diversion.
        availIN = 0.0;
        planOUT = 0.0;
        for( i=_n_meth-1; i>=0; i-- )
        {
            meth = _method_list[i];
            // Ensure the method is a "top level" method by checking for
            // an expression.  Also check to make sure we are working with
            // a diversion-type method (currently Lookup3 only).
            if( _expr_list[i] != NULL && meth->_group_id == DIVERS_METHOD )
            {
                // If this method is active, then it was executed.
                if ( _method_list[i]->_Active )
                {
                    // Request the method's value as a diversion
                    methValIN = 0.0;
                    methValOUT = 0.0;
                    _method_list[i]->getMyValueAsOut( &methValIN, &methValOUT );
                    availIN += methValIN;
                    planOUT += methValOUT;

                    // We have found the active method (it may be Combo or
                    // simple), but we don't need to continue to look.
                    i = -1;
                }
            }
        }
        // Modify the _diversion variable.
        _diversion = planOUT - availIN;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Node_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Node_SetGet.cxx,v 1.12 2006/10/26 15:27:51 hsu Exp $";}
/*  ===================================================  */

}
