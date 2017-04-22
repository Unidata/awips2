//------------------------------------------------------------------------------
// Reach Set/Get routines.
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
// 28 Feb 1998	Daniel Weiler, RTi	Added most functionality.
// 16 Apr 2001  James R. VanShaar, RTi	Made correction in assignment of
//					TSIdent
// 15 May 2001	JRV, RTi	Improved error / warning handling in setStates
// 10 Jul 2001	JRV, RTi	Corrected issues from misnamed time series and
//				use of full identifiers
// 13 Nov 2001  JRV, RTi        Added setCOstring
// 12 Mar 2004	JRV, RTi	Added TSOutput of Inflow timeseries.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reach.h"
#include "TSList.h"

double* Reach :: getInternalValuePtr( char* id )
{
	char	routine[]="Reach :: getInternalValuePtr";

	// Basically we just have to loop through the reserved key-words and 
	// check if we have one.

	// None yet...

	return( NULL );
}

int Reach :: setCOstring( TSDate & date )
{
	char routine[]="Reach::setCOstring";
	// Need to update this
	return(STATUS_SUCCESS);
}

int Reach :: setStates( char** states, int n_items )
{
	char routine[]="Reach :: setStates", **list = NULL, ts_id[MAXC]="";
	int i, j, nlist = 0, n_out = 0, totErrs = 0;

	for( i = 0; i < n_items; i++ ) {
                if( strlen( states[i] ) == 0 || states[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( states[i], " \n\t",
			DELIM_SKIP_BLANKS, &nlist );
                if( list == NULL ) {
			totErrs++;
			PrintError( routine, "Troubles getting state "
				"data values for %s - REACH class malformed.",
				getID() );
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
                        if( !strcasecmp( list[1], "INFLOW" ) ) {
				// Check for TSINPUT INFLOW in addition to 
				// inflow from an upstream component
				if( _n_son != 0 ) {
					PrintWarning( 1, routine, 
						"Applying 'Lateral-type' flow "
						"\"%s\" at head of reach "
						"\"%s\". This is in addition "
						"to the inflow(s) from "
						"component(s) topologically "
						"upstream. ", list[2],
						getID() );
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
				// Check to see if it is an instantaneous data 
				// type.	
				if( !_inflow_ts[_in_count-1]->
					getInstantaneous() ) {
					PrintWarning( 2, routine, 
						"Inflow time series %s for %s "
						"is not set as instantaneous.",
						ts_id, _id );
				}
			}
		list = FreeStringList( list );
		continue;
		}
		
		// Handle keyword TSOUTPUT
		else if( !strcasecmp( list[0], "TSOUTPUT" ) ) {
			if ( nlist < 3 ) {
				totErrs++;
				PrintError( routine, 
					"Keyword OUTFLOW (or RELEASE) and "
					"timeseries identifier required "
					"immediately after reach keyword %s.", 
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
			if( !strcasecmp( list[1], "OUTFLOW" ) ||
				!strcasecmp( list[1], "RELEASE" ) ) {
				_output_ts[n_out] = &_outflow_ts; 
				if( _output_ts[n_out] == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
						"%s as a valid reach %s "
						"timeseries.", list[0], ts_id );
					list = FreeStringList( list );
					continue;
				}
			}
			else if (!strcasecmp( list[1], "INFLOW" ) ) {
				_output_ts[n_out] = &_totalInflow; 
				if( _output_ts[n_out] == NULL ) {
					totErrs++;
					PrintError( routine, "Could not find "
						"%s as a valid reach %s "
						"timeseries.", list[0], ts_id );
					list = FreeStringList( list );
					continue;
				}
			}
			else {
				totErrs++;
				PrintError( routine,
					"Keyword OUTFLOW, RELEASE, or INFLOW "
					"required immediately after reach "
					"keyword %s. %s is not valid.", 
					list[0], list[1] );
				list = FreeStringList( list );
				continue;
			}
			HourTS *tempTS = (HourTS*)TSList::getTSFromList(ts_id);
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

 		// Handle keyword CONSTANT
                // CONSTANTs are handled by parseConstant, so skip over
                else if( !strcasecmp( list[0], "CONSTANT" ) ) {
                        continue;
                }

		// Handle other
		else {
			PrintWarning(1, routine, "Unrecognized keyword \"%s\" "
				"for Reach line \"%s\".", list[0], states[i] );
		}

		list = FreeStringList( list );
		continue;
	}

	if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}

	_n_output_ts = n_out;
	return( STATUS_SUCCESS ); 

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reach_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Reach_SetGet.cxx,v 1.7 2006/10/26 15:30:13 hsu Exp $";}
/*  ===================================================  */

}
