//------------------------------------------------------------------------------
// ResJSys::run - main entry point for the RESJ operation.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 09 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 01 Apr 1998	MJR	Added code to parse the time series section.
// 14 May 2001	James R. VanShaar, RTi	Modified error and warning handling
//					to correctly represent significance of
//					incorrect input.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// fname	I	File name containing operation description.
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "Solver.h"
#include "TSList.h"

#define	APPEND 1
#define CREATE 2

char* AppendLine( char*, char*, int );

int ResJSys::run( char* fname )
{
	char	*full_line=NULL, line[512]="", **list=NULL, *pt=NULL,
		routine[]="ResJSys::run", **param_list=NULL, **rule_list=NULL,
		**topo_list=NULL, **ts_list=NULL;
	FILE	*fp=NULL;
	int	cont=0, len=0, ntopo_list=0, nparam_list=0, nrule_list=0, 
		nlist=0, status=0, nts_list=0, numErrs=0;
	double lfactor = 1.0, ffactor = 1.0, vfactor = 1.0, conv_val;

	if( !fname ){
		PrintError( routine, "Incoming file name NULL." );
		return( STATUS_FAILURE );
	}
	strcpy( _control_file, fname );

	PrintStatus( 1, routine,
	"Using \"%s\" for RESJ operation information.", fname );

	fp = fopen( fname, "r" );

	if( fp == NULL ){
		PrintError( routine,
		"File \"%s\" is not available for reading.", _control_file );
		return( STATUS_FAILURE );
	}

	while( fgets( line, 256, fp ) != NULL ) {
		if( list ) {
			list = FreeStringList( list );
		}

		UnpadString( line, " \t\n", PAD_BACK );
		list = BreakStringList( line, " \t\n", DELIM_SKIP_BLANKS,
			&nlist );
		if( list == NULL || nlist == 0 ) {
			continue;
		}

		if( !strcasecmp( list[0], "UNITS" ) ) {
			if( !strcasecmp( list[1], "ENGLISH" ) ) {
				lfactor = 0.3048;
				vfactor = 1233.5;
				ffactor = 0.028317;
			}
		break;
		}
	}	
        Component::_lfactor = lfactor;
        Component::_vfactor = vfactor;
       	Component::_ffactor = ffactor;
// printf("\n 1!!!!! lfactor= %10f  ffactor= %10f \n", Component::_lfactor, Component::_ffactor);
	fclose(fp);
	fp = fopen( fname, "r" );

	// We need to parse this file line by line, taking care of things
	// like line continuances, comments and converting the whole thing
	// into upper case....After all this processing is done, a string
	// list of the complete lines is created and this is sub-divided into
	// the TOPOLOGY, RULES and PARAMETERS string lists.

	while( fgets( line, 512, fp ) != NULL ){
		// Get rid of the '\n' character and do an initial
		// unpad, replace tabs with spaces.
		RemoveNewline( line );
		ReplaceChar( line, '\t', ' ' );
		UnpadString( line, " ", PAD_FRONT_BACK );

		len = strlen( line );

		// Now we need to construct the full line out of all the
		// line continuances...
		cont = 0;
		while( line[len-1] == '\\' ){
			// Replace it with a NULL.
			line[len-1] = '\0';

			UnpadString( line, "\t ", PAD_FRONT_BACK );

			if( cont == 0 ){
				full_line = AppendLine( full_line, line, 
				CREATE );
			}
			else {
				full_line = AppendLine( full_line, line, 
				APPEND );
			}
			cont = 1;

			if( fgets( line, 512, fp ) == NULL ){
				// We ran out of lines...
				break;
			}
			RemoveNewline( line );
			UnpadString( line, "\t ", PAD_FRONT_BACK );
			ReplaceChar( line, '\t', ' ' );

			len = strlen( line );
			if( line[len-1] != '\\' ){
				full_line = AppendLine( full_line, line, 
				APPEND );
			}
		}

		if( cont == 0 ){
			full_line = AppendLine( full_line, line, CREATE );
		}
		// Ok if we are here we have a full line which possibly has
		// some '#' comment characters in it.

		ReplaceChar( full_line, '#', '\0' );

		// Ok we have a line that was processed for continuances and 
		// comment characters so we need to determine what list to
		// add this to.

		UnpadString( full_line, "\t ", PAD_FRONT_BACK );

		// We need to make sure that this line actually contains 
		// something...
		//len = strlen( line );
		len = strlen( full_line );

		if( len <= 0 ){
			continue;
		}

		// We need to convert this whole line to upper case values.

		ToUpper( full_line );

		// Now we just need to put this line in a new string
		// list which we will use to carve up into sub lists.

		list = AddToStringList( list, full_line, &nlist );
	}

	if( full_line ){
		free( full_line );
	}

	fclose( fp );

	// Now we need to carve up the main list into the portions...
	// All portions will be tested before returning failure(s) cause
	//	a return to calling function.

	ts_list = GetSubStringList( list, nlist, "TIMESERIES", &nts_list,
		RETURN_NO_KEYWORDS );

	if( ts_list == NULL || nts_list == 0 ){
		numErrs++;
		PrintError( routine,
			"Troubles getting TIMESERIES/ENDTIMESERIES lines." );
	}

	topo_list = GetSubStringList( list, nlist, "TOPOLOGY", &ntopo_list,
		RETURN_NO_KEYWORDS );

	if( topo_list == NULL || ntopo_list == 0 ){
		numErrs++;
		PrintError( routine,
			"Troubles getting TOPOLOGY/ENDTOPOLOGY lines." );
	}

	rule_list = GetSubStringList( list, nlist, "RULES", &nrule_list,
		RETURN_NO_KEYWORDS );

	if( rule_list == NULL || nrule_list == 0 ){
		numErrs++;
		PrintError( routine,
			"Troubles getting RULES/ENDRULES lines." );
	}

	param_list = GetSubStringList( list, nlist, "PARAMETERS", 
		&nparam_list, RETURN_NO_KEYWORDS );

	if( param_list == NULL || nparam_list == 0 ){
		numErrs++;
		PrintError( routine,
			"Troubles getting PARAMETERS/ENDPARAMETERS lines." );
	}
	
	if ( numErrs > 0 ) {
		list = FreeStringList( list );
		ts_list = FreeStringList( ts_list );
		topo_list = FreeStringList( topo_list );
		rule_list = FreeStringList( rule_list );
		param_list = FreeStringList( param_list );
		return( STATUS_FAILURE );
	}

	// Free the big list...
	list = FreeStringList( list );

	// Now we just need to call the parse routines in turn. Timeseries
	// has to be first...
	// Whether we like it or not, these parse functions are generally
	// dependent on the successful completion of the previous one(s).
	// Therefore, we can not check the structure of the entire input file
	// before issuing a warning.  Failure in any parse routine will cause
	// termination of the program prior to calling the remaining parse 
	// routines.
	if( parseTimeSeries( ts_list, nts_list ) ){
		ts_list = FreeStringList( ts_list );
		topo_list = FreeStringList( topo_list );
		rule_list = FreeStringList( rule_list );
		param_list = FreeStringList( param_list );
		return( STATUS_FAILURE );
	}

	if( parseTopology( topo_list, ntopo_list ) ){
		ts_list = FreeStringList( ts_list );
		topo_list = FreeStringList( topo_list );
		rule_list = FreeStringList( rule_list );
		param_list = FreeStringList( param_list );
		return( STATUS_FAILURE );
	}

	if( parseConstants( param_list, nparam_list ) ){
		ts_list = FreeStringList( ts_list );
		topo_list = FreeStringList( topo_list );
		rule_list = FreeStringList( rule_list );
		param_list = FreeStringList( param_list );
		return( STATUS_FAILURE );
	}

	// We have to save the pointer to the root with the expression
	// stuff...
	Expression::setComponentRoot( _root );

	if( parseRules( rule_list, nrule_list, param_list, nparam_list ) ) {
		ts_list = FreeStringList( ts_list );
		topo_list = FreeStringList( topo_list );
		rule_list = FreeStringList( rule_list );
		param_list = FreeStringList( param_list );
		return( STATUS_FAILURE );
	}

	if( parseParameters( param_list, nparam_list ) ){
		ts_list = FreeStringList( ts_list );
		topo_list = FreeStringList( topo_list );
		rule_list = FreeStringList( rule_list );
		param_list = FreeStringList( param_list );
		return( STATUS_FAILURE );
	}
	ts_list = FreeStringList( ts_list );
	topo_list = FreeStringList( topo_list );
	rule_list = FreeStringList( rule_list );
	param_list = FreeStringList( param_list );

	if( !_root ){
		PrintWarning( 1, routine,
		"Topology tree is NULL!" );
		return( STATUS_FAILURE );
	}
	if( _root->checkMethods() ){
		// If this method returns failure, it means that there was an
		// entry in the rules section that wasn't in the parameters
		// section...
		return( STATUS_FAILURE );
	}

	PrintStatus( 1, routine,
	"Checked syntax of \"%s\".", _control_file );

	return( STATUS_SUCCESS );
}

int ResJSys::run( char* fname, TSDate& t1, TSDate& t2 )
{
	char routine[] = "ResJSys::run( char* fname, TSDate& t1, TSDate& t2 )";

	_t1	= t1;
	_t2	= t2;

	Method::setForecastDate1( _t1 );
	Method::setForecastDate2( _t2 );

	return( run( fname ) );
}

int ResJSys::run( 
	char* fname, int nts, char** tsids, float** tsdata )
{
	char	routine[]="ResJSys::run";
	int	base=0, i=0, j=0, mult=0;
	TS	*tspt=NULL;
	TSDate	t;
	TSIdent	id;

	// Check the validity of the arguments...
	if( !fname || !tsids || !tsdata ){
		PrintWarning( 1, routine,
		"Incoming arguments are NULL." );
		return( STATUS_FAILURE );
	}

	// First we have to run the parsing routine...
	if( run( fname ) ) {
		return( STATUS_FAILURE );
	}

	// Need to clear out the CO list for the actual ResJ execution
	deleteCOString();
	// t1 and t2 are set by the calling function 

	PrintStatus( 1, routine,
	"Running simulation from %s - %s.", _t1.toString(), _t2.toString() );

	// Now we have to work our way through the time series and 
	// save the data.

	for( i=0; i<nts; i++ ){
		// We have to look up the timeseries in the TSList and 
		// fill in the data...

		tspt = TSList::getTSFromList( tsids[i] );

		if( !tspt ){
			PrintWarning( 1, routine,"time series %s does not"
			" appear in the TIMESERIES section of the control file.",
			tsids[i] );
			// We want to continue here since the parsing code
			// will have made sure that all the needed timeseries
			// are in the TIMESERIES section.
			return( STATUS_FAILURE );
		}

		// Allocate memory here as opposed to in parseTimeseries
		if( tspt->allocateDataSpace() ) {
			PrintWarning( 1, routine, "Troubles allocating memory.");
			return( STATUS_FAILURE );
		}

		id = tspt->getIdentifier();

		base = id.getIntervalBase();
		mult = id.getIntervalMult();
		for( j=0, t=_t1; t <= _t2; j++, t.addInterval( base, mult ) ){
			if( tspt->setDataValue( t, tsdata[i][j] ) ){
				PrintWarning( 1, routine,
				"Troubles setting data value to %f at %s.",
				tsdata[i][j], t.toString() );
			}
		}
	}

	// We have filled in the data so now we have to solve...

	return( runSolver() );
}

char* AppendLine( char* line, char* new_line, int flag )
{
	char	routine[]="AppendLine";
	int	new_len=0, old_len=0, out_len=0;

	if( new_line == NULL ){
		PrintWarning( 1, routine, "Line to append is NULL." );
		return( NULL );
	}

	if( line == NULL ){ // This is the first time...  old_len = 0;
	}
	else {
		old_len = strlen( line );
	}

	new_len = strlen( new_line );

	if( flag == CREATE ){
		out_len = new_len+1;

		if( line != NULL ){
			// we are supposed to create a line, but there has 
			// already been memory allocated for it, we need
			// to free it first.
			free( line );
			line = NULL;
		}
	}
	else {
		out_len = old_len + new_len + 2;
	}

	line = (char*)realloc( (char*)line, (out_len * sizeof( char )) );

	if( line == NULL ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %d char.", out_len );
		return( NULL );
	}

	if( flag == CREATE ){
		strcpy( line, new_line );
	}
	else {
		strcat( line, " " );
		strcat( line, new_line );
	}

	return( line );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_run.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_run.cxx,v 1.7 2006/10/26 15:31:28 hsu Exp $";}
/*  ===================================================  */

}
