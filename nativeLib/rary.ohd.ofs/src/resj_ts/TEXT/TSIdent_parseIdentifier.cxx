//------------------------------------------------------------------------------
// TSIdent Constructor - create using full identifier
//------------------------------------------------------------------------------
// History:
// 
// 17 Sep 1997	Steven A. Malers, RTi	This routine supersedes SplitTSID.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// dl		L	Debug level.
// full_location L	Location of TS.
// full_source	L	Source of the time series data.
// i		L	Loop counter for "list" strings.
// identifier	I	5-part time series identifier string.
// interval_string L	TS interval, as string.
// list		L	List of period-separated strings that comprise "tsid".
// nlist*	L	Number of strings in "list".
// quote	L	Quote character used for locations.
// scenario	L	Scenario identifier.
// type		L	TS type.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

#include <ctype.h>
#include <string.h>

TSIdent TSIdent :: parseIdentifier ( char *identifier )
{	char	routine[] = "TSIdent.parseIdenifier(char*)";
	TSIdent	tsident;
	
	tsident = parseIdentifier ( identifier, 0 );
	return tsident;
}

TSIdent TSIdent :: parseIdentifier (	char *identifier,
					unsigned int behavior_flag )
{	char	routine[]="TSIdent::parseIdentifier(char*,flag)";
	int	dl = 50;
	
	// Declare a TSIdent which we will fill and return...

	PrintDebug ( dl, routine, "Declare TSIdent within this routine..." );
	TSIdent	tsident ( behavior_flag );
	PrintDebug ( dl, routine, "...done declaring TSIdent" );

	// Parse the identifier using the code from SplitTSID...

	char	full_location[256], full_source[256], interval_string[256],
		**list, quote, scenario[256], type[256];
	int	i, nlist1;

	interval_string[0]	= '\0';
	full_location[0]	= '\0';	
	full_source[0]		= '\0';
	scenario[0]		= '\0';
	type[0]			= '\0';

	// Figure out whether we are using the new or old conventions.  First
	// check to see if the number of fields is small.  Then check to see if
	// the data type and interval are combined.

	list = BreakStringList ( identifier, ".", 0, &nlist1 );
	for ( i = 0; i < nlist1; i++ ) {
		PrintDebug ( dl, routine,
		"TS ID list[%d]:  \"%s\"", i, list[i] );
	}
	list = FreeStringList ( list );

	PrintDebug ( dl, routine, "Full TS ID:  \"%s\"", identifier );

	// Parse out location and split the rest of the ID...
	//
	// This field is allowed to be surrounded by quotes since some
	// locations cannot be identified by a simple string.  Allow
	// either ' or " to be used and bracket it.

	quote = '\0';
	if ( (identifier[0] == '\'') || (identifier[0] == '\"') ) {
		StringReadToDelim ( &identifier[1], identifier[0],
		full_location );
		// Get the 2nd+ fields...
		quote = identifier[0];
		list =	BreakStringList (&identifier[strlen(full_location)+1],
			".", 0, &nlist1 );
	}
	else {	list =	BreakStringList ( identifier, ".", 0, &nlist1 );
		if ( nlist1 >= 1 ) {
			strcpy ( full_location, list[0] );
		}
	}
	// Data source...
	if ( nlist1 >= 2 ) {
		strcpy ( full_source, list[1] );
	}
	// Data type...
	if ( nlist1 >= 3 ) {
		strcpy ( type, list[2] );
	}
	// Data interval...
	if ( nlist1 >= 4 ) {
		strcpy ( interval_string, list[3] );
	}
	// Scenario...  It is possible that the scenario has delimeters
	// in it.  Therefore, we need to concatenate all the remaining
	// fields to compose the complete scenario...
	if ( nlist1 >= 5 ) {
		strcpy ( scenario, list[4] );
		for ( i = 5; i < nlist1; i++ ) {
			strcat ( scenario, "." );
			strcat ( scenario, list[i] );
		}
	}
	// Now free the memory for the list...
	FreeStringList ( list );
	list = (char **)NULL;
	// Now split the location again into major area (to replace
	// "location") and subarea.  They are divided by an underscore.
	// Only do this for non-quoted locations.
	//
	// This is probably unneeded given the new TSIdent set routine
	// functionality so comment out for now...
	/*
	if ( !quote && strchr(location,'_') ) {
		list = BreakStringList ( location, "_", 0, &nlist1 );
		strcpy ( location, list[0] );
		if ( nlist1 > 1 )
			strcpy ( subarea, list[1] );
		else	*subarea = '\0';
		FreeStringList ( list );
		list = (char **)NULL;
	}
	*/
	PrintDebug ( dl, routine,
	"After split: fullloc=\"%s\" fullsrc=\"%s\" type=\"%s\" int=\"%s\" scen=\"%s\"",
	full_location, full_source, type, interval_string, scenario );

	// Now set the identifier component parts...

	tsident.setLocation ( full_location );
	tsident.setSource ( full_source );
	tsident.setType ( type );
	tsident.setInterval ( interval_string );
	tsident.setScenario ( scenario );

	// Return the TSIdent object for use elsewhere...

	PrintDebug ( dl, routine, "Returning local TSIdent..." );
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_parseIdentifier.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_parseIdentifier.cxx,v 1.3 2000/05/19 13:08:20 dws Exp $";}
/*  ===================================================  */

	return tsident;

}
