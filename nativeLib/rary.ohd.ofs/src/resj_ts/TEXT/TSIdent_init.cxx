//------------------------------------------------------------------------------
// TSIdent::init - initialize private data members
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function should be called from constructors, etc.,
//			when data members need to be initialied.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Update for new data members.
// 07 Jan 1998	SAM, RTi		Add _alias.  Make sure that code agrees
//					with Java version.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// error_count	L	Counter for errors during initialization.
// routine	L	Name of this routine.
// _*		C	All other variables are TSIdent class variables.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: init ( void )
{	char	routine[] = "TSIdent::init";
	int	dl = 50, error_count = 0;

	PrintDebug ( dl, routine, "Initializing TSIdent..." );
	_behavior_mask = 0;	// Default is to process sub-location and
				// sub-source

	// Initialize to null strings so that we do not have problems with the
	// recursive stuff...

	_alias = (char *)NULL;
	_identifier = (char *)NULL;
	_full_location = (char *)NULL;
	_main_location = (char *)NULL;
	_sub_location = (char *)NULL;
	_full_source = (char *)NULL;
	_main_source = (char *)NULL;
	_sub_source = (char *)NULL;
	_type = (char *)NULL;
	_interval_string = (char *)NULL;
	_scenario = (char *)NULL;

	if ( setAlias ("") ) {
		PrintWarning( 1, routine,
		"Unable to initialize alias" );
		++error_count;
	}

	// Initialize the overall identifier to an empty string...

	if ( setFullIdentifier ("") ) {
		PrintWarning( 1, routine,
		"Unable to initialize full identifier" );
		++error_count;
	}

	// Initialize the location components...

	if ( setMainLocation("") ){
		PrintWarning( 1, routine,
		"Unable to initialize main location" );
		++error_count;
	}

	if ( setSubLocation("") ){
		PrintWarning( 1, routine,
		"Unable to initialize sub-location" );
		++error_count;
	}

	// Initialize the source...

	if ( setMainSource("") ){
		PrintWarning( 1, routine,
		"Unable to initialize main source" );
		++error_count;
	}
	if ( setSubSource("") ){
		PrintWarning( 1, routine, "Unable to initialize sub-source" );
		++error_count;
	}

	// Initialize the data type...

	if ( setType("") ){
		PrintWarning( 1, routine, "Unable to initialize type" );
		++error_count;
	}

	// Initialize the interval...

	_interval_base = 0;
	_interval_mult = 0;

	if ( setInterval("") ){
		PrintWarning( 1, routine, "Unable to initialize interval" );
		++error_count;
	}

	// Initialize the scenario...

	if ( setScenario("") ){
		PrintWarning( 1, routine, "Unable to initialize scenario" );
		++error_count;
	}

	PrintDebug ( dl, routine, "...done initializing TSIdent." );
        return error_count;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_init.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_init.cxx,v 1.2 2000/05/19 13:06:36 dws Exp $";}
/*  ===================================================  */

}
