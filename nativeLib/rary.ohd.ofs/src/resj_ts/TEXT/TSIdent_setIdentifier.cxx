//------------------------------------------------------------------------------
// TSIdent::setIdentifier - set the full identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine forms the full identifier.  Depending on
//			the arguments, this routine may decompose the data sent
//			in and set the subcomponents.  The full identifier is
//			ultimately set by the setIdentifier(void) function.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Update to have a void version to reset
//					the identifier.
// 08 Jan 1997	SAM, RTi		Clean up to match Java version.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

// Notes:	(1)	Version to compose the identifier from its components.
int TSIdent :: setIdentifier ( void )
{	char	routine[] = "TSIdent::setIdentifier(void)";
	int	dl = 50;

	// We assume that all the individual set routines have handled the
	// _behavior_mask accordingly and therefore we can just concatenate
	// strings here...

	PrintDebug ( dl, routine, "Setting full identifier from parts..." );

	char	full_identifier[256];
	PrintDebug ( dl, routine, "Calling getIdentifierFromParts..." );
	if (	getIdentifierFromParts(_full_location, _full_source, _type,
		_interval_string, _scenario, full_identifier ) ) {
		PrintWarning ( 2, routine,
		"Problem getting full identifier from parts" );
		return 1;
	}
	PrintDebug ( dl, routine,
	"...successfully called getIdentifierFromParts..." );

	setFullIdentifier ( full_identifier );
	return 0;
}

// Notes:	(1)	Version to set the identifier given a full identifier.
//			The identifier will be split first so that the
//			component parts can be set.
int TSIdent :: setIdentifier( char *identifier )
{	char	routine[] = "TSIdent::setIdentifier";
	int	dl = 20;

	if ( !identifier ) {
		return 1;
	}

	PrintDebug ( dl, routine, "Trying to set identifier to \"%s\"",
	identifier );

	if ( !identifier[0] ) {
		// We cannot parse the identifier because doing so would get us
		// into an infinite loop.  If this routine is being called with
		// an empty string, it is a mistake.  The initialization code
		// will call setFullIdentifier() directly.
		PrintDebug ( dl, routine,
		"Identifier string is empty, not processing!" );
		return 1;
	}

	// Parse the identifier using the public static function to create a
	// temporary identifier object...

	TSIdent tsident;
	PrintDebug ( dl, routine, "Done declaring temp TSIdent." );
	PrintDebug ( dl, routine, "Parsing identifier..." );
	tsident = parseIdentifier ( identifier, _behavior_mask );
	PrintDebug ( dl, routine, "...back from parsing identifier" );

	// Now copy the temporary copy into this instance...

	PrintDebug ( dl, routine, "Setting the individual parts..." );
	setLocation( tsident.getLocation() );
	setSource( tsident.getSource() );
	setType( tsident.getType() );
	setInterval( tsident.getInterval() );
	setScenario( tsident.getScenario() );
	PrintDebug ( dl, routine, "... done setting the individual parts" );

	// The temporary object will be deleted when we leave this function.
	return 0;
}

// Notes:	(1)	This function sets the idenfier using its component
//			parts.
int TSIdent :: setIdentifier (	char *full_location, char *full_source,
				char *type, char *interval_string,
				char *scenario )
{
	setLocation ( full_location );
	setSource ( full_source );
	setType ( type );
	setInterval ( interval_string );
	setScenario ( scenario );
	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setIdentifier.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setIdentifier.cxx,v 1.2 2000/05/19 13:08:21 dws Exp $";}
/*  ===================================================  */

}
