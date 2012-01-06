//------------------------------------------------------------------------------
// TSIdent.setLocation - set the full location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full location.  Depending on
//			the information specified, this may also set the
//			location parts.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// location	I	The full location.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

// Notes:	(1)	Reset the location from its component parts.  This
//			routine is generally called from setMainLocation and
//			setSubLocation to reset _full_location.
int TSIdent :: setLocation ( void )
{	char	routine[] = "TSIdent.setLocation(void)";
	int	dl = 50;
	
	PrintDebug ( dl, routine, "Resetting full location from parts..." );
	if ( _behavior_mask & NO_SUB_LOCATION ) {
		// Just use the main location as the full location...
		if ( _main_location ) {
			// There should always be a main location after the
			// object is initialized...
			setFullLocation ( _main_location );
		}
	}
	else {	// Concatenate the main and sub-locations to get the full
		// location.
		char	full_location[256];
		// We may want to check for _main_location[] also...
		if ( _main_location ) {
			// This should always be the case after the object is
			// initialized...
			strcpy ( full_location, _main_location );
			if ( _sub_location ) {
				// We only want to add the sublocation if it is
				// not an empty string (it will be an empty
				// string after the object is initialized).
				if ( _sub_location[0] ) {
					// We have a sub_location so append it
					// to the main location...
					strcat ( full_location,
					LOCATION_SEPARATOR );
					strcat ( full_location, _sub_location );
				}
			}
			setFullLocation ( full_location );
		}
	}
	// Now reset the full identifier...
	setIdentifier ();
	return 0;
}

// Set the location from the component parts...
int TSIdent :: setLocation ( char *main_location, char *sub_location )
{
	setMainLocation ( main_location );
	setSubLocation ( sub_location );
	// The full location will be set when the parts are set.
	return 0;
}

// Notes:	(1)	We first split the location into its components and
//			then set the sub-components.
int TSIdent :: setLocation( char *location )
{	char	routine[] = "TSIdent::setLocation(char*)";
	int	dl = 20;

	if ( !location ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set location to \"%s\"",
	location );

	if ( _behavior_mask & NO_SUB_LOCATION ) {
		// The entire string passed in is used for the main location...
		setMainLocation ( location );
	}
	else {	// Need to split the location into main and sub-location...
		char	**list, sub_location[256];
		int	nlist;
		list =	BreakStringList ( location,
			LOCATION_SEPARATOR, 0, &nlist );
		if ( nlist >= 1 ) {
			// Set the main location...
			setMainLocation ( list[0] );
		}
		if ( nlist >= 2 ) {
			// Now set the sub-location...
			sub_location[0] = '\0';
			int iend = nlist - 1;
			for ( int i = 1; i <= iend; i++ ) {
				strcat ( sub_location, list[i] );
				if ( i != iend ) {
					strcat ( sub_location,
					LOCATION_SEPARATOR );
				}
			}
			setSubLocation ( sub_location );
		}
		else {	// Since we are only setting the main location we
			// need to set the sub-location to an empty string...
			setSubLocation ( "" );
		}
		list = FreeStringList( list );
	}

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setLocation.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setLocation.cxx,v 1.2 2000/05/19 13:08:22 dws Exp $";}
/*  ===================================================  */

}
