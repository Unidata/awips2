//------------------------------------------------------------------------------
// TSIdent::setSource - set the full source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full source.  Depending on
//			the information specified, this may also set the
//			source parts.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
// 08 Jan 1998	SAM, RTi		Update to be compatible with Java.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// source	I	The full source.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

// Notes:	(1)	Reset the source from its component parts.  This
//			routine is generally called from setMainSource and
//			setSubSource to reset _full_source.
int TSIdent :: setSource ( void )
{	char	routine[] = "TSIdent.setSource";
	int	dl = 50;

	PrintDebug ( dl, routine, "Resetting full source from its parts" );

	if ( _behavior_mask & NO_SUB_SOURCE ) {
		// Just use the main source as the full source...
		if ( _main_source ) {
			// There should always be a main source after the
			// object is initialized...
			setFullSource ( _main_source );
		}
	}
	else {	// Concatenate the main and sub-sources to get the full
		// source.
		char	full_source[256];
		if ( _main_source ) {
			// We only want to add the subsource if it is not an
			// empty string (it will be an empty string after the
			// object is initialized).
			strcpy ( full_source, _main_source );
			if ( _sub_source ) {
				// We have sub_source so append it to the main
				// source...
				// We have a sub_source so append it to the
				// main source...
				if ( _sub_source[0] ) {
					strcat ( full_source,
					SOURCE_SEPARATOR );
					strcat ( full_source, _sub_source );
				}
			}
			setFullSource ( full_source );
		}
	}
	// Now reset the full identifier...
	setIdentifier ();
	return 0;
}

// Set the source from the component parts...
int TSIdent :: setSource ( char *main_source, char *sub_source )
{	char	routine[] = "TSIdent.setSource(char*,char*)";
	int	dl = 50;

	PrintDebug ( dl, routine, "Setting source using \"%s\" and \"%s\"",
	main_source, sub_source );
	setMainSource ( main_source );
	setSubSource ( sub_source );
	// The full source will be set when the parts are set.
	return 0;
}

// Notes:	(1)	We first split the source into its components and
//			then set the sub-components.
int TSIdent :: setSource( char *source )
{	char	routine[] = "TSIdent.setSource(char*)";
	int	dl = 50;

	if ( !source ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting source using \"%s\"", source );

	if ( _behavior_mask & NO_SUB_SOURCE ) {
		// The entire string passed in is used for the main source...
		setMainSource ( source );
	}
	else {	// Need to split the source into main and sub-source...
		char	**list, sub_source[256];
		int	nlist;
		list =	BreakStringList ( source,
			SOURCE_SEPARATOR, 0, &nlist );
		if ( nlist >= 1 ) {
			// Set the main source...
			setMainSource ( list[0] );
		}
		if ( nlist >= 2 ) {
			// Now set the sub-source...
			sub_source[0] = '\0';
			int iend = nlist - 1;
			for ( int i = 1; i <= iend; i++ ) {
				strcat ( sub_source, list[i] );
				if ( i != iend ) {
					strcat ( sub_source,
					SOURCE_SEPARATOR );
				}
			}
			setSubSource ( sub_source );
		}
		else {	// Since we are only setting the main location we
			// need to set the sub-location to an empty string...
			setSubSource ( "" );
		}
	}

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setSource.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setSource.cxx,v 1.2 2000/05/19 13:08:22 dws Exp $";}
/*  ===================================================  */

}
