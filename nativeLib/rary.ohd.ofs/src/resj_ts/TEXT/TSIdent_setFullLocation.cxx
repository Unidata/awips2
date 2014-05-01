//------------------------------------------------------------------------------
// TSIdent::setFullLocation - set the full location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full location.  It is only called
//			from within this class.
//------------------------------------------------------------------------------
// History:
// 
// 17 Sep 1997	Steven A. Malers, RTi	Create routine.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// full_location I	Main location part of location.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setFullLocation ( char *full_location )
{	char	routine[] = "TSIdent::setFullLocation";
	int	dl = 50;

	if ( !full_location ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Resetting full location to \"%s\"",
	full_location );

	if( _full_location ) {
        	delete [] _full_location;
	}

	_full_location = new char[strlen( full_location )+1];

	if( !_full_location ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for full location \"%s\"",
		full_location );
                return( STATUS_FAILURE );
	}

	strcpy( _full_location, full_location );

	// DO NOT call setIdentifier() from here!

        return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setFullLocation.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setFullLocation.cxx,v 1.2 2000/05/19 13:08:21 dws Exp $";}
/*  ===================================================  */

}
