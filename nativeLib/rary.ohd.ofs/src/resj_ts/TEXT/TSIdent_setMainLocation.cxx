//------------------------------------------------------------------------------
// TSIdent::setMainLocation - set the main location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the main location and then triggers
//			a reset of the full location.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// main_location I	Main location part of location.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setMainLocation ( char *main_location )
{	char	routine[] = "TSIdent::setMainLocation";
	int	dl = 50;

	if ( !main_location ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set main location to \"%s\"",
	main_location );

	if( _main_location ) {
        	delete [] _main_location;
	}

	_main_location = new char[strlen( main_location )+1];

	if( !_main_location ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for main location \"%s\"",
		main_location );
                return 1;
	}

	strcpy( _main_location, main_location );

	setLocation();

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setMainLocation.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setMainLocation.cxx,v 1.2 2000/05/19 13:08:22 dws Exp $";}
/*  ===================================================  */

}
