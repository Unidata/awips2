//------------------------------------------------------------------------------
// TSIdent::setSubLocation - set the sub-location part of the location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the sublocation and then calls
//			setLocation() to reset the full location.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
// 08 Jan 1998	SAM, RTi		Update to agree with Java.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// sub_location	I	Sub-location to set.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setSubLocation ( char *sub_location )
{	char	routine[]="TSIdent.setSubLocation";
	int	dl = 50;

	if ( !sub_location ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set sub-location to \"%s\"",
	sub_location );

	if( _sub_location ){
        	delete [] _sub_location;
	}

	_sub_location = new char[strlen( sub_location )+1];

	if( !_sub_location ){
		PrintWarning( 1, routine,
		"Unable to Allocate memory for sub-location \"%s\"",
		sub_location );
                return 1;
	}

	strcpy( _sub_location, sub_location );

	setLocation();

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setSubLocation.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setSubLocation.cxx,v 1.2 2000/05/19 13:08:22 dws Exp $";}
/*  ===================================================  */

}
