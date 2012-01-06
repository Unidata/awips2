//------------------------------------------------------------------------------
// TSIdent::setFullIdentifier - set the full identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full identifier.  It is only
//			called from within this class.
//------------------------------------------------------------------------------
// History:
// 
// 17 Sep 1997	Steven A. Malers, RTi	Create routine.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// full_identifier I	Full identifier.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setFullIdentifier ( char *full_identifier )
{	char	routine[] = "TSIdent::setFullIdentifier";

	if ( !full_identifier ) {
		return 0;
	}

	if( _identifier ) {
        	delete [] _identifier;
	}

	_identifier = new char[strlen( full_identifier )+1];

	if( !_identifier ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for full identifier \"%s\"",
		full_identifier );
                return( STATUS_FAILURE );
	}

	strcpy( _identifier, full_identifier );

	// DO NOT call setIdentifier() from here!

        return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setFullIdentifier.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setFullIdentifier.cxx,v 1.2 2000/05/19 13:08:21 dws Exp $";}
/*  ===================================================  */

}
