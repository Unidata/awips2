//------------------------------------------------------------------------------
// TSIdent::setFullSource - set the full source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full source.  It is only called
//			from within this class.
//------------------------------------------------------------------------------
// History:
// 
// 17 Sep 1997	Steven A. Malers, RTi	Create routine.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// full_source I	Main source part of source.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setFullSource ( char *full_source )
{	char	routine[] = "TSIdent::setFullSource";
	int	dl = 50;

	if ( !full_source ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting full source to \"%s\"",
	full_source );

	if( _full_source ) {
        	delete [] _full_source;
	}

	_full_source = new char[strlen( full_source )+1];

	if( !_full_source ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for full source \"%s\"",
		full_source );
                return( STATUS_FAILURE );
	}

	strcpy( _full_source, full_source );

	// DO NOT call setIdentifier() from here!

        return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setFullSource.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setFullSource.cxx,v 1.2 2000/05/19 13:08:21 dws Exp $";}
/*  ===================================================  */

}
