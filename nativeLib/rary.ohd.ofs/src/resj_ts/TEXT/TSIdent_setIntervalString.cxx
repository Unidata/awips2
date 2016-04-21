//------------------------------------------------------------------------------
// TSIdent::setIntervalString - set the full interval string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the interval string.  It is only
//			called from within this class.
//------------------------------------------------------------------------------
// History:
// 
// 17 Sep 1997	Steven A. Malers, RTi	Create routine.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// interval_string I	Main location part of location.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setIntervalString ( char *interval_string )
{	char	routine[] = "TSIdent::setIntervalString";

	if ( !interval_string ) {
		return 0;
	}

	if( _interval_string ) {
        	delete [] _interval_string;
	}

	_interval_string = new char[strlen( interval_string )+1];

	if( !_interval_string ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for interval string \"%s\"",
		interval_string );
                return( STATUS_FAILURE );
	}

	strcpy( _interval_string, interval_string );

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setIntervalString.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setIntervalString.cxx,v 1.2 2000/05/19 13:08:21 dws Exp $";}
/*  ===================================================  */

}
