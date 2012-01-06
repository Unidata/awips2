//------------------------------------------------------------------------------
// TSInterval::setBaseString - set interval base string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Port from Java.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// interval_base_string	I Interval base as string.
//------------------------------------------------------------------------------

#include "TSInterval.h"
#include <string.h>

int TSInterval :: setBaseString ( char *interval_base_string )
{	char	routine[]="TSInterval.setBaseString";
	int	dl = 50;

	if ( !interval_base_string ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set sub-location to \"%s\"",
	interval_base_string );

	if( _interval_base_string ){
        	delete [] _interval_base_string;
	}

	_interval_base_string = new char[strlen( interval_base_string )+1];

	if( !_interval_base_string ){
		PrintWarning( 1, routine,
		"Unable to Allocate memory for sub-location \"%s\"",
		interval_base_string );
                return 1;
	}

	strcpy( _interval_base_string, interval_base_string );

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_setBaseString.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_setBaseString.cxx,v 1.1 1999/02/18 15:19:52 dws Exp $";}
/*  ===================================================  */

}
