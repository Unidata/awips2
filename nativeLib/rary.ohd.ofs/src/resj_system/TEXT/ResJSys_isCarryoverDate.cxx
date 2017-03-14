//------------------------------------------------------------------------------
// ResJSys::isCarryoverDate - If the incoming TSDate matches one in the 
//				_co_dates array, returns a true.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 03 Jun 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "ResJSys.h"

int ResJSys::isCarryoverDate( TSDate& date )
{
	char routine[] = "ResJSys::isCarryoverDate";

	// If the _co_dates array is NULL, then return a false
	if( !_co_dates ) {
		return( STATUS_SUCCESS );
	}

	// Otherwise, try to match the incoming date
	for( int i = 0; i < _num_co_dates; i++ ) {
		if( date == _co_dates[i] ) {
			return( STATUS_FAILURE );
		}
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return( STATUS_SUCCESS );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_isCarryoverDate.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_isCarryoverDate.cxx,v 1.3 2006/10/26 15:31:05 hsu Exp $";}
/*  ===================================================  */

}
