//------------------------------------------------------------------------------
// TS.setMissing
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setMissing( double missing )
{
	_missing = missing;

	if( missing < 0 ) {

		_missingl = missing * 1.001;
		_missingu = missing * 0.999;
	}
	else {
		_missingl = missing * 0.999;
		_missingu = missing * 1.001;
	}

        return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setMissing.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setMissing.cxx,v 1.2 2000/05/19 13:39:51 dws Exp $";}
/*  ===================================================  */

}
