//------------------------------------------------------------------------------
// TS.setDataUnitsOriginal
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

int TS :: setDataUnitsOriginal( char *units )
{
	char	routine[] = "TS::setDataUnitsOriginal";

	if ( !units ) {
		return 1;
	}

	if( _data_units_original ){
		delete [] _data_units_original;
	}

	_data_units_original = new char[strlen(units)+1];

	if( !_data_units_original ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", units );
                return( STATUS_FAILURE );
	}

	strcpy( _data_units_original, units );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setDataUnitsOriginal.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setDataUnitsOriginal.cxx,v 1.2 2000/05/19 13:38:45 dws Exp $";}
/*  ===================================================  */

}
