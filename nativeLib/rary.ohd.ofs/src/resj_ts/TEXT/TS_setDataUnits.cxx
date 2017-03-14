//------------------------------------------------------------------------------
// TS.setDataUnits
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

int TS :: setDataUnits( char *data_units )
{
	char	routine[]="TS::setDataUnits";

	if ( !data_units ) {
		return 1;
	}

	if( _data_units ) {
		delete [] _data_units;
		_data_units = (char*)NULL;
	}

	_data_units = new char [strlen(data_units)+1];

	if( !_data_units ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", data_units );
                return( STATUS_FAILURE );
	}

	strcpy( _data_units, data_units );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setDataUnits.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setDataUnits.cxx,v 1.2 2000/05/19 13:38:45 dws Exp $";}
/*  ===================================================  */

}
