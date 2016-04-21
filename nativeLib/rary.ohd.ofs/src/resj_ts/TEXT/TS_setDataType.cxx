//------------------------------------------------------------------------------
// TS.setDataType
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

int TS :: setDataType( char *data_type )
{
	char	routine[]="TS::setDataType";

	if ( !data_type ) {
		return 1;
	}

	if( _data_type ){
        	delete [] _data_type;
	}

	_data_type = new char[strlen(data_type)+1];

	if( !_data_type ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", data_type );
		return( STATUS_FAILURE );
	}

	strcpy( _data_type, data_type );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setDataType.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setDataType.cxx,v 1.2 2000/05/19 13:38:44 dws Exp $";}
/*  ===================================================  */

}
