//------------------------------------------------------------------------------
// TS.setDescription
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

int TS :: setDescription( char *description )
{
	char	routine[] = "TS::setDescrition";

	if ( !description ) {
		return 1;
	}

	if( _description ) {
        	delete [] _description;
	}

	_description = new char[strlen( description )+1];

	if( !_description ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", description );
                return( STATUS_FAILURE );
	}

	strcpy( _description, description );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setDescription.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setDescription.cxx,v 1.2 2000/05/19 13:39:50 dws Exp $";}
/*  ===================================================  */

}
