//------------------------------------------------------------------------------
// TS.setVersion
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

int TS :: setVersion( char *version )
{
	char	routine[]="TS::setVersion";

	if ( !version ) {
		return 1;
	}

	if( _version ){
		delete [] _version;
	}

	_version = new char[strlen(version)+1];

	if( !_version ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", version );
                return( STATUS_FAILURE);
	}

	strcpy( _version, version );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setVersion.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setVersion.cxx,v 1.2 2000/05/19 13:39:52 dws Exp $";}
/*  ===================================================  */

}
