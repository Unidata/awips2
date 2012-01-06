//------------------------------------------------------------------------------
// TSIdent.setType - set the data type part of the TS identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	Data types are the NWS data types or other
//			abbreviations.  Maybe we need a _behavior_flag setting
//			that forces a check for a valid data type?
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
// 08 Jan 1998	SAM, RTi		Update to agree with Java port.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// type		I	Time series data type.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setType ( char *type )
{	char 	routine[] = "TSIdent.setType";
	int	dl = 50;

	if ( !type ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting type to \"%s\"...", type );

	if( _type ) {
        	delete [] _type;
	}

	_type = new char[strlen( type )+1];

	if( !_type ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for data type \"%s\"", type );
                return 1;
	}

	strcpy( _type, type );

	setIdentifier();

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setType.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setType.cxx,v 1.2 2000/05/19 13:08:23 dws Exp $";}
/*  ===================================================  */

}
