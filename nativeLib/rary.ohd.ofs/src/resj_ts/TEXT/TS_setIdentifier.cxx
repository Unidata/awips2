//------------------------------------------------------------------------------
// TS.setIdentifier
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
// 01 Apr 1998	Matthew J. Rutherford, RTi
//					Overload the function to take a
//					TSIdent&
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setIdentifier( TSIdent& identifier )
{
	_id = identifier;

	return( 0 );
}
int TS :: setIdentifier( char *identifier )
{
	if ( identifier ) {
		_id.setIdentifier( identifier );
	}

	return( 0 );
}

int TS :: setIdentifier( 	char* location,	char* source,
				char* type,	char* interval,
				char* scenario )
{
	if ( location ) {
		_id.setLocation( location );
	}

	if ( source ) {
		_id.setSource( source );
	}

	if ( type ) {
		_id.setType( type );
	}

	if ( interval ) {
		_id.setInterval( interval );
	}

	if ( scenario ) {
		_id.setScenario( scenario );
	}

	return( 0 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setIdentifier.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setIdentifier.cxx,v 1.2 2000/05/19 13:39:51 dws Exp $";}
/*  ===================================================  */

}
