//------------------------------------------------------------------------------
// TSIdent - overloaded operators
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford,	Created member function.
// 16 Sep 1997	Steven A. Malers, RTi	Update to use new data members.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

TSIdent& TSIdent :: operator= ( const TSIdent& id )
{	char	routine[] = "TSIdent::operator=";
	int	dl = 50;

	// Don't need to call setIdentifier() because it will be set by its
	// parts.

	PrintDebug ( dl, routine, "Copying data..." );
	setAlias ( id._alias );
	setBehaviorMask ( id._behavior_mask );
	setLocation( id._full_location );
	setSource( id._full_source );
	setType( id._type );
	setInterval( id._interval_string );
	_interval_base = id._interval_base;
	_interval_mult = id._interval_mult;
	setScenario( id._scenario );

	// Now reset the parts...
	setIdentifier();

	PrintDebug ( dl, routine, "...done copying data." );

	return *this;
}

int TSIdent :: operator== ( const TSIdent& id )
{
	// Just call the named function...
	return equals ( id );
}

// Named function...
int TSIdent :: equals ( const TSIdent& id ) {
	return( !strcmp( _identifier, id._identifier ) );
}

int TSIdent :: operator!= ( const TSIdent& id )
{
	// Just call the named function...
	return notEquals ( id );
}

int TSIdent :: notEquals ( const TSIdent& id )
{
	if ( equals(id) ) {
		return 0;
	}
	else {	return 1;
	}
}

TSIdent :: operator char *( void )
{
	// Call the named function...

	return toString ();

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_Operators.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_Operators.cxx,v 1.2 2000/05/19 13:06:32 dws Exp $";}
/*  ===================================================  */

}
