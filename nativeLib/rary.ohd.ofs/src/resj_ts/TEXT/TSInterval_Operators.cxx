//----------------------------------------------------------------------------
// TSInterval Operators
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)	For Java, we do not have overloaded operators, but
//			for the C++ versions we have now implemented Java-like
//			functions so that it is easier to keep the C++ and
//			Java versions compatible.
//----------------------------------------------------------------------------

#include "TSInterval.h"

//----------------------------------------------------------------------------
// TSInterval overload = operator
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	
//
//----------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Initial C++ version.	
//		Riverside Technology,
//		inc.
//----------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// interval	I	Incoming TSInterval.
//----------------------------------------------------------------------------

TSInterval& TSInterval :: operator= ( const TSInterval& interval )
{
	if ( &interval == this) {
                // self declaration
        }
        else {	_interval_base		= interval._interval_base;
		_interval_mult		= interval._interval_mult;
		setBaseString ( interval._interval_base_string );
        }
	return *this;
}

TSInterval :: operator char *( void )
{
	// Call the named function...

	return toString ();

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_Operators.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_Operators.cxx,v 1.1 1999/02/18 15:19:45 dws Exp $";}
/*  ===================================================  */

}
