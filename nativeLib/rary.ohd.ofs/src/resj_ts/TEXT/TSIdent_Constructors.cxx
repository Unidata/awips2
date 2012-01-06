//------------------------------------------------------------------------------
// TSIdent constructors
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The TSIdent constructors allow a TSIdent object to be
//			intantiated with various identifier components.
//------------------------------------------------------------------------------
// History:
// 
// 08 Nov 1996	Matthew J. Rutherford,	Created member functions.
//		RTi
// 16 Sep 1997	Steven A. Malers, RTi	Update to use new suite of TSIdent
//					member functions.
// 07 Jan 1998	SAM, RTi		Update to agree with Java version and
//					add time series alias.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

//------------------------------------------------------------------------------
// Default Constructor - default
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford, RTi	created member function
//------------------------------------------------------------------------------
TSIdent :: TSIdent ( void )
{
	init();
}

// Variant with behavior mask...
TSIdent :: TSIdent ( unsigned int mask )
{
	init ();
	setBehaviorMask ( mask );
}

//------------------------------------------------------------------------------
// TSIdent Constructor - create using full identifier
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford, RTi	created member function
//------------------------------------------------------------------------------
TSIdent :: TSIdent ( char *identifier )
{	char	routine[] = "TSIdent::TSIdent(char*)";
	
	// Initialize the data members...

	PrintDebug ( 50, routine, "Enter constructor" );
	init();
	setIdentifier ( identifier );

}

// Variant with behavior mask...
TSIdent :: TSIdent ( char *identifier, unsigned int mask )
{	char	routine[] = "TSIdent::TSIdent(char*,mask)";

	// Initialize the data members...

	PrintDebug ( 50, routine, "Enter constructor" );
	init();
	setBehaviorMask ( mask );
	setIdentifier ( identifier );
}

//------------------------------------------------------------------------------
// TSIdent Constructor - use separate identifier components
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford,	Created member function.
//		RTi
// 16 Sep 1997	Steven A. Malers, RTi	Update to use new suite of member
//					functions.
//------------------------------------------------------------------------------
TSIdent :: TSIdent (	char *full_location, char *full_source, char *type,
			char *interval_string, char *scenario )
{	char	routine[] = "TSIdent::TSIdent(parts)";

	PrintDebug ( 50, routine, "Enter constructor" );
	init();
	setIdentifier ( full_location, full_source, type, interval_string,
			scenario );
}

// Variant with behavior mask...
TSIdent :: TSIdent (	char *full_location, char *full_source, char *type,
			char *interval_string, char *scenario,
			unsigned int mask )
{	char	routine[] = "TSIdent::TSIdent(parts,mask)";

	PrintDebug ( 50, routine, "Enter constructor" );
	init();
	setBehaviorMask ( mask );
	setIdentifier ( full_location, full_source, type, interval_string,
			scenario );
}

//------------------------------------------------------------------------------
// TSIdent Copy Constructor
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford,	Created member function.
//		RTi
//------------------------------------------------------------------------------
TSIdent :: TSIdent ( const TSIdent& ts )
{	char	routine[] = "TSIdent::TSIdent(copy)";

	//Identifier will get set from its parts

	PrintDebug ( 50, routine, "Enter copy constructor" );
	init();
	setBehaviorMask ( ts._behavior_mask );
	// Do not use the following!  It triggers infinite recursion!
	//setIdentifier ( ts._identifier );
	setIdentifier ( ts._full_location, ts._full_source, ts._type,
			ts._interval_string, ts._scenario );
	setAlias( ts._alias );
	_interval_base = ts._interval_base;
	_interval_mult = ts._interval_mult;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_Constructors.cxx,v 1.2 2000/05/19 13:04:31 dws Exp $";}
/*  ===================================================  */

}
