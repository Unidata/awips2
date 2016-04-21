//------------------------------------------------------------------------------
// MaxDecrease :: MaxDecrease - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 Mar 1998	Matthew J. Rutherford, RTi
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "MaxDecrease.h"
#include "Reservoir.h"

MaxDecrease :: MaxDecrease( Reservoir* owner ) : ReservoirMethod( owner )
{
	if( owner == NULL ) {
		PrintWarning( 1, "MaxDecrease::constructor",
		"Cannot set owner to NULL." );
	}
	initialize();

	// Set the _owner
	_owner = owner;
}

MaxDecrease :: MaxDecrease( const MaxDecrease& meth, Reservoir* owner ) 
	: ReservoirMethod( meth, owner )
{
	char	routine[]="MaxDecrease::MaxDecrease";

	initialize();

	_max_decrease = meth._max_decrease;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxDecrease_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: MaxDecrease_Constructors.cxx,v 1.2 2006/10/26 15:25:51 hsu Exp $";}
/*  ===================================================  */

}
