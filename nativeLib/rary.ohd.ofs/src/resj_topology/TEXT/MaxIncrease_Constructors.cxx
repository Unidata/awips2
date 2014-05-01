//------------------------------------------------------------------------------
// MaxIncrease :: MaxIncrease - Constructors.
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
#include "MaxIncrease.h"
#include "Reservoir.h"

MaxIncrease :: MaxIncrease( Reservoir* owner ) : ReservoirMethod( owner )
{
	if( owner == NULL ) {
		PrintWarning( 1, "MaxIncrease::constructor",
		"Cannot set owner to NULL." );
	}
	initialize();

	// Set the _owner
	_owner = owner;
}

MaxIncrease :: MaxIncrease( const MaxIncrease& meth, Reservoir* owner ) 
	: ReservoirMethod( meth, owner )
{
	char	routine[]="MaxIncrease::MaxIncrease";

	initialize();

	_max_increase = meth._max_increase;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxIncrease_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: MaxIncrease_Constructors.cxx,v 1.2 2006/10/26 15:26:17 hsu Exp $";}
/*  ===================================================  */

}
