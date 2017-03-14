//------------------------------------------------------------------------------
// ReservoirMethod :: ReservoirMethod - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 22 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ReservoirMethod.h"

ReservoirMethod :: ReservoirMethod( Reservoir* owner ) : Method() 
{
	initialize();
	_owner = owner;
}

ReservoirMethod :: ReservoirMethod( const ReservoirMethod& meth, 
	Reservoir* owner ) : Method( meth )
{
	char	routine[]="ReservoirMethod :: ReservoirMethod";

	initialize();

	_owner = owner;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/ReservoirMethod_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: ReservoirMethod_Constructors.cxx,v 1.3 2006/10/26 15:31:52 hsu Exp $";}
/*  ===================================================  */

}
