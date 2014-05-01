//------------------------------------------------------------------------------
// ReachMethod :: ReachMethod - Constructors.
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
#include "ReachMethod.h"

ReachMethod :: ReachMethod( Reach* owner ) : Method() 
{
	initialize();
	_owner = owner;
}

ReachMethod :: ReachMethod( const ReachMethod& meth, Reach* owner ) : 
	Method( meth )
{
	char	routine[]="ReachMethod :: ReachMethod";

	initialize();

	_owner = owner; 


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/ReachMethod_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: ReachMethod_Constructors.cxx,v 1.3 2006/10/26 15:29:43 hsu Exp $";}
/*  ===================================================  */

}
