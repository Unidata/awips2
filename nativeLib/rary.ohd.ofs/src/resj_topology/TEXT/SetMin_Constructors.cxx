//------------------------------------------------------------------------------
// SetMin :: SetMin - Constructors.
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
// 22 Apr 1998  Daniel Weiler, RTi	Derived from ComboMethod.
// 16 Feb 2006  JRV, RTi        Generalized the method for different component
//                              types.  (It Was Reservoir only.)
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetMin.h"
#include "Component.h"

SetMin :: SetMin( Component* owner, char* type ) : ComboMethod( owner, type )
{
	initialize();
}

SetMin :: SetMin( const SetMin& meth, Component* owner ) : 
	ComboMethod( meth, owner )
{
	char	routine[]="SetMin :: SetMin";

	initialize();


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetMin_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: SetMin_Constructors.cxx,v 1.3 2006/10/26 15:33:43 hsu Exp $";}
/*  ===================================================  */

}
