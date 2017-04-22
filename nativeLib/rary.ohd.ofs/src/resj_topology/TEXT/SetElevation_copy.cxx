//------------------------------------------------------------------------------
// SetElevation :: copy - calls copy constructor for method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 05 May 1998 	Daniel Weiler, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetElevation.h"

SetElevation* SetElevation :: copy( Component* owner )
{
	SetElevation* meth = NULL;
	meth = new SetElevation( *this, (Reservoir*)owner );
	return( meth );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetElevation_copy.cxx,v $";
 static char rcs_id2[] = "$Id: SetElevation_copy.cxx,v 1.2 2006/10/26 15:33:00 hsu Exp $";}
/*  ===================================================  */

}
