//------------------------------------------------------------------------------
// SetRelease::copy - calls copy constructor for method.
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
#include "SetRelease.h"

SetRelease* SetRelease::copy( Component* owner )
{
	SetRelease* meth = NULL;
	meth = new SetRelease( *this, (Reservoir*)owner );
	return( meth );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetRelease_copy.cxx,v $";
 static char rcs_id2[] = "$Id: SetRelease_copy.cxx,v 1.3 2006/10/26 15:34:19 hsu Exp $";}
/*  ===================================================  */

}
