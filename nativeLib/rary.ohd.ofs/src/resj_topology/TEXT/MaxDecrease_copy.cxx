//------------------------------------------------------------------------------
// MaxDecrease :: copy - calls copy constructor for method.
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
#include "MaxDecrease.h"

MaxDecrease* MaxDecrease :: copy( Component* owner )
{
	MaxDecrease* meth = NULL;
	meth = new MaxDecrease( *this, (Reservoir*)owner );
	return( meth );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxDecrease_copy.cxx,v $";
 static char rcs_id2[] = "$Id: MaxDecrease_copy.cxx,v 1.2 2006/10/26 15:25:59 hsu Exp $";}
/*  ===================================================  */

}
