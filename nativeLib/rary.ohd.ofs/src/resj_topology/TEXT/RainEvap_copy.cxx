//------------------------------------------------------------------------------
// RainEvap :: copy - calls copy constructor for method.
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
#include "RainEvap.h"

RainEvap* RainEvap :: copy( Component* owner )
{
	RainEvap* meth = NULL;
	meth = new RainEvap( *this, (Reservoir*)owner );
	return( meth );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/RainEvap_copy.cxx,v $";
 static char rcs_id2[] = "$Id: RainEvap_copy.cxx,v 1.2 2006/10/26 15:29:24 hsu Exp $";}
/*  ===================================================  */

}
