//------------------------------------------------------------------------------
// Spillway :: copy - calls copy constructor for method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 24 Dec 2002	James R. VanShaar, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Spillway.h"

Spillway* Spillway :: copy( Component* owner )
{
	Spillway* meth = NULL;
	meth = new Spillway( *this, (Reservoir*)owner );
	return( meth );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_copy.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_copy.cxx,v 1.2 2006/10/26 15:36:07 hsu Exp $";}
/*  ===================================================  */

}
