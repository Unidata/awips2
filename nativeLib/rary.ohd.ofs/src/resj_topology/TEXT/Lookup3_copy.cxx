//------------------------------------------------------------------------------
// Lookup3 :: copy - calls copy constructor for method.
//------------------------------------------------------------------------------
// History:
// 
// 01 Feb 2006  James R. VanShaar, RTi	Created initial file, although not
//                                      functional.
//------------------------------------------------------------------------------
#include "Lookup3.h"

Lookup3* Lookup3 :: copy( Component* owner )
{
	Lookup3* meth = NULL;
//	meth = new Lookup3( *this, (Reservoir*)owner );
	return( meth );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Lookup3_copy.cxx,v $";
 static char rcs_id2[] = "$Id: Lookup3_copy.cxx,v 1.1 2006/10/26 15:24:30 hsu Exp $";}
/*  ===================================================  */

}
