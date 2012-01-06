//------------------------------------------------------------------------------
// Passflow :: copy - calls copy constructor for method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
//  23 AUG 2004	KSH, OHD	Added this method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Passflow.h"

Passflow* Passflow :: copy( Component* owner )
{
	Passflow* meth = NULL;
	meth = new Passflow( *this, (Reservoir*)owner );
	return( meth );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Passflow_copy.cxx,v $";
 static char rcs_id2[] = "$Id: Passflow_copy.cxx,v 1.2 2006/10/26 15:28:54 hsu Exp $";}
/*  ===================================================  */

}
