//------------------------------------------------------------------------------
// LagK :: copy - calls copy constructor for method.
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
#include "LagK.h"

LagK* LagK :: copy( Component* owner )
{
	LagK* meth = NULL;
	meth = new LagK( *this, (Reach*)owner );

//	strcat( meth->_id, "_2" );

	return( meth );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_copy.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_copy.cxx,v 1.3 2006/10/26 15:22:30 hsu Exp $";}
/*  ===================================================  */

}
