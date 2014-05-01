//------------------------------------------------------------------------------
// Reach :: Reach - constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 05 May 1998	DKW		Enabled copy constructors.	
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reach.h"

Reach :: Reach() : Component() 
{
	initialize();
}

Reach :: Reach( const Reach& reach ) : Component( reach )
{
	initialize();

	_outflow = reach._outflow;


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reach_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Reach_Constructors.cxx,v 1.3 2006/10/26 15:29:58 hsu Exp $";}
/*  ===================================================  */

}
