//------------------------------------------------------------------------------
// Reach :: copy - Copies "this"	
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 05 May 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reach.h"

Reach* Reach :: copy()
{
	char	routine[]="Reach :: copy";
	Reach* copy_reach = NULL;
	int i; 

	// Call the copy constructor for the Reservoir. This daisy-chains the
	// Component constructor so we should have our bases covered.
	copy_reach = new Reach( *this );

//	strcat( copy_reach->_id, "_2" );
	return( copy_reach );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reach_copy.cxx,v $";
 static char rcs_id2[] = "$Id: Reach_copy.cxx,v 1.4 2006/10/26 15:30:17 hsu Exp $";}
/*  ===================================================  */

}
