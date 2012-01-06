//------------------------------------------------------------------------------
// Reservoir :: copy - Copies "this"	
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 May 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reservoir.h"

Reservoir* Reservoir :: copy()
{
	char	routine[]="Reservoir :: copy";
	Reservoir* copy_res = NULL;
	int i; 

	// Call the copy constructor for the Reservoir. This daisy-chains the
	// Component constructor so we should have our bases covered.
	copy_res = new Reservoir( *this );

//	strcat( copy_res->_id, "_2" );

	return( copy_res );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reservoir_copy.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_copy.cxx,v 1.4 2006/10/26 15:32:22 hsu Exp $";}
/*  ===================================================  */

}
