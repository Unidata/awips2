//------------------------------------------------------------------------------
// Spillway :: print - printd instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 24 Dec 2002	James R. VanShaar, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Spillway.h"
#include "TSDateIterator.h"

int Spillway :: print( FILE* fp )
{
	char routine[] = "Spillway :: print";

	if( fp == NULL ) {
		PrintWarning( 1, routine, "Cannot print Spillway info -"
		" null FILE*.");
		return( STATUS_FAILURE );
	}

	// Print self id fnd owner first
	fprintf( fp, "Spillway method \"%s\" owned by Component \"%s\".\n", 
		_id, _owner->getID() );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_print.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_print.cxx,v 1.2 2006/10/26 15:36:19 hsu Exp $";}
/*  ===================================================  */

}
