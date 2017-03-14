//------------------------------------------------------------------------------
// LagK :: print - printd instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Mar 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "LagK.h"
#include "TSDateIterator.h"

int LagK :: print( FILE* fp )
{
	char routine[] = "LagK :: print";
	int i;
	TSDate	date, start;
	TSDateIterator* tsdi = NULL;

	if( fp == NULL ) {
		PrintWarning( 1, routine, "Cannot print LagK info -"
		" null FILE*.");
		return( STATUS_FAILURE );
	}

	// Print self id fnd owner first
	fprintf( fp, "LagK method \"%s\" owned by Component \"%s\".\n", 
		_id, _owner->getID() );
 	fprintf( fp, "End of \"%s\" information.\n\n", _id );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_print.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_print.cxx,v 1.3 2006/10/26 15:22:59 hsu Exp $";}
/*  ===================================================  */

}
