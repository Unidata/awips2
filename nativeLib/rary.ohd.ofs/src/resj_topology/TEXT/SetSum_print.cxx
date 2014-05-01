//------------------------------------------------------------------------------
// SetSum :: print - printd instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 Mar 1998	Matthew J. Rutherford, RTi
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetSum.h"
#include "TSDateIterator.h"

int SetSum :: print( FILE* fp )
{
	char routine[] = "SetSum :: print";

	if( fp == NULL ) {
		PrintWarning( 1, routine, "Cannot print SetSum info -"
		" null FILE*.");
		return( STATUS_FAILURE );
	}

	// Print self id fnd owner first
	fprintf( fp, "SetSum method \"%s\" owned by Component \"%s\".\n", 
		_id, _owner->getID() );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetSum_print.cxx,v $";
 static char rcs_id2[] = "$Id: SetSum_print.cxx,v 1.3 2006/10/26 15:34:54 hsu Exp $";}
/*  ===================================================  */

}
