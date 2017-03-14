//------------------------------------------------------------------------------
// Balance :: print - printd instance data members.
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
#include "Balance.h"
#include "TSDateIterator.h"

int Balance :: print( FILE* fp )
{
	char routine[] = "Balance :: print";

	if( fp == NULL ) {
		PrintWarning( 1, routine, "Cannot print Balance info -"
		" null FILE*.");
		return( STATUS_FAILURE );
	}

	// Print self id fnd owner first
	fprintf( fp, "Balance method \"%s\" owned by Component \"%s\".\n", 
		_id, _owner->getID() );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Balance_print.cxx,v $";
 static char rcs_id2[] = "$Id: Balance_print.cxx,v 1.3 2006/10/26 15:11:21 hsu Exp $";}
/*  ===================================================  */

}
