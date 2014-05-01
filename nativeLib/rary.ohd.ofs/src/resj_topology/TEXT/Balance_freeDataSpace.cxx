//------------------------------------------------------------------------------
// Balance :: freeDataSpace - delete dynamically allocated memory.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 27 Apr 1998  Daniel Weiler, RTi	Added first cut at functionality.
// 12 Jun 2002	James R. VanShaar, RTi	Added _max_rel.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Balance.h"

int Balance :: freeDataSpace()
{
	int i;		

	if( _lower_stor != NULL ) {
		delete [] _lower_stor;
	}
	if( _upper_stor != NULL ) {
		delete [] _upper_stor;
	}
	if( _min_rel != NULL ) {
		delete [] _min_rel;
	}
	if( _max_rel != NULL ) {
		delete [] _max_rel;
	}
	if( _balance_res != NULL ) {
		delete [] _balance_res;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Balance_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Balance_freeDataSpace.cxx,v 1.3 2006/10/26 15:11:15 hsu Exp $";}
/*  ===================================================  */

}
