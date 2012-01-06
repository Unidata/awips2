//------------------------------------------------------------------------------
// SetRelease::print - printd instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetRelease.h"

int SetRelease::print( FILE* fp )
{
	char routine[] = "SetRelease::print";
	int i;
	TSDate	date, start;

	if( fp == NULL ) {
		PrintWarning( 1, routine, "Cannot print SetRelease info -"
		" null FILE*.");
		return( STATUS_FAILURE );
	}

	// Print self id fnd owner first
	fprintf( fp, "SetRelease method \"%s\" owned by Component \"%s\".\n", 
		_id, _owner->getID() );

	for( i = 0; i < _n_elev; i++ ) {
		fprintf( fp, "Release timeseries corresponding to "
		"Elevation: %f  :\n", _elev[i] );
		for( date = _release_ctl[i].getDate1(); 
		date <= _release_ctl[i].getDate2(); 
		date.addInterval( _t_int, _t_mult ) ) {
			PrintDebug( 10, "Date: %s	Release: %f\n", 
			date.toString(), _release_ctl[i].getDataValue( date, 1 ) );
		}
	}

 	fprintf( fp, "End of \"%s\" information.\n\n", _id );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetRelease_print.cxx,v $";
 static char rcs_id2[] = "$Id: SetRelease_print.cxx,v 1.3 2006/10/26 15:34:28 hsu Exp $";}
/*  ===================================================  */

}
