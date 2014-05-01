//------------------------------------------------------------------------------
// Reservoir :: printContents - prints out the contents of a reservoir.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reservoir.h"

void Reservoir :: printContents( FILE* fp )
{
	if( !fp ){
		return;
	}

	fprintf( fp, "Pool: %0.3f\n", _pool );

	for( int i=0; i< (_elev_stor_tbl.getNRows() ); i++ ){
		fprintf( fp, "Elevation[%d]  %f  Storage  %f\n", i, 
		_elev_stor_tbl.lookup( i, 0 ), 
		_elev_stor_tbl.lookup( i, 1 ) );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reservoir_printContents.cxx,v $";
 static char rcs_id2[] = "$Id: Reservoir_printContents.cxx,v 1.2 2006/10/26 15:32:35 hsu Exp $";}
/*  ===================================================  */

}
