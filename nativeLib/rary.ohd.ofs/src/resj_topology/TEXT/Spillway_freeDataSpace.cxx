//------------------------------------------------------------------------------
// Spillway :: freeDataSpace - delete dynamically allocated memory.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Dec 2002	James R. VanShaar, Riverside Technology, inc
//					Created initial version.
// 2003-11-21 Luiz Teixeira, RTi - Added the missing [] to the *_ts blocks. 
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Spillway.h"

int Spillway :: freeDataSpace()
{
	if( _stor_spill_tbl ) {
		delete _stor_spill_tbl;
	}

	if( _withVol_ts ) {
		delete[] _withVol_ts;
	}
	if( _relVol_ts ) {
		delete[] _relVol_ts;
	}
	if( _stor_ts ) {
		delete[] _stor_ts;
	}
	if( _spillVol_ts ) {
		delete[] _spillVol_ts;
	}
	if( _inflVol_ts ) {
		delete[] _inflVol_ts;
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_freeDataSpace.cxx,v 1.3 2006/10/26 15:36:13 hsu Exp $";}
/*  ===================================================  */

}
