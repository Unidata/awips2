//------------------------------------------------------------------------------
// ResJSys :: freeDataSpaceStatic - destroys dynamically allocated static data.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 29 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 28 Oct 2003	James R. VanShaar, RTi	Modified to free _co_dates properly.
// 11 Feb 2004  James R. VanShaar, RTi  Eliminated problematic free _co_dates.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "TSList.h"

int ResJSys :: freeDataSpaceStatic()
{
	if( _co_list ){
		free( _co_list );
	}
	if( _debug_str ){
		free( _debug_str );
	}
	if( _po_list ){
		free( _po_list );
	}
	if( _warning_str ){
		free( _warning_str );
	}
	if( _co_array_str ) {
		for( int i = 0; i <_num_co_str; i++ ) {
			if( _co_array_str[i] != NULL ) {
				delete [] _co_array_str[i];
			}
		}
		delete [] _co_array_str;
	}
	//if( _co_dates ) {
	//	delete []( _co_dates );
	//}

	TSList :: DeleteTSList();
	initializeStatic();
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_freeDataSpaceStatic.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_freeDataSpaceStatic.cxx,v 1.6 2006/10/26 15:30:57 hsu Exp $";}
/*  ===================================================  */

}
