//------------------------------------------------------------------------------
// Table :: freeDataSpace - deletes dynamically allocated memory.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 07 Apr 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "resj/Table.h"

int Table :: freeDataSpace()
{
	if( _col_1 ){
		if( _rows > 1 ) {
			delete [] _col_1;
		}
		else {
			delete _col_1;
		}
	}
	if( _col_2 ){
		if( _rows > 1 ) {
			delete [] _col_2;
		}
		else {
			delete _col_2;
		}
	}
	initialize();

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils2/RCS/Table_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Table_freeDataSpace.cxx,v 1.3 2006/10/26 15:36:58 hsu Exp $";}
/*  ===================================================  */

}
