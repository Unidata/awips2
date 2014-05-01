//------------------------------------------------------------------------------
// Table :: allocateDataSpace - allocates memory for the data storage.
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

int Table :: allocateDataSpace( int rows )
{
	char	routine[]="Table::allocateDataSpace";
	if( rows < 0 ){
		PrintWarning( 1, routine,
		"The number of rows %d is invalid.", rows );
		return( STATUS_FAILURE );
	}

	if( freeDataSpace() ){
		return( STATUS_FAILURE );
	}

	// Clib is stupid and has problems allocating an array of size 1.
	if( rows > 1 ) { 
		_col_1 = new double [ rows ];
		_col_2 = new double [ rows ];
	}
	else {
		_col_1 = new double;
		_col_2 = new double;
	}

	if( !_col_1 || !_col_2 ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %d double.", rows );
		if( _col_1 ){
			delete _col_1;
		}
		if( _col_2 ){
			delete _col_2;
		}
		return( STATUS_FAILURE );
	}

	_rows	= rows;

	for( int i=0; i<_rows; i++ ){
		_col_1[i]	= -999.0;
		_col_2[i]	= -999.0;
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return( STATUS_SUCCESS );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils2/RCS/Table_allocateDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Table_allocateDataSpace.cxx,v 1.4 2006/10/26 15:36:54 hsu Exp $";}
/*  ===================================================  */

}
