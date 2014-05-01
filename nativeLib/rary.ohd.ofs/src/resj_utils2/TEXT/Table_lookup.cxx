//------------------------------------------------------------------------------
// Table :: lookup - routines to provide access to the table data.
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

double Table :: lookup( int row, int col )
{
	char	routine[]="Table::lookup";
	double	*ptr=NULL;

	if( 	row < 0 || row >= _rows || 
		(col != GETCOLUMN_1 && col != GETCOLUMN_2 ) ){
		PrintWarning( 1, routine,
		"Arguments %d and/or %d are not valid.",
		row, col );
		return( STATUS_FAILURE );
	}
	if( col == GETCOLUMN_1 ){
		ptr = _col_1;
	}
	else {
		ptr = _col_2;
	}

	return( ptr[row] );
}

double Table :: lookup( double val, int col, int flag )
{
	char	routine[]="Table::lookup";
	double	*ptr[2];

	if( col == GETCOLUMN_1 ){
		ptr[0] = _col_2;
		ptr[1] = _col_1;
	}
	else {
		ptr[0] = _col_1;
		ptr[1] = _col_2;
	}

	if( val <= ptr[0][0] ){
		if( flag == ALLOW_BOUNDS ){
			return( ptr[1][0] );
		}
		else {
			// Deny bounds not supported yet
			return( ptr[1][0] );
		}
	}
	if( val > ptr[0][_rows-1] ){
		if( flag == ALLOW_BOUNDS ){
			return( ptr[1][_rows-1] );
		}
		else {
			// Deny bounds not supported yet
			return( ptr[1][_rows-1] );
		}
	}
	// If we got to here we need to do an interpolation...
	return( TableInterp( val, ptr, _rows, DATA_AS_ROWS ) );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils2/RCS/Table_lookup.cxx,v $";
 static char rcs_id2[] = "$Id: Table_lookup.cxx,v 1.3 2006/10/26 15:37:05 hsu Exp $";}
/*  ===================================================  */

}
