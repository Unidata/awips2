//------------------------------------------------------------------------------
// Table Set/Get routines.
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
// 28 Apr 1998	Daniel Weiler, RTi	Added getMin and getMax.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "resj/Table.h"

char* Table :: getID()
{
	return( _id );
}

int Table :: getNRows()
{
	return( _rows );
}

double Table :: getMax( int pos )
{
	if( pos == GETCOLUMN_1 ) {
		return( _col_1[_rows-1] );
	}
	else {
		return( _col_2[_rows-1] );
	}
}

double Table :: getMin( int pos )
{
	if( pos == GETCOLUMN_1 ) {
		return( _col_1[0] );
	}
	else {
		return( _col_2[0] );
	}
}

int Table :: setID( char* id )
{
	if( !id ){
		char	routine[]="Table::setID";
		PrintWarning( 1, routine,
		"Incoming argument is NULL." );
		return( STATUS_FAILURE );
	}
	// We use strncpy here since the _id value is statically allocated.
	strncpy( _id, id, MAXC );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils2/RCS/Table_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Table_SetGet.cxx,v 1.3 2006/10/26 15:36:45 hsu Exp $";}
/*  ===================================================  */

}
