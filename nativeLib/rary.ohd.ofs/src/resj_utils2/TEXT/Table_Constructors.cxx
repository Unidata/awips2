//------------------------------------------------------------------------------
// Table :: Table - Constructors.
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
// 05 May 1998	MJR	Added guts to the copy constructor.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "resj/Table.h"

Table :: Table()
{
	initialize();
}

Table :: Table( const Table& t )
{
	char	routine[]="Table::Table(const Table&)";

	initialize();

	// Copy the ID.
	strcpy( _id, t._id );

	// Send in the arrays to create a new table.
	if( populate( t._col_1, t._col_2, t._rows ) ){
		PrintWarning( 1, routine,
		"Troubles copying Table data." );
		freeDataSpace();
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils2/RCS/Table_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Table_Constructors.cxx,v 1.3 2006/10/26 15:36:34 hsu Exp $";}
/*  ===================================================  */

}
