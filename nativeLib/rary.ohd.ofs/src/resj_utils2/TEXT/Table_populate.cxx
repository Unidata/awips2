//------------------------------------------------------------------------------
// Table :: populate - fills in a table.
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
// 18 Apr 1998	Daniel Weiler, RTi	Added default argument factor to be
//					applied to the second column - default
//					is 1.0. 
// 13 Apr 2006	James R. VanShaar, RTi	Added Error message for nsublist != 2,
//					and revised Incoming NULL arrays test.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "resj/Table.h"

int Table :: populate( char** list, int nlist, double lfactor, double vfactor )
{
	char	routine[]="Table::populate";

	if( !list || !nlist ){
		PrintWarning( 1, routine,
		"Incoming arguments are NULL or zero." );
		return( STATUS_FAILURE );
	}

	if( allocateDataSpace( nlist ) ){
		return( STATUS_FAILURE );
	}
	for( int i=0; i<nlist; i++ ){
		int	nsublist=0;
		char	**sublist = BreakStringList( list[i], " \t\n", 
			DELIM_SKIP_BLANKS, &nsublist );

		if( !sublist || nsublist != 2 ){
			PrintError( routine, "Table row \"%s\"\n does "
				"not have exactly 2 fields.", list[i] );
			sublist = FreeStringList( sublist );
			freeDataSpace();
			return( STATUS_FAILURE );
		}

		if( populate( i, 0, atof( sublist[0] ) * lfactor ) ){
			sublist = FreeStringList( sublist );
			freeDataSpace();
			return( STATUS_FAILURE );
		}
		if( populate( i, 1, atof( sublist[1] ) * vfactor ) ){
			sublist = FreeStringList( sublist );
			freeDataSpace();
			return( STATUS_FAILURE );
		}

		sublist = FreeStringList( sublist );
	}

	return( STATUS_SUCCESS );
}

int Table :: populate( double *c1, double *c2, int size )
{
	char	routine[]="Table::populate";

	if( !c1 || !c2 ){
		PrintWarning( 1, routine,
		"Incomining arrays are NULL." );
		return( STATUS_FAILURE );
	}

	if( allocateDataSpace( size ) ){
		return( STATUS_FAILURE );
	}

	for( int i=0; i<size; i++ ){
		if( populate( i, 0, c1[i] ) ){
			return( STATUS_FAILURE );
		}
		if( populate( i, 1, c2[i] ) ){
			return( STATUS_FAILURE );
		}
	}
	return( STATUS_SUCCESS );
}

int Table :: populate( int row, int col, double value )
{
	char	routine[]="Table::populate";
	if( row < 0 || row >= _rows ){
		PrintWarning( 1, routine,
		"Row value %d is not valid.", row );
		return( STATUS_FAILURE );
	}

	if( col < 0 || col > 1 ){
		PrintWarning( 1, routine,
		"Column value %d is not valid.", col );
		return( STATUS_FAILURE );
	}

	if( col == 0 ){
		_col_1[row] = value;
	}
	else {
		_col_2[row] = value;
	}
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils2/RCS/Table_populate.cxx,v $";
 static char rcs_id2[] = "$Id: Table_populate.cxx,v 1.3 2006/10/26 15:37:09 hsu Exp $";}
/*  ===================================================  */

}
