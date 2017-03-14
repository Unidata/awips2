//------------------------------------------------------------------------------
// Method :: allocateDataSpace 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 23 Mar 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Method.h"

double** Method :: allocateDataSpace( int rows, int cols ) 
{
	char routine[] = "Method :: allocateDataSpace";
	int i,j;
	double** temp = NULL;

	if( rows == 0 || cols == 0 ) {
		PrintWarning( 1, routine, "Zero rows and/or columns not a "
		"valid size for data block." );
		return( NULL );
	}

	temp = new double* [rows];

	for( i = 0; i < rows; i++ ) {
		temp[i] = new double[ cols ];

		if( temp[i] == NULL ) {
			PrintWarning( 1, routine, "Unable to allocate %d "
			"doubles for row %d.", cols, (i+1) );
			return( NULL );
		}

		for( j = 0; j < cols; j++ ) {
			temp[i][j] = MISSING;
		}
	}
	return( temp );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Method_allocateDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Method_allocateDataSpace.cxx,v 1.3 2006/10/26 15:27:19 hsu Exp $";}
/*  ===================================================  */

}
