/*
** TableInterp.c - Daniel Weiler, Riverside Technology, inc.
** 
** Function will linearly interpolate for a y value given an x and a table
** to lookup the y value
**
** February 17, 1998
*/

#include "ResJ.h"

double TableInterp( double x_value, double** table, int table_length,
	int flag)
{
	int i;

	if( table_length == 1 ) {
		return( x_value );
	}
	if( flag == DATA_AS_COLUMNS ) {
		for( i = 1; i < table_length; i++ ) {
			if( table[i][0] <= table[i-1][0] ) {
				PrintWarning( 1, "TableInterp", 
				"X axis of table must be increasing." );
				return( -999.0 );
			}
			if( x_value <= table[i][0] && x_value > 
			table[i-1][0] ) {
				return( Interp( (float)x_value, 
					(float)table[i-1][0],
					(float)table[i][0], 
					(float)table[i-1][1], 
					(float)table[i][1] ) );
			}
		}
	}

	if( flag == DATA_AS_ROWS ) {
		for( i = 1; i < table_length; i++ ) {
			if( table[0][i] <= table[0][i-1] ) {
				PrintWarning( 1, "TableInterp", 
				"X axis of table must be increasing." );
				return( -999.0 );
			}
			if( x_value <= table[0][i] && x_value > 
			table[0][i-1] ) {
				return( Interp( (float)x_value, 
					(float)table[0][i-1],
					(float)table[0][i], 
					(float)table[1][i-1], 
					(float)table[1][i] ) );
			}			
		}
	}

	/* If we get here then something is wrong with the table*/
	return -999.0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/TableInterp.c,v $";
 static char rcs_id2[] = "$Id: TableInterp.c,v 1.1 1999/02/18 15:17:24 dws Exp $";}
/*  ===================================================  */

}
