//------------------------------------------------------------------------------
// Table - Encapsulates a table.
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
// 10 Apr 1998  Daniel Weiler, RTI	Added, revised, etc.
//------------------------------------------------------------------------------
#ifndef Table_INCLUDED
#define Table_INCLUDED

#include "ResJ.h"

#define ALLOW_BOUNDS 0		// Used with the lookup routines to allow or 
#define DENY_BOUNDS 1 		// deny lookups on values outside the bounds.

#define GETCOLUMN_1 0		// Used with the lookup routines to signify
#define GETCOLUMN_2 1		// which column to retreive value from.

class Table
{
public:
	int	allocateDataSpace( int );
					// This routine allocates the data
					// space based on the number of 
					// rows passed in.
	int	freeDataSpace();	// Routine used to free the 
					// dynamically allocated data and 
					// re-initialize the data.
	char*	getID();		// Returns the identifier.

	int	getNRows();		// Returns the number of rows.

	double	getMin( int num );
	double	getMax( int num );
					// Gets the min or max value of 
					// column 0, 1, ... num 

	double	lookup( int, int );	// Used to lookup the table value
					// explicitly.
	double	lookup( double, int, int );
					// This routine is used to lookup a
					// value, interpolating as necessary.
					// The arguments are:
					// double - Value to lookup against.
					// int    - either GETCOLUMN_1 or 
					//          GETCOLUMN_2.
					// int    - either ALLOW_BOUNDS or
					//	    DISALLOW_BOUNDS.
	void 	operator= ( const Table& );
					// Equals operator.
	int	populate( char**, int, double, double );
	int 	populate( double*, double*, int );
	int 	populate( int, int, double );
					// Routine used for filling in the 
					// data space.

	int	setID( char* );		// Sets the identifier.

		Table();		// Default constructor.

		Table( const Table& );	// Copy constructor.

		~Table();		// Destructor
private:
	int 	initialize();		// Initializes private data members.

	char	_id[MAXC];		// Name of table (this is optional).

	double	*_col_1;		// The data array for the first
					// column.
	double	*_col_2;		// The data array for the second
					// column.
	int	_rows;			// The number of rows in the table.
};

#endif
