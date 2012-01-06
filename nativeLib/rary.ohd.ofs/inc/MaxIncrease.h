//------------------------------------------------------------------------------
// MaxIncrease - Object containing all MaxIncrease scheme information.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 30 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 17 Apr 1998	Daniel Weiler, RTi	Added data members/functions.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef MaxIncrease_INCLUDED
#define MaxIncrease_INCLUDED

#include "ReservoirMethod.h"

class MaxIncrease : public ReservoirMethod 
{
public:
	MaxIncrease( Reservoir* );	// Default constructor

	MaxIncrease( const MaxIncrease&, Reservoir* );
					// Copy constructor.

	virtual ~MaxIncrease();	// Destructor

	int construct( char**, int );	// Time series construction.

	MaxIncrease* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();		// Deletes dynamically allocated data.

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// MaxIncrease object.

private:
	int initialize();		// Initialize data members

        double          _max_increase;  // Value that the release increases
					// compared to the previous release.

};

#endif
