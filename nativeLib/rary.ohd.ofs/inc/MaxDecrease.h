//------------------------------------------------------------------------------
// MaxDecrease - Object containing all MaxDecrease scheme information.
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
// 17 Apr 1998	Daniel Weiler, RTi	Added data members/functions
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef MaxDecrease_INCLUDED
#define MaxDecrease_INCLUDED

#include "ReservoirMethod.h"

class MaxDecrease : public ReservoirMethod 
{
public:
	MaxDecrease( Reservoir* );	// Default constructor

	MaxDecrease( const MaxDecrease&, Reservoir* );
					// Copy constructor.

	virtual ~MaxDecrease();	// Destructor

	int construct( char**, int );	// Time series construction.

	MaxDecrease* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();		// Deletes dynamically allocated data.

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// MaxDecrease object.

private:
	int initialize();		// Initialize data members

        double          _max_decrease;  // Value that the release decreases
					// compared to the previous 24 hrs' 
					// average release.

};

#endif
