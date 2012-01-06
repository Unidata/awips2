//------------------------------------------------------------------------------
// Passflow - Object containing all Passflow scheme information.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
//  6 AUG 2004	KSH, OHD	Added this method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Passflow_INCLUDED
#define Passflow_INCLUDED

#include "ReservoirMethod.h"

class Passflow : public ReservoirMethod 
{
public:
	Passflow( Reservoir* );	// Default constructor

	Passflow( const Passflow&, Reservoir* );
					// Copy constructor.

	virtual ~Passflow();	// Destructor

	int construct( char**, int );	// Time series construction.

	Passflow* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();		// Deletes dynamically allocated data.

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// MaxIncrease object.

private:
	int initialize();		// Initialize data members

};

#endif
