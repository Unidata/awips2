//------------------------------------------------------------------------------
// Solver - encapsulates a joint reservoir solver.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)It is intended that this solver be used for the primary 
//		solution of the system, and any sub-solutions that are 
//		specific to methods.
//------------------------------------------------------------------------------
// History:
// 
// 06 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 19 Mar 1998	MJR	Put in the guts of the solver.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Solver_INCLUDED
#define Solver_INCLUDED

#include "Component.h"

class Solver
{
public:
	int freeDataSpace();	// Frees dynamically allocated data.

	Solver();			// Default constructor.

	Solver( const Solver& );
				// Copy constructor.

	~Solver();		// Destructor.

	void operator= ( const Solver& );
				// = Operator.
	int run( Component*, TSDate&, TSDate&, int, int, int );
				// Main execution point for the solver. The
				// inputs to this system are the topology tree,
				// the start and end dates for the simulation,
				// the base and mult interval and the 
				// isPrimary flag.
private:
	int initialize();	// Initializes private data members.

	Component	*_root;
				// The root of the topology tree.
	int		_isPrimary;
				// Flag which tells this solver that it is the
				// primary solver. This flag should be set to
				// TRUE for only one solver in any system.
};
#endif
