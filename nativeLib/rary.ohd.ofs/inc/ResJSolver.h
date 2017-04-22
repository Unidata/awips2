//------------------------------------------------------------------------------
// ResJSolver - encapsulates a joint reservoir solver.
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

#ifndef ResJSolver_INCLUDED
#define ResJSolver_INCLUDED

#include "Component.h"

class ResJSolver
{
public:
	int freeDataSpace();	// Frees dynamically allocated data.

	ResJSolver();			// Default constructor.

	ResJSolver( const ResJSolver& );
				// Copy constructor.

	~ResJSolver();		// Destructor.

	operator= ( const ResJSolver& );
				// = Operator.
private:
	initialize();		// Initializes private data members.

	Component	*_topo_root;
				// The root of the topology tree.
};
#endif
