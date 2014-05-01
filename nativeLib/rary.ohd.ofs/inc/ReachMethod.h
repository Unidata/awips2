//------------------------------------------------------------------------------
// ReachMethod - Derived class for methods that only act on reservoir
//			components.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 22 Apr 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef ReachMethod_INCLUDED
#define ReachMethod_INCLUDED

#include "Method.h"
#include "Reach.h"

class ReachMethod : public Method
{
public:
	ReachMethod( Reach* );			// Default constructor.

	ReachMethod( const ReachMethod&, Reach* );	
						// Copy constructor.

	virtual ~ReachMethod();		// Default destructor.

	ReachMethod* copy( Component* ) = 0;

	Reach*      getOwner();     // returns _owner

	virtual int solveMethod( TSDate&, int, double** = NULL ) = 0;

	virtual int print( FILE* ) = 0;

	virtual int freeDataSpace() = 0;

private:
	int initialize();

protected:
	Reach* 	_owner;
};

#endif
