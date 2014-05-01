//------------------------------------------------------------------------------
// ReservoirMethod - Derived class for methods that only act on reservoir
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
// 16 May 2001	James R. VanShaar, RTi	Added NORMAL, INTERPOLATE_* definitions
// 08 Feb 2006  JRV, RTi     Added INTERPOLATE definitions for LOOKUP3 method.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef ReservoirMethod_INCLUDED
#define ReservoirMethod_INCLUDED

#include "Reservoir.h"
#include "Method.h"

#define	NORMAL 0
#define INTERPOLATE 1
#define INTERPOLATE_TIME 1 
#define INTERPOLATE_ELEV 2 
#define INTERPOLATE_ALL 3 
#define INTERPOLATE_ROWS 1 
#define INTERPOLATE_COLS 2 

class ReservoirMethod : public Method
{
public:
	ReservoirMethod( Reservoir* );			// Default constructor.

	ReservoirMethod( const ReservoirMethod&,
			Reservoir* );	// Copy constructor.

	virtual ~ReservoirMethod();		// Default destructor.

	Reservoir*      getOwner();     // returns _owner

	ReservoirMethod* copy( Component* ) = 0;

	virtual int solveMethod( TSDate&, int, double** = NULL ) = 0;

	virtual int print( FILE* ) = 0;

	virtual int freeDataSpace() = 0;

private:
	int initialize();

protected:
	Reservoir* 	_owner;
};

#endif
