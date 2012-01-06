//------------------------------------------------------------------------------
// SetSum - Object containing all SetSum scheme information.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 22 Apr 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 22 Apr 1998 	Daniel Weiler, RTI	Derived from ComboMethod.
// 28 Nov 2001  James R. VanShaar, RTi  Added setInactiveState
// 13 Dec 2002	JRV, RTi	Added sendToTS.
// 16 Feb 2006  JRV, RTi        Generalized the method for different component
//                              types.  (It Was Reservoir only.)
// 27 Mar 2006  JRV, RTi    Added void sendToTS( TSDate date ),
//                          void sendToTS( TSDate date, double scalar ), and
//                          getMyValueOut( double *in, double *out ).
//                          Eliminated double sendToTS( TSDate, double, double).
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef SetSum_INCLUDED
#define SetSum_INCLUDED

#include "ComboMethod.h"
#include "Node.h"
#include "Reservoir.h"

class SetSum : public ComboMethod 
{
public:
	SetSum( Component*, char* );	// Default constructor

	SetSum( const SetSum&, Component* );
					// Copy constructor.

	virtual ~SetSum();	// Destructor

	SetSum* copy( Component* );

	int freeDataSpace();		// Deletes dynamically allocated data.

	void getMyValueAsOut( double *in, double *out );

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// SetSum object.

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	void  sendToTS( TSDate date );
	void  sendToTS( TSDate date, double scalar );

private:
	int initialize();		// Initialize data members

//	Reservoir* 	_owner;		// Pointer to Reservoir component
					// that this method belongs to.
};

#endif
