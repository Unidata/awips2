//------------------------------------------------------------------------------
// SetMin - Object containing all SetMin scheme information.
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
// 22 Apr 1998	Daniel Weiler, RTi	Derived from ComboMethod.
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

#ifndef SetMin_INCLUDED
#define SetMin_INCLUDED

#include "ComboMethod.h"
#include "Node.h"
#include "Reservoir.h"

class SetMin : public ComboMethod 
{
public:
	SetMin( Component*, char* );	// Default constructor

	SetMin( const SetMin&, Component* );
					// Copy constructor.

	virtual ~SetMin();	// Destructor

	SetMin* copy( Component* );

	int freeDataSpace();		// Deletes dynamically allocated data.

	void getMyValueAsOut( double *in, double *out );

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// SetMin object.

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	void  sendToTS( TSDate date );
	void  sendToTS( TSDate date, double scalar );

private:
	int initialize();		// Initialize data members

};

#endif
