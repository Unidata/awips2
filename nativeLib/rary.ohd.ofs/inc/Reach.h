//------------------------------------------------------------------------------
// Reach - encapsulates information about a node topology type.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 06 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 13 Nov 2001  James R. VanShaar, RTi  Added function setCOstring
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Reach_INCLUDED
#define Reach_INCLUDED

#include "Component.h"

class Reach : public Component
{
public:
	Reach* 	copy();		// Calls copy constructor on "this".

	//HourTS* getOutputTS( char* );

        int finalizeSolution( TSDate& ); 
				// Handles solution cleanup at the end of 
				// each time-step.
	int freeDataSpace();	// Frees dynamically allocated data.

	int setStates( char**, int );
				// Sets state variable of the Reach.

	Reach();			// Default constructor.

	Reach( const Reach& );
				// Copy constructor.

	virtual ~Reach();		// Destructor.

	void operator= ( const Reach& );
				// = Operator.

	double	_outflow;	// Local working copy of the outflow being
				// computed.

	virtual void printContents( FILE* );

	int setCOstring (TSDate&);      // Prepares adds carryover string to the
					// ResJSys carryover array

protected:
	virtual double* getInternalValuePtr( char* );
				// Returns an internal pointer based on 
				// the identifier that is passed in.
private:
	int initialize();	// Initializes private data members.
};
#endif
