//------------------------------------------------------------------------------
// ComboMethod - Derived class for methods that only act on multiple methods.
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
// 16 Feb 2006  JRV, RTi        Generalized the method for different component
//                              types.  (It Was Reservoir only.)
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef ComboMethod_INCLUDED
#define ComboMethod_INCLUDED

#include "ComponentMethod.h"

class ComboMethod : public ComponentMethod
{
public:
	ComboMethod( Component*, char* );  // Default constructor.

	ComboMethod( const ComboMethod&, Component* );	
					// Copy constructor.

	virtual ~ComboMethod();		// Default destructor.

	int construct( char**, int );

	virtual ComboMethod* copy( Component* ) = 0;

	virtual int freeDataSpace() = 0;

	virtual int print( FILE* ) = 0;

	virtual int solveMethod( TSDate&, int, double** = NULL ) = 0;

	void setGroup(ComboMethod*);
	
private:
	int initialize();

protected:
	Method**	_group;

	int		_group_n;

};

#endif
