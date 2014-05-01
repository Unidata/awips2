//------------------------------------------------------------------------------
// Expression - base class for rules expressions.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This class basically defines an interface for expressions.
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 19 Feb 1998	MJR	Added the static function getConstPtr()
// 20 Feb 1998	MJR	Put in the verify function to allow for a separate
//			method of checking an expression tree.
// 02 Mar 1998	MJR	Put in support for the DATE macro and comparison of
//			dates.
// 05 May 1998	MJR		Added the copy() function.
//------------------------------------------------------------------------------
#ifndef Expression_INCLUDED
#define Expression_INCLUDED

#include "ResJ.h"
#include "resj/TSDate.h"

// defines for the various strings...

#define AND "&&"
#define AND_LEN 2
#define EQUALS "=="
#define EQUALS_LEN  2
#define GTE ">="
#define GTE_LEN 2
#define GT ">"
#define GT_LEN 1
#define LTE "<="
#define LTE_LEN 2
#define LT "<"
#define LT_LEN 1
#define MINUS "-"
#define MINUS_LEN 1
#define NE "!="
#define NE_LEN 2
#define OR "||"
#define OR_LEN 2

// Do a forward declaration of the Component class here to avoid a
// circular reference to the class.

class Component;

class Expression
{
public:
	virtual Expression* copy() = 0; // Function to recursively copy
					// an expression tree.
	virtual ~Expression();		// This forces chaining of the 
					// Destructors.
	virtual double evaluate( ) = 0;	
					// This will evaluate the expression.
					// The integer will receive a flag that
					// tells what the returned string 
					// contains (ie. BOOLEAN, VALUE,
					// VARIABLE) so that the calling 
					// function can use this information
					// to make some decisions.
	static double* getConstantPtr( char* );
					// This routine does a lookup on the 
					// component tree for a variable.
	void print( FILE* );		// Prints the expression to a file.

	static int setComponentRoot( Component* );

	static int setCurrentDate( TSDate& );
					// Routine which sets the value for the
					// current date during solution.

	virtual char* toString() = 0;	// Routine used to print out the 
					// expressions.
	virtual int verify() = 0;	// Routine to verify that all the 
					// values/variables used in the 
					// expression are able to be looked up.
protected:
	static double		_current_date_value;
				// Double representation of the current date.
private:
	virtual void initialize() = 0;

	static Component	*_component_root;
					// Pointer to the root of the component
					// tree, this is used when doing a
					// lookup for the identifier.
};

#endif
