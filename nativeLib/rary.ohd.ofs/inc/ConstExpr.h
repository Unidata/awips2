//------------------------------------------------------------------------------
// ConstExpr - base class for rules expressions.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This class basically defines an interface for expressions.
//------------------------------------------------------------------------------
// History:
// 
// 15 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 20 Feb 1998	MJR	Added the verify function.
// 05 May 1998	MJR	Added the copy() function.
//------------------------------------------------------------------------------
#ifndef ConstExpr_INCLUDED
#define ConstExpr_INCLUDED

#include "Expression.h"

class ConstExpr : public Expression
{
public:
	ConstExpr( char* );		// Create a constant expression.

	~ConstExpr();			// Destroy a constant expression.

	Expression* copy();		// Copy the expression.

	virtual double evaluate( ); 	// This will evaluate the expression.
					// The integer will receive a flag that
					// tells what the returned string 
					// contains (ie. BOOLEAN, VALUE,
					// VARIABLE) so that the calling 
					// function can use this information
					// to make some decision.
	char* toString();		// Prints the constant as a string.

	virtual int verify();		// Verifies that the constants do 
					// evalutate.
private:
	void	initialize();		// Initializes private data members.

	char	_constant[MAXC];	// String to hold constant or 
					// variable name.
	double	*_value;		// The actual value of the constant.

	int	_is_local_value,	// Flag to tell if the value in
					// _value is allocated locally.
		_is_verified;		// Flag that gets set when an expression
					// evaluates successfully.
};

#endif
