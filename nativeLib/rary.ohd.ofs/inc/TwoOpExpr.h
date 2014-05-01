//------------------------------------------------------------------------------
// TwoOpExpr - base class for rules expressions.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This class basically defines an interface for expressions.
//------------------------------------------------------------------------------
// History:
// 
// 15 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 20 Feb 1998	MJR	Added the verify function to chain the checking of 
//			the expression.
//------------------------------------------------------------------------------
#ifndef TwoOpExpr_INCLUDED
#define TwoOpExpr_INCLUDED

#include "Expression.h"

class TwoOpExpr : public Expression
{
public:
	TwoOpExpr( Expression*, Expression* );

	virtual ~TwoOpExpr();	// This forces chaining of the 
				// Destructors.
	virtual int verify();	// Verifies each of the individual exprs.
protected:
	char		_string[5000];	// String used for printing the 
					// expressions.
	Expression	*_left_op,	// The left hand operand of the 
					// two operand expression.
			*_right_op;	// The right hand operand of the 
					// two operand expression.
private:
	void initialize();		// Initializes private data members.
};

#endif
