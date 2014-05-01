//------------------------------------------------------------------------------
// MathExpr - class representing an the result of a two term mathematical
//		expression.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 07 Jun 2002	James R. VanShaar, Riverside Technology, inc
//				Created initial version based on MinusExpr.h
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#ifndef MathExpr_INCLUDED
#define MathExpr_INCLUDED

#include "TwoOpExpr.h"

class MathExpr : public TwoOpExpr
{
public:
	MathExpr( Expression*, char, Expression* );

	Expression* copy();

	double evaluate();

	char* toString();

	char	_mathSymbol;	// Mathematical symbol.
};

#endif
