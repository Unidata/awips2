//------------------------------------------------------------------------------
// OrExpr - class representing an "exp1 || exp2".
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 15 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 05 May 1998	MJR		Added the copy() function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#ifndef OrExpr_INCLUDED
#define OrExpr_INCLUDED

#include "TwoOpExpr.h"

class OrExpr : public TwoOpExpr
{
public:
	OrExpr( Expression*, Expression* );

	Expression* copy();

	double evaluate();

	char* toString();
};

#endif
