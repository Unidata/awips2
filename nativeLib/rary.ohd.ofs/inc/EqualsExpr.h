//------------------------------------------------------------------------------
// EqualsExpr - class representing an "exp1 == exp2".
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
#ifndef EqualsExpr_INCLUDED
#define EqualsExpr_INCLUDED

#include "TwoOpExpr.h"

class EqualsExpr : public TwoOpExpr
{
public:
	EqualsExpr( Expression*, Expression* );

	Expression* copy();

	double evaluate();

	char* toString();
};

#endif
