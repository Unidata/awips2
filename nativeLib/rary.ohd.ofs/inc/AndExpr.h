//------------------------------------------------------------------------------
// AndExpr - class representing an "exp1 && exp2".
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
// 05 May 1998	MJR	Added the copy() function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#ifndef AndExpr_INCLUDED
#define AndExpr_INCLUDED

#include "TwoOpExpr.h"

class AndExpr : public TwoOpExpr
{
public:
	AndExpr( Expression*, Expression* );

	Expression* copy();

	double evaluate();

	char* toString();
};

#endif
