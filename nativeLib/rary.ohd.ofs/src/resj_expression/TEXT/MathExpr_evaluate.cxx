//------------------------------------------------------------------------------
// MathExpr :: evaluate - evaluates the result of a two piece mathematical 
//				expression.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 07 Jun 2002	James R. VanShaar, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "MathExpr.h"

double MathExpr :: evaluate()
{
	if ( _mathSymbol == '+' ) {
		return( (double)(_left_op->evaluate() + _right_op->evaluate()));
	}
	if ( _mathSymbol == '-' ) {
		return( (double)(_left_op->evaluate() - _right_op->evaluate()));
	}

	// Otherwise we are in some type of trouble

	return( MISSING );


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/MathExpr_evaluate.cxx,v $";
 static char rcs_id2[] = "$Id: MathExpr_evaluate.cxx,v 1.2 2006/10/26 15:25:38 hsu Exp $";}
/*  ===================================================  */

}
