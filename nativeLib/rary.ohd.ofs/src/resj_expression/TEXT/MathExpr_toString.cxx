//------------------------------------------------------------------------------
// MathExpr :: toString - prints out an - expression.
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

char* MathExpr :: toString()
{
	if( _left_op && _right_op ){
		sprintf( _string, "( %s %c %s )", _left_op->toString(), 
		_mathSymbol, _right_op->toString() );
	}
	return( _string );


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/MathExpr_toString.cxx,v $";
 static char rcs_id2[] = "$Id: MathExpr_toString.cxx,v 1.2 2006/10/26 15:25:44 hsu Exp $";}
/*  ===================================================  */

}
