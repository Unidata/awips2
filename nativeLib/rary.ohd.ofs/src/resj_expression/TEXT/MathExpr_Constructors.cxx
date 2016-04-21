//------------------------------------------------------------------------------
// MathExpr :: MathExpr - Constructor.
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

MathExpr :: MathExpr( Expression* e1, char mathSymbol, Expression* e2 )
	: TwoOpExpr( e1, e2 )
{
	_mathSymbol = mathSymbol;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/MathExpr_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: MathExpr_Constructors.cxx,v 1.2 2006/10/26 15:25:33 hsu Exp $";}
/*  ===================================================  */

}
