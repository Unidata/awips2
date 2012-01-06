//------------------------------------------------------------------------------
// MathExpr :: copy - recursively copies the expression.
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

Expression* MathExpr :: copy()
{
	char		routine[]="MathExpr :: copy()";

	Expression	*ept=NULL, *lept=NULL, *rept=NULL;

	lept = _left_op->copy();

	if( !lept ){
		return( NULL );
	}

	rept = _right_op->copy();

	if( !rept ){
		if( lept ){
			delete lept;
		}
		return( NULL );
	}

	ept = new MathExpr( lept, _mathSymbol, rept );

	if( !ept ){
		PrintWarning( 1, routine,
		"Unable to copy expression \"%s\".", toString() );
		delete lept;
		delete rept;
		return( NULL );
	}

	return( ept );


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/MathExpr_copy.cxx,v $";
 static char rcs_id2[] = "$Id: MathExpr_copy.cxx,v 1.2 2006/10/26 15:25:36 hsu Exp $";}
/*  ===================================================  */

}
