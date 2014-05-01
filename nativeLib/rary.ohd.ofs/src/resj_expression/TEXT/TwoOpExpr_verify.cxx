//------------------------------------------------------------------------------
// TwoOpExpr :: verify - verifies each of the individual expressions.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "TwoOpExpr.h"

int TwoOpExpr :: verify()
{
	if( _left_op ){
		if( _left_op->verify() ){
			return( STATUS_FAILURE );
		}
	}
	if( _right_op ){
		if( _right_op->verify() ){
			return( STATUS_FAILURE );
		}
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/TwoOpExpr_verify.cxx,v $";
 static char rcs_id2[] = "$Id: TwoOpExpr_verify.cxx,v 1.4 2006/10/26 15:37:22 hsu Exp $";}
/*  ===================================================  */

}
