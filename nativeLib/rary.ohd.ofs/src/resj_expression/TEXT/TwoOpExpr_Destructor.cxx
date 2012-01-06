//------------------------------------------------------------------------------
// TwoOpExpr :: ~TwoOpExpr - Destructor.
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
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "TwoOpExpr.h"

TwoOpExpr :: ~TwoOpExpr()
{
	if( _left_op ){
		delete _left_op;
	}
	if( _right_op ){
		delete _right_op;
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/TwoOpExpr_Destructor.cxx,v $";
 static char rcs_id2[] = "$Id: TwoOpExpr_Destructor.cxx,v 1.2 2006/10/26 15:37:15 hsu Exp $";}
/*  ===================================================  */

}
