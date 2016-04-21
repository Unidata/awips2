//------------------------------------------------------------------------------
// ConstExpr :: evaluate - evaluates the value of a ConstExpr.
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
// 19 Feb 1998	MJR	Moved a lot of this functionality into the 
//			ConstExpr constructor.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ConstExpr.h"

double ConstExpr :: evaluate()
{
//printf( "yyyy ConstExpr :: evaluate constant value= %f \n",*_value );
	if( verify() ){
		return( MISSING );
	}

	return( *_value );
/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ConstExpr_evaluate.cxx,v $";
 static char rcs_id2[] = "$Id: ConstExpr_evaluate.cxx,v 1.3 2006/10/26 15:19:09 hsu Exp $";}
/*  ===================================================  */

}
