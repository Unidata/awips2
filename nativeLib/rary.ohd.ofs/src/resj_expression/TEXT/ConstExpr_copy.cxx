//------------------------------------------------------------------------------
// ConstExpr :: copy - copies a constant expression.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 05 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ConstExpr.h"

Expression* ConstExpr :: copy()
{
	char	routine[]="ConstExpr :: copy()";

	Expression* ept = NULL;

	ept = new ConstExpr( _constant );

	if( !ept ){
		PrintWarning( 1, routine,
		"Unable to allocate a constant expression for \"%s\".",
		_constant );
		return( NULL );
	}
	return( ept );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ConstExpr_copy.cxx,v $";
 static char rcs_id2[] = "$Id: ConstExpr_copy.cxx,v 1.3 2006/10/26 15:19:05 hsu Exp $";}
/*  ===================================================  */

}
