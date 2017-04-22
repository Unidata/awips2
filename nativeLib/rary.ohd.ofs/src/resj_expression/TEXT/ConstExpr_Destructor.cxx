//------------------------------------------------------------------------------
// ConstExpr :: ~ConstExpr - destructor.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ConstExpr.h"

ConstExpr :: ~ConstExpr()
{
	if( _is_local_value && _value ){
		delete _value;
		_value = NULL;
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ConstExpr_Destructor.cxx,v $";
 static char rcs_id2[] = "$Id: ConstExpr_Destructor.cxx,v 1.2 2006/10/26 15:19:02 hsu Exp $";}
/*  ===================================================  */

}
