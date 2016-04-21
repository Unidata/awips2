//------------------------------------------------------------------------------
// ConstExpr :: initialize - initializes private data members.
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
#include "ConstExpr.h"

void ConstExpr :: initialize()
{
	// We need to initialize the constant to zero.
	strcpy( _constant, "0" );

	_is_local_value = 0;

	_is_verified = 0;

	_value = NULL;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ConstExpr_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: ConstExpr_initialize.cxx,v 1.2 2006/10/26 15:19:12 hsu Exp $";}
/*  ===================================================  */

}
