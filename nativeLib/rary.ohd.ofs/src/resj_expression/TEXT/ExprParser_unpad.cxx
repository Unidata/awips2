//------------------------------------------------------------------------------
// ExprParser :: unpad - skips over ' ' characters.
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
#include "ExprParser.h"

void ExprParser :: unpad()
{
	while( isspace( _cur_ch ) ){
		iterate();
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ExprParser_unpad.cxx,v $";
 static char rcs_id2[] = "$Id: ExprParser_unpad.cxx,v 1.2 2006/10/26 15:20:10 hsu Exp $";}
/*  ===================================================  */

}
