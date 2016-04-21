//------------------------------------------------------------------------------
// ExprParser :: iterate - moves to the next character in the string.
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

void ExprParser :: iterate()
{
	char routine[]="ExprParser :: iterate";

	if( _cur_ch != '\0' ){
		_cur_index++;
		_cur_ch = _string[_cur_index];
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ExprParser_iterate.cxx,v $";
 static char rcs_id2[] = "$Id: ExprParser_iterate.cxx,v 1.3 2006/10/26 15:19:51 hsu Exp $";}
/*  ===================================================  */

}
