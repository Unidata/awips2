//------------------------------------------------------------------------------
// ExprParser :: parse_expression - parses an expression.
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

Expression* ExprParser :: parse_expression()
{
	char routine[]="ExprParser :: parse_expression";
	Expression	*e = parse_term();

	if( e == NULL ){
		return( NULL );
	}

	PrintDebug( 10, routine,
	"Parsed: \"%s\".", e->toString() );
	
	unpad();

	while( 	!strncmp( &_string[_cur_index], OR, OR_LEN ) ||
		!strncmp( &_string[_cur_index], AND, AND_LEN ) ){
		if( !strncmp( &_string[_cur_index], OR, OR_LEN ) ){
			iterate();
			iterate();
			unpad();
			e = new OrExpr( e, parse_term() );
		}
		else {
			iterate();
			iterate();
			unpad();
			e = new AndExpr( e, parse_term() );
		}
		unpad();
	}
	return( e );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ExprParser_parse_expression.cxx,v $";
 static char rcs_id2[] = "$Id: ExprParser_parse_expression.cxx,v 1.3 2006/10/26 15:19:59 hsu Exp $";}
/*  ===================================================  */

}
