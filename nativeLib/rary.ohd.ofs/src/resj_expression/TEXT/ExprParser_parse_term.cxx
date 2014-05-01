//------------------------------------------------------------------------------
// ExprParser :: parse_term - parses a term.
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

Expression* ExprParser :: parse_term()
{
	char routine[]="ExprParser :: parse_term";
	Expression *e = parse_factor();

	if( e == NULL ){
		return( NULL );
	}

	PrintDebug( 10, routine,
	"Parsed: \"%s\".", e->toString() );

	unpad();

	while(	!strncmp( &_string[_cur_index], LTE, LTE_LEN ) ||
		!strncmp( &_string[_cur_index], LT, LT_LEN ) ||
		!strncmp( &_string[_cur_index], GTE, GTE_LEN ) ||
		!strncmp( &_string[_cur_index], GT, GT_LEN ) ||
		!strncmp( &_string[_cur_index], EQUALS, EQUALS_LEN ) ||
		!strncmp( &_string[_cur_index], NE, NE_LEN ) ){

		if( !strncmp( &_string[_cur_index], LTE, LTE_LEN ) ){
			iterate();
			iterate();
			unpad();
			e = new LessThanEqualsExpr( e, parse_factor() );
		}
		else if( !strncmp( &_string[_cur_index], LT, LT_LEN ) ){
			iterate();
			unpad();
			e = new LessThanExpr( e, parse_factor() );
		}
		else if( !strncmp( &_string[_cur_index], GTE, GTE_LEN ) ){
			iterate();
			iterate();
			unpad();
			e = new GreaterThanEqualsExpr( e, parse_factor() );
		}
		else if( !strncmp( &_string[_cur_index], GT, GT_LEN ) ){
			iterate();
			unpad();
			e = new GreaterThanExpr( e, parse_factor() );
		}
		else if( !strncmp( &_string[_cur_index], EQUALS, EQUALS_LEN ) ){
			iterate();
			iterate();
			unpad();
			e = new EqualsExpr( e, parse_factor() );
		}
		else if( !strncmp( &_string[_cur_index], NE, NE_LEN ) ){
			iterate();
			iterate();
			unpad();
			e = new NotEqualsExpr( e, parse_factor() );
		}
		unpad();
	}
	return( e );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ExprParser_parse_term.cxx,v $";
 static char rcs_id2[] = "$Id: ExprParser_parse_term.cxx,v 1.3 2006/10/26 15:20:06 hsu Exp $";}
/*  ===================================================  */

}
