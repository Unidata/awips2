//------------------------------------------------------------------------------
// ExprParser::parse_factor() - parses out a factor.
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
// 06 Jun 2002  JRV, RTi        Added cfactor adjustment for inflows, 
//				withdrawals and expanded its capability for all 
//				pools, and releases.
// 07 Jun 2002	JRV, RTi	Added MathExpr handling and removed reset of
//				Component::cfactor when no unit type found.
// 10 Jun 2002	JRV, RTi	Added cfactor adjustment for node DISCHARGE.
// 01 Jul 2002	JRV, RTi	Fixed problem with strcmp and NULL tok.
// 31 Oct 2003	JRV, RTi	Modified to free memory completely.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ExprParser.h"
#include "Component.h"

Expression* ExprParser::parse_factor()
{
	double cfactor;
        char    **list=NULL, routine[]="ExprParser::parseFactor";
        int     nlist=0;

	Expression 	*e;

	if( _cur_ch == '-' ){
		iterate();
		unpad();
		e = new MinusExpr( new ConstExpr( "0" ), parse_factor() );
	}
	else if( _cur_ch == '(' ){
		/*
		iterate();
		unpad();
		e = parse_expression();
		if( _cur_ch != ')' ){
			//return( error( routine, "Expected ')'." ) );
		}
		iterate();
		*/
		iterate();
		int paren_cnt = 1;
		int sind = _cur_index;
		while( paren_cnt != 0 && _cur_ch != '\0' ){
			if( _cur_ch == ')' ){
				paren_cnt--;
			}
			if( _cur_ch == '(' ){
				paren_cnt++;
			}
			iterate();
		}
		if( paren_cnt != 0 ){
			return( error( routine, "Unbalanced parentheses!" ) );
		}
		char	tmp_str[MAXC];

		strncpy( tmp_str, &_string[sind], (_cur_index-sind-1) );

		tmp_str[_cur_index-sind-1] = '\0';

		UnpadString( tmp_str, " ", PAD_FRONT_BACK );

		ExprParser *ep = new ExprParser();

		if( !ep ){
			PrintWarning( 1, routine,
			"Unable to allocate 1 ExprParser()" );
			return( NULL );
		}

		e = ep->parseString( tmp_str );

		delete ep;
	}
	else {
		// We have a number, a date string, or a variable name...
		char *end, tmp[MAXC], const_str[MAXC];

		// We need to get the first token so that we can test if this 
		// is a date string.
		strcpy( tmp, &_string[_cur_index] );
		char *tok = strtok( tmp, " \t\n" );

		cfactor = Component::_cfactor;

		// This next command will set end to the string just after a
		// leading double, if the first term is a double.
		// It also converts the units of the double.
		double val = strtod( &_string[_cur_index], &end ) * cfactor;

		list = BreakStringList( tmp, ".", DELIM_SKIP_BLANKS, &nlist );
		if( nlist > 1 ) {
			// Convert list[1] to uppercase to not complicate string
			// comparisons
			ToUpper( list[1] );
			if( NULL != strstr( list[1], "POOL" ) ) {
				Component::_cfactor = Component::_lfactor;
			}
			else if( NULL != strstr( list[1], "INFLOW" ) ) {
				Component::_cfactor = Component::_ffactor;
			}
			else if( NULL != strstr( list[1], "DISCHARGE" ) ) {
				Component::_cfactor = Component::_ffactor;
			}
			else if( NULL != strstr( list[1], "RELEASE" ) ) {
				Component::_cfactor = Component::_ffactor;
			}
			else if( NULL != strstr( list[1], "WITHDRAW" ) ) {
				Component::_cfactor = Component::_ffactor;
			}
			else {
				//Component::_cfactor = 1.0;
			}	
			list = FreeStringList( list );
		}
		else {
			list = FreeStringList( list );
		}

	
		char	delims[MAXC];
		sprintf( delims, " \t\n%s%s%s%s%s%s", AND, OR, EQUALS, GT, LT, 
			NE );

		if( end == &_string[_cur_index] || strchr( tok, '/' ) ){
			// This means that we didn't parse out a number, so
			// we have to get the next token separated by 
			// white-space.
			strcpy( tmp, &_string[_cur_index] );

			char* tmp2 = strtok( tmp, delims );

			if( tmp2 == NULL ){
				error( routine, "Variable name expected." );
			}
			strcpy( const_str, tmp2 );

			_cur_index = _cur_index+strlen( const_str );

			_cur_ch    = _string[_cur_index];
		}
		else {
			sprintf(const_str, "%f", val );

			_cur_index = end - _string;

			_cur_ch = _string[_cur_index];
		}

		// Check if this is a MathExpr.  
		// Currently only add and subtract are available.
		unpad();
		strcpy( tmp, &_string[_cur_index] );
		tok = strtok( tmp, delims );
		// Check if the token equals a math symbol.  The if tok is longer than
		// one character (negative something) it will also return false.
		if ( tok == NULL ) {
			// This is not a MathExpr.
			e = new ConstExpr( const_str );
		}
		else {
			if ( !strcmp(tok, "-") || !strcmp(tok, "+") ) {
				iterate();
				unpad();
				e = new MathExpr( new ConstExpr( const_str ), tok[0], 
					parse_factor() );
			}
			else {
				// This is not a MathExpr.
				e = new ConstExpr( const_str );
			}
		}
	}

	return( e );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ExprParser_parse_factor.cxx,v $";
 static char rcs_id2[] = "$Id: ExprParser_parse_factor.cxx,v 1.5 2006/10/26 15:20:03 hsu Exp $";}
/*  ===================================================  */

}
