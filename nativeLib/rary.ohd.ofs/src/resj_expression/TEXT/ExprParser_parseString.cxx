//------------------------------------------------------------------------------
// ExprParser :: parseString - parses an expression string.
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

Expression* ExprParser :: parseString( const char* string )
{
	char routine[]="ExprParser :: parseString";

	strcpy( _string, string );

	PrintDebug( 10, routine,
	"Parsing: \"%s\".", _string );

	_cur_index	= 0;

	_cur_ch		= _string[_cur_index];

	Expression *expr = parse_expression();

	// If it failed somewhere underneath here we just return null.
	if( expr == NULL ){
		return( NULL );
	}

	unpad();

	if( _cur_ch != '\0' ){
		delete expr;
		return( error( routine, "Extra characters at end of string." ));
	}

	PrintDebug( 10, routine,
	"Parsed \"%s\" to \"%s\".", string, expr->toString() );

	_cur_index	= 0;
	_cur_ch		= '\0';

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return( expr );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ExprParser_parseString.cxx,v $";
 static char rcs_id2[] = "$Id: ExprParser_parseString.cxx,v 1.4 2006/10/26 15:19:54 hsu Exp $";}
/*  ===================================================  */

}
