//------------------------------------------------------------------------------
// ExprParser - Class to encapsulate the parsing of a regular expression string.
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
// 07 Jun 2002	James R. VanShaar, RTi	Added MathExpr.
//				
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#ifndef ExprParser_INCLUDED
#define ExprParser_INCLUDED

#include "AndExpr.h"
#include "ConstExpr.h"
#include "EqualsExpr.h"
#include "GreaterThanEqualsExpr.h"
#include "GreaterThanExpr.h"
#include "LessThanEqualsExpr.h"
#include "LessThanExpr.h"
#include "MathExpr.h"
#include "MinusExpr.h"
#include "NotEqualsExpr.h"
#include "OrExpr.h"

class ExprParser 
{
public:
	ExprParser();		// Constructor

	~ExprParser();		// Destructor.

	Expression* parseString( const char* );
				// Main parsing routine.
	Expression* parseStringList( const char**, const int );
				// Main routine for parsing a string list.
private:
	Expression* error( const char*, const char* );
				// Prints an error message and points to
				// the message.
	void iterate();		// Moves the pointer to the next character.

	Expression* parse_expression();

	Expression* parse_factor();

	Expression* parse_term();

	void unpad();		// Removes whitespace at the current
				// position.

	char		_cur_ch;// Current character in the string.

	char	_string[5000];	// String that is being parsed.

	int 		_cur_index;
				// Current index in the string.
};

#endif
