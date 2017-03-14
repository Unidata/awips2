//------------------------------------------------------------------------------
// ExprParser :: error - prints an error message.
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

Expression* ExprParser :: error( const char* routine, const char* message )
{
	char	tmp[MAXC]="";

	PrintWarning( 1, (char*)routine, 
	"Parse error while parsing: %s", _string );

	for( int ctr=0; ctr<_cur_index; ctr++ ){
		strcat( tmp, " " );
	}

	PrintWarning( 1, (char*)routine, 
	"                 location: %s^", tmp );

	PrintWarning( 1, (char*)routine, 
	"                    error: %s", message );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return( NULL );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ExprParser_error.cxx,v $";
 static char rcs_id2[] = "$Id: ExprParser_error.cxx,v 1.3 2006/10/26 15:19:45 hsu Exp $";}
/*  ===================================================  */

}
