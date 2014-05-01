//------------------------------------------------------------------------------
// ExprParser :: parseStringList - parses a string list.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 02 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ExprParser.h"

Expression* ExprParser :: parseStringList( const char** list, const int nlist )
{
	char	routine[]="ExprParser :: parseStringList", *string=NULL;
	int	len=0;

	for( int i=0; i<nlist; i++ ){
		if( string == NULL ){
			len = 0;
		}
		else {
			len = strlen( string );
		}

		string = (char*)realloc( string, 
		(len+strlen(list[i])+1)*sizeof( char ) );

		if( len == 0 ){
			strcpy( string, list[i] );
		}
		else {
			strcat( string, list[i] );
		}
	}

	Expression* exp = parseString( string );

	if( string ){
		free( string );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return( exp );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ExprParser_parseStringList.cxx,v $";
 static char rcs_id2[] = "$Id: ExprParser_parseStringList.cxx,v 1.4 2006/10/26 15:19:56 hsu Exp $";}
/*  ===================================================  */

}
