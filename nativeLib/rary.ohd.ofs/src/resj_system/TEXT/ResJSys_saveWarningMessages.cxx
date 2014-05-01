//------------------------------------------------------------------------------
// ResJSys :: saveWarningMessages - stores warning messages in a string.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 29 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"

int ResJSys :: saveWarningMessages( int dl, char* routine, char* message )
{
	if( !message ){
		return( STATUS_FAILURE );
	}
	_warning_str = (char*)realloc( _warning_str,
	(_warning_str_len + strlen( message ) + 2) * sizeof( char ) );

	if( !_warning_str ){
		// We can't print a message here since will go into an
		// infinite loop.
		return( STATUS_FAILURE );
	}
	_warning_str[_warning_str_len] = '\0';
	_warning_str_len += (strlen( message ) + 1);
	strcat( _warning_str, message );
	strcat( _warning_str, "\n" );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_saveWarningMessages.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_saveWarningMessages.cxx,v 1.2 2006/10/26 15:31:36 hsu Exp $";}
/*  ===================================================  */

}
