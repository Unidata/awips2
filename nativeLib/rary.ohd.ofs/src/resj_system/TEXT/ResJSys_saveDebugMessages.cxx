//------------------------------------------------------------------------------
// ResJSys :: saveDebugMessages - stores debug messages in a stringl
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

int ResJSys :: saveDebugMessages( int dl, char* routine, char* message )
{
	if( !message ){
		return( STATUS_FAILURE );
	}
	_debug_str = (char*)realloc( _debug_str,
	(_debug_str_len + strlen( message ) + 1) * sizeof( char ) );

	if( !_debug_str ){
		// We can't print a message here since will go into an
		// infinite loop.
		return( STATUS_FAILURE );
	}
	_debug_str[_debug_str_len] = '\0';
	_debug_str_len += strlen( message );
	strcat( _debug_str, message );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_saveDebugMessages.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_saveDebugMessages.cxx,v 1.2 2006/10/26 15:31:33 hsu Exp $";}
/*  ===================================================  */

}
