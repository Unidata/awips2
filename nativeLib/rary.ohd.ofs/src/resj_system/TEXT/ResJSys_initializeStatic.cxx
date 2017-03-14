//------------------------------------------------------------------------------
// ResJSys :: initializeStatic - declares and initializes static data members.
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
// 08 May 2001  James R. VanShaar, RTi	Added static members _ipr and _iodebug
// 09 May 2001	JRV, RTi	Introduced multiple calls to setOutputFunctions
//				to account for change in that function
// 10 May 2001	JRV, RTi	Assigned DEBUG_INDEX for use with debug, status;
//				WARNING_INDEX for use with warning, error
// 07 Feb 2006	JRV, RTi	Added _mainum static initialization.
// 18 Feb 2006	JRV, RTi	Added initialization of _writeLastCO.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"

int ResJSys :: initializeStatic()
{
	_co_list		= NULL;
	_co_size		= 0;
	_debug_str		= NULL;
	_debug_str_len		= 0;
	_po_list		= NULL;
	_po_size		= 0;
	_warning_str		= NULL;
	_warning_str_len 	= 0;
	_num_co_str		= 0;
	_num_co_dates		= 0;
	_co_array_str		= NULL;
	_writeLastCO 		= 0;

	// Now we have to register the functions. It doesn't really matter
	// if we do this multiple times.
	SetOutputFunctions( ResJSys :: saveWarningMessages, DEBUG_INDEX,
		"DEBUG" );
	SetOutputFunctions( &PrintNWSError, WARNING_INDEX, "ERROR" );
	SetOutputFunctions( ResJSys :: saveWarningMessages, DEBUG_INDEX,
		"STATUS" );
	SetOutputFunctions( &PrintNWSWarning, WARNING_INDEX, "WARNING" );

	// Initialize warning, debug levels to default case
	SetDebugLevel( 0, DEBUG_INDEX);
	SetStatusLevel( 0, DEBUG_INDEX);
	SetWarningLevel( 1, WARNING_INDEX);
	//SetWarningLevel( WARNING_INDEX, 1 );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_initializeStatic.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_initializeStatic.cxx,v 1.4 2006/10/26 15:31:02 hsu Exp $";}
/*  ===================================================  */

}
char* 	ResJSys :: _co_list 		= NULL;
int 	ResJSys :: _co_size		= 0;
char* 	ResJSys :: _debug_str		= NULL;
int 	ResJSys :: _debug_str_len	= 0;
char* 	ResJSys :: _po_list		= NULL;
int 	ResJSys :: _po_size		= 0;
int 	ResJSys :: _ref_count		= 0;
char* 	ResJSys :: _warning_str		= NULL;
int 	ResJSys :: _warning_str_len	= 0;
int 	ResJSys :: _num_co_str		= 0;
int 	ResJSys :: _num_co_dates	= 0;
char** 	ResJSys :: _co_array_str	= NULL;

// New: JRV
int	ResJSys :: _ipr			= 0;
int	ResJSys :: _iodebug		= 0;
int     ResJSys :: _TSdefined           = 0;
int     ResJSys :: _mainum              = 0;
int     ResJSys :: _writeLastCO         = 0;

