//------------------------------------------------------------------------------
// ResJSys :: freeDataSpace - delete ResJSys dynamically allocated memory.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 09 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"

int ResJSys :: freeDataSpace()
{
	if( _root ){
		delete _root;
	}
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_freeDataSpace.cxx,v 1.2 2006/10/26 15:30:54 hsu Exp $";}
/*  ===================================================  */

}
