//------------------------------------------------------------------------------
// ResJSys :: initialize - initialized instance data members.
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
// 29 May 1998	Daniel Weiler, RTi	Declare static _po_list and 
//					_co_list.
// 29 May 1998	MJR		Break static member declaration into the 
//				initializeStatic function file.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"

int ResJSys :: initialize()
{
	_control_file[0] = '\0';
	_root	= NULL;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_initialize.cxx,v 1.2 2006/10/26 15:30:59 hsu Exp $";}
/*  ===================================================  */

}
