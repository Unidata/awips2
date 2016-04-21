//------------------------------------------------------------------------------
// ResJSys :: ~ResJSys - destructor.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 06 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 29 May 1998	MJR	Put in reference counting logic and static destruction.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"

ResJSys :: ~ResJSys()
{
	freeDataSpace();
	--_ref_count;
	if( !_ref_count ){
		freeDataSpaceStatic();
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_Destructor.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_Destructor.cxx,v 1.2 2006/10/26 15:30:41 hsu Exp $";}
/*  ===================================================  */

}
