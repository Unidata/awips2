//------------------------------------------------------------------------------
// Node :: freeDataSpace - deletes the whole tree.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 16 Feb 2006	James R. VanShaar, RTi	Commented out unused stage work.
//                                      Added deletion of _diversion_ts.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Node.h"

int Node :: freeDataSpace()
{
    char routine[]="Node :: freeDataSpace";

/*****************************
// This section is unnecessary since as of 2006-02-16 no stage calculations
// are done.
//	if( _stage_ts ) {
//		delete _stage_ts;
//	}
*****************************/
    if( _diversion_ts ) {
        delete _diversion_ts;
    }

    return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Node_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Node_freeDataSpace.cxx,v 1.3 2006/10/26 15:28:01 hsu Exp $";}
/*  ===================================================  */

}
