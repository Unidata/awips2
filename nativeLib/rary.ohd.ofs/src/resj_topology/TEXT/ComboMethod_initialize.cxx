//------------------------------------------------------------------------------
// ComboMethod :: initialize - initializes data members 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 22 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ComboMethod.h"

int ComboMethod :: initialize()
{
	char routine[] = "ComboMethod :: initialize";
	int i;
	_group = new Method*[100];
	_group_n = 0;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/ComboMethod_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: ComboMethod_initialize.cxx,v 1.3 2006/10/26 15:12:44 hsu Exp $";}
/*  ===================================================  */

}
