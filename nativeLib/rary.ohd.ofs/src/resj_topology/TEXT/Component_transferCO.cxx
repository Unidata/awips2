//------------------------------------------------------------------------------
// Component::transferCO - Virtual function to transfer carry over parameters 
//				for a component
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 21 Nov 2001	James R. VanShaar, Riverside Technology, Inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// @params resOLD Component object from old resj system definition (not used 
//	here)
// @params cOLD character string pointing to the portion of the old C array 
//	containing carryover for the active component.
// @params cNEW character string pointing to the portion of the new C array 
//	containing carryover for the active component.
// @returns ierr integer representing errors occuring or significant warnings:
//	0 = no problems
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "Component.h"
#include <string.h>

int Component :: transferCO ( Component * resOLD, char * cOLD, char * cNEW,
	int * ipr )

	// This function simply covers the base for component carryover
	//	which has not been developed at the Reach level.

	// No carry over currently (11/01) exists.  We will simply return an
	//	error as we should not find any true Reach component carry over.
{

	char routine[]="Component::transferCO", message[256];

	sprintf( message, "No carry over should exist for Components other "
		"than RESERVOIR and NODE.  Reach-like carry over is contained "
		"in LagK method carry over only." );
	PrintError ( routine, message );

	return (STATUS_FAILURE);

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_transferCO.cxx,v $";
 static char rcs_id2[] = "$Id: Component_transferCO.cxx,v 1.3 2006/10/26 15:18:53 hsu Exp $";}
/*  ===================================================  */

}

