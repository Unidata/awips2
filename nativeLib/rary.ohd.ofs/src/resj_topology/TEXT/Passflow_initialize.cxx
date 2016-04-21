//------------------------------------------------------------------------------
// Passflow :: initialize - initialized instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
//  23 AUG 2004	KSH, OHD	Added this method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Passflow.h"

int Passflow :: initialize()
{
	strcpy( _type, "PASSFLOW" );
	_group_id = RELEASE_METHOD;

	_hasStates = 0;

	return( STATUS_SUCCESS );


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Passflow_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Passflow_initialize.cxx,v 1.2 2006/10/26 15:28:59 hsu Exp $";}
/*  ===================================================  */

}
