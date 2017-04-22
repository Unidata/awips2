//------------------------------------------------------------------------------
// Solver :: initialize - initialized instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Solver.h"

int Solver :: initialize()
{
	_root	= NULL;
	_isPrimary = 1;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_solver/RCS/Solver_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Solver_initialize.cxx,v 1.2 2006/10/26 15:35:50 hsu Exp $";}
/*  ===================================================  */

}
