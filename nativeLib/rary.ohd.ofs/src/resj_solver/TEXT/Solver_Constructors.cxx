//------------------------------------------------------------------------------
// Solver :: Solver - Constructors.
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

Solver :: Solver()
{
	initialize();
}

Solver :: Solver( const Solver& rjs )
{
	char	routine[]="Solver :: Solver";

	initialize();

	PrintWarning( 1, routine,
	"The \"%s\" routine is not currently enabled.", routine );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_solver/RCS/Solver_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Solver_Constructors.cxx,v 1.3 2006/10/26 15:35:32 hsu Exp $";}
/*  ===================================================  */

}
