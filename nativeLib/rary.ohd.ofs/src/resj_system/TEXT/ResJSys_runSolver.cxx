//------------------------------------------------------------------------------
// ResJSys :: runSolver - this function runs the solver...
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 02 Apr 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "Solver.h"

int ResJSys :: runSolver()
{
	int	ierr;
	char	routine[]="ResJSys :: runSolver";
	Solver	solver;

	ierr= solver.run( _root, _t1, _t2, 
		Method :: getTimeInterval(), Method :: getTimeMult(), 1 );

	if( ierr ){
		PrintWarning( 1, routine, "TROUBLES RUNNING SOLVER." );
		return( STATUS_FAILURE );
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_runSolver.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_runSolver.cxx,v 1.4 2006/10/26 15:31:31 hsu Exp $";}
/*  ===================================================  */

}
