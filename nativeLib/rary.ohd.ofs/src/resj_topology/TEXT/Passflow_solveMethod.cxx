//------------------------------------------------------------------------------
// Passflow :: solveMethod - Algorithm that solves the Passflow method.
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

int Passflow :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[]="Passflow::solveMethod";
	double	endInflow, endRelease;
	double	prev_release = 0.0, new_rel = 0.0;
	TSDate	idate;

	// Set _Active to 1
	_Active = 1;
	endInflow = _owner->sumInflow( cur_date );
	endRelease = endInflow;

	_owner->_release = endRelease;

	return( STATUS_SUCCESS );


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Passflow_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: Passflow_solveMethod.cxx,v 1.2 2006/10/26 15:29:04 hsu Exp $";}
/*  ===================================================  */

}
