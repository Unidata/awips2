//------------------------------------------------------------------------------
// Reach :: finalizeSolution - does solution cleanup at the end of a 
//				   time-step.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 31 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 15 Apr 1998	Daniel Weiler		Added TSDate& argument.
// 19 Feb 2004	JRV, RTi	Replaced calls to sumInflow with calls to 
// 				getTotalInflow.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reach.h"

int Reach :: finalizeSolution( TSDate& cur_date )
{
	char routine[] = "Reach :: finalizeSolution";
	double cur_inflow;
	int i;

	/*
	// Call recursively...
	for( i=0; i<_n_son; i++ ) {
		if( _son_list[i]->finalizeSolution( cur_date ) ){
			return( STATUS_FAILURE );
		}
	}
	*/

	if( _outflow == MISSING ) {
		PrintError( routine, "Outflow on reach '%s' is missing.  "
			"Please ensure a routing method is active." , _id );
		return( STATUS_FAILURE );
	}
	TSIdent tempJRV = _outflow_ts.getIdentifier();
	_outflow_ts.setDataValue( cur_date, _outflow );

	cur_inflow = getTotalInflow( cur_date );
	PrintDebug( 5, routine, "TIME  \"%s\".", cur_date.toString() );
	PrintDebug( 5, routine, "Reach  \"%s\".", _id );
	PrintDebug( 5, routine, "Inflow   %f", cur_inflow );
	PrintDebug( 5, routine, "Outflow   %f", _outflow );
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reach_finalizeSolution.cxx,v $";
 static char rcs_id2[] = "$Id: Reach_finalizeSolution.cxx,v 1.4 2006/10/26 15:30:21 hsu Exp $";}
/*  ===================================================  */

}
