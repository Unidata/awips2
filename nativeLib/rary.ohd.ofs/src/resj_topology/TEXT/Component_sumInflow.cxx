//------------------------------------------------------------------------------
// Component::sumInflow 	Sums the inflow from the _inflow list.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 06 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

double Component::sumInflow( TSDate& date )
{
	char routine[]="Component::sumInflow";
	double	in_value = 0.0, accum_value = 0.0;
	int i, imax = _n_son;

	// Loop thorough the number of inflows, which should be the same as 
	// the number of sons, and return the sum. If the number of inflows
	// is not the same as the number of sons, we will sum the first
	// _in_count number of inflow values.
	for( i = 0; i < _in_count; i++ ) {
		if( _inflow_ts[i] == NULL ) {
			PrintWarning( 1, routine, "Troubles summing inflow for "
			"%s.", _id );
			return( MISSING );
		}

		TSIdent tempJRV = _inflow_ts[i]->getIdentifier();

		in_value = _inflow_ts[i]->getDataValue( date ); 

		if( in_value == MISSING ) {
			return( MISSING );
		}
		else if( in_value < 0 ) {
			accum_value += 0;
		}
		else {
			accum_value += in_value;
		}
	}
	return( accum_value );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_sumInflow.cxx,v $";
 static char rcs_id2[] = "$Id: Component_sumInflow.cxx,v 1.4 2006/10/26 15:18:46 hsu Exp $";}
/*  ===================================================  */

}
