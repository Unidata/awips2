//------------------------------------------------------------------------------
// Component :: finalize - cleans up after the time step is completed. 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 18 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

int Component :: finalize( TSDate& cur_date )
{
	char	routine[]="Component :: finalize";
	int i;

	// First, we must call recursively on all of the sons of this
	// component...
	for( i=0; i<_n_son; i++ ){
		if( _son_list[i]->finalizeSolution( cur_date ) ){
			return( STATUS_FAILURE );
		}
	}

	_prev_date = cur_date;
	PrintDebug( 5, routine, "Setting _prev_date to %s.", 
		cur_date.toString() );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_finalize.cxx,v $";
 static char rcs_id2[] = "$Id: Component_finalize.cxx,v 1.3 2006/10/26 15:18:25 hsu Exp $";}
/*  ===================================================  */

}
