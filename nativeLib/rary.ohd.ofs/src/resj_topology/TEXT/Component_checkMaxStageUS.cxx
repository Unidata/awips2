//------------------------------------------------------------------------------
// Component :: checkMaxStageUS - checks for MaxStage method upstrem of "this". 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)We want to check as many sons as possible even if there are
//		some failures.
//------------------------------------------------------------------------------
// History:
// 
// 06 May 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

int Component :: checkMaxStageUS( Component* ucp )
{
	char	routine[]="Component :: checkMaxStageUS";
	int i;

	// First check to see if "this" is the upstream point.
	if( this == ucp ) {
		return( STATUS_SUCCESS );
	}
	
	for( i = 0; i < _n_son; i++ ){
		if( _son_list[i]->checkMaxStageUS( ucp ) == STATUS_SUCCESS ) {	
			return( STATUS_SUCCESS );
		}
	}

	// If we get to here, this means that no MaxStage methods were detected
	return( STATUS_FAILURE );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_checkMaxStageUS.cxx,v $";
 static char rcs_id2[] = "$Id: Component_checkMaxStageUS.cxx,v 1.3 2006/10/26 15:18:12 hsu Exp $";}
/*  ===================================================  */

}
