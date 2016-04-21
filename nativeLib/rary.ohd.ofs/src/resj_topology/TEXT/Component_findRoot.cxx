//------------------------------------------------------------------------------
// Component :: findRoot - traverses upward to find the root component.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 18 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

Component* Component :: findRoot()
{
	char	routine[]="Component :: findRoot";

	if( _father == NULL ){
		return( this );
	}

	return( _father->findRoot() );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_findRoot.cxx,v $";
 static char rcs_id2[] = "$Id: Component_findRoot.cxx,v 1.3 2006/10/26 15:18:28 hsu Exp $";}
/*  ===================================================  */

}
