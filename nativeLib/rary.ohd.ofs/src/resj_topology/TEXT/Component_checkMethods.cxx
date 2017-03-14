//------------------------------------------------------------------------------
// Component :: checkMethods - 	checks that all the methods have been 
//				constructed.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)We want to check as many sons as possible even if there are
//		some failures.
//------------------------------------------------------------------------------
// History:
// 
// 04 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

int Component :: checkMethods()
{
	char	routine[]="Component :: checkMethods";
	int	i, status=STATUS_SUCCESS;

	// First we need to check the sons

	for( i=0; i<_n_son; i++ ){
		if( _son_list[i]->checkMethods() ){
			status = STATUS_FAILURE;
		}
	}
	// Now we have to check the methods in this one...

	for( i=0; i<_n_meth; i++ ){
		if( !_method_list[i]->isConstructed() ){
			PrintWarning( 1, routine,
"%s Method %s on %s (%s) has not been successfully constructed.",
			_method_list[i]->getType(), _method_list[i]->getID(), _id, _type );
			status = STATUS_FAILURE;
		}
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return( status );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_checkMethods.cxx,v $";
 static char rcs_id2[] = "$Id: Component_checkMethods.cxx,v 1.5 2006/10/26 15:18:15 hsu Exp $";}
/*  ===================================================  */

}
