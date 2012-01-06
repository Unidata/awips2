//------------------------------------------------------------------------------
// Component :: verifyExpressions() - recursively verifies the expressions.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 06 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 07 May 1998	Daniel Weiler, RTi	Checked _expr_list for nullness.	
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

int Component :: verifyExpressions()
{
	char	routine[]="Component :: verifyExpressions";
	int	i=0;

	// We have to verify the sons first...
	for( i=0; i<_n_son; i++ ){
		if( _son_list[i]->verifyExpressions() ){
			return( STATUS_FAILURE );
		}
	}
	// Now we verify these expressions...
	for( i=0; i<_n_meth; i++ ){
		if( _expr_list[i] != NULL ) {
			if( _expr_list[i]->verify() ){
				PrintWarning( 1, routine,
				"Troubles verifying Expression: \"%s\".",
				_expr_list[i]->toString() );
				return( STATUS_FAILURE );
			}
		}
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_verifyExpressions.cxx,v $";
 static char rcs_id2[] = "$Id: Component_verifyExpressions.cxx,v 1.3 2006/10/26 15:18:56 hsu Exp $";}
/*  ===================================================  */

}
