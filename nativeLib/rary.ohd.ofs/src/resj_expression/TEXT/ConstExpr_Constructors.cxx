//------------------------------------------------------------------------------
// ConstExpr :: ConstExpr - constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 15 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ConstExpr.h"

ConstExpr :: ConstExpr( char* exp )
{
	char	routine[]="ConstExpr :: ConstExpr";

	_is_local_value = 0;
	_value = NULL;
	_is_verified = 0;

	if( exp != NULL ){
		strcpy( _constant, exp );
	}
	else {
		PrintWarning( 1, routine,
		"Attempting to set a NULL constant expression." );
		sprintf( _constant, "%f", MISSING );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ConstExpr_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: ConstExpr_Constructors.cxx,v 1.3 2006/10/26 15:18:59 hsu Exp $";}
/*  ===================================================  */

}
