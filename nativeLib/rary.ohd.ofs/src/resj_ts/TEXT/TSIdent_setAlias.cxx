//------------------------------------------------------------------------------
// TSIdent.setAlias - set the time series alias
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the time series alias.
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// alias	I	Alias to set.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setAlias ( char *alias )
{	char	routine[]="TSIdent.setAlias";
	int	dl = 50;

	if ( !alias ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set alias to \"%s\"", alias );

	if( _alias ){
        	delete [] _alias;
	}

	_alias = new char[strlen( alias )+1];

	if( !_alias ){
		PrintWarning( 1, routine,
		"Unable to Allocate memory for alias \"%s\"", alias );
                return 1;
	}

	strcpy( _alias, alias );

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setAlias.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setAlias.cxx,v 1.2 2000/05/19 13:08:20 dws Exp $";}
/*  ===================================================  */

}
