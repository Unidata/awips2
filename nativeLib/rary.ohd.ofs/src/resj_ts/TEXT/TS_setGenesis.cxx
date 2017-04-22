//------------------------------------------------------------------------------
// TS.setGenesis - set the full comments information
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Create function.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setGenesis ( char **genesis )
{	char	routine[] = "TS.setGenesis";

	if ( !genesis ) {
		return 1;
	}

	if( _genesis ) {
        	_genesis = FreeStringList ( _genesis );
	}

	int nlist;
	_genesis = AddListToStringList ( _genesis, genesis, &nlist );

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setGenesis.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setGenesis.cxx,v 1.2 2000/05/19 13:39:50 dws Exp $";}
/*  ===================================================  */

}
