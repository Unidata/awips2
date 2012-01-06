//------------------------------------------------------------------------------
// TS.addToGenesis - add to the genesis
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function adds a string to the genesis string list.
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers, RTi	Created function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: addToGenesis( char *genesis )
{	char	routine[]="TS.addToGenesis";

	int	listlen;
	_genesis = AddToStringList ( _genesis, genesis, &listlen );

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_addToGenesis.cxx,v $";
 static char rcs_id2[] = "$Id: TS_addToGenesis.cxx,v 1.2 2000/05/19 13:35:21 dws Exp $";}
/*  ===================================================  */

}
