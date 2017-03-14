//------------------------------------------------------------------------------
// TS.setComments - set the full comments information
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setComments ( char **comments )
{	char	routine[] = "TS.setComments";

	if ( !comments ) {
		return 1;
	}

	if( _comments ) {
        	_comments = FreeStringList ( _comments );
	}

	int nlist;
	_comments = AddListToStringList ( _comments, comments, &nlist );

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setComments.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setComments.cxx,v 1.2 2000/05/19 13:38:44 dws Exp $";}
/*  ===================================================  */

}
