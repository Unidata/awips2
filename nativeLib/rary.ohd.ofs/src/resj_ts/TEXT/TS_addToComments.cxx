//------------------------------------------------------------------------------
// TS.addToComments - add to the comments
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function adds a string to the comments string list.
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers, RTi	Created function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: addToComments( char *comment )
{	char	routine[]="TS.addToComments";

	int	listlen;
	_comments = AddToStringList ( _comments, comment, &listlen );

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_addToComments.cxx,v $";
 static char rcs_id2[] = "$Id: TS_addToComments.cxx,v 1.2 2000/05/19 13:35:20 dws Exp $";}
/*  ===================================================  */

}
