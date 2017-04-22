//------------------------------------------------------------------------------
// TS.refresh 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function used to be calcMaxMinValues but was
//			changed to refresh() to be more generic.
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford,	Created function.
//		RTi.
// 16 Jun 1997	MJR, RTi		Added overload.
// 06 Jan 1996	Steven A. Malers, RTi	Change function to refresh().
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: refresh( void )
{	char	routine[]="TS.refresh()";

	PrintWarning( 1, routine,
	"This is a virtual function, define in lower classes" );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_refresh.cxx,v $";
 static char rcs_id2[] = "$Id: TS_refresh.cxx,v 1.2 2000/05/19 13:38:44 dws Exp $";}
/*  ===================================================  */

}
