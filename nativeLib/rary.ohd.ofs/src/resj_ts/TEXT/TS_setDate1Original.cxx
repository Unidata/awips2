//------------------------------------------------------------------------------
// TS.setDate1Original - set the date for the original first data point
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setDate1Original( TSDate t )
{
	_date1_original = t;

	return( 0 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setDate1Original.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setDate1Original.cxx,v 1.2 2000/05/19 13:38:45 dws Exp $";}
/*  ===================================================  */

}
