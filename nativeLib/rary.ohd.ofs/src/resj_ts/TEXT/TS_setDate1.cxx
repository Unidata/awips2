//------------------------------------------------------------------------------
// TS.setDate1 - set the start of data date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,	Do not use _data_limits because
//		inc.			because we need to get at
//					directly in the data access functions!
//					_data_limits is reset by refresh().
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setDate1( TSDate t )
{
	_date1 = t;

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setDate1.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setDate1.cxx,v 1.2 2000/05/19 13:38:45 dws Exp $";}
/*  ===================================================  */

}
