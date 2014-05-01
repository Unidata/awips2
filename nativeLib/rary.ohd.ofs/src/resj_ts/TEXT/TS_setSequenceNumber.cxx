//------------------------------------------------------------------------------
// TS.setSequenceNumber - set the time series sequence number
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 09 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: setSequenceNumber ( int sequence_number )
{
	_sequence_number = sequence_number;

	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_setSequenceNumber.cxx,v $";
 static char rcs_id2[] = "$Id: TS_setSequenceNumber.cxx,v 1.2 2000/05/19 13:39:52 dws Exp $";}
/*  ===================================================  */

}
