//------------------------------------------------------------------------------
// TS.getSequenceNumber - get the time series sequence number
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 09 Jan 1998	Steven A. Malers,	Iniitial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: getSequenceNumber ( void )
{
	return( _sequence_number );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getSequenceNumber.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getSequenceNumber.cxx,v 1.2 2000/05/19 13:37:38 dws Exp $";}
/*  ===================================================  */

}
