//------------------------------------------------------------------------------
// TS::getDataPosition - get the data array position for a date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 06 Feb 1997	Steven A. Malers,	Created member function.
//		Riverside Technology,
//		inc.
// 06 Jun 1997	SAM, RTi		Add third positional argument to support
//					minute time series.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// column	I	Column position in data array.
// date		I	Date to get data position form.
// row		I	Row position in data array.
// third	I	Third array position for data array.
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: getDataPosition( TSDate& date, int *row, int *column, int *third )
{	char	routine[] = "TS::getDataPointer";

	PrintWarning( 1, routine, 
	"This is a virtual function, redefine in lower classes" );

	return 1; 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getDataPosition.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getDataPosition.cxx,v 1.2 2000/05/19 13:36:20 dws Exp $";}
/*  ===================================================  */

}
