//------------------------------------------------------------------------------
// TS::getDataPoint
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 06 Jan 1998	Steven A. Malers, RTi	Created member function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSData TS :: getDataPoint ( TSDate& date )
{	char	routine[]="TS::getDataPoint";

	PrintWarning( 1, routine, 
	"This is a virtual function, redefine in lower classes" );

	TSData data;
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getDataPoint.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getDataPoint.cxx,v 1.3 2000/05/19 13:36:20 dws Exp $";}
/*  ===================================================  */

	return data;

}
