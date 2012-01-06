//----------------------------------------------------------------------------
// TSInterval.toString - convert to string representation
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	
//
//----------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Initial version.  Port from Java.
//		Riverside Technology,
//		inc.
//----------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//----------------------------------------------------------------------------

#include "TSInterval.h"

char TSIntervalString[256];   //cfan

char * TSInterval :: toString ( void )
{
//cfan	char	string[256];

//cfan	sprintf( string,
	sprintf( TSIntervalString, //cfan
	"TSInterval Base=%d,Mult=%d,String=\"%s\"",
	_interval_base, _interval_mult, _interval_base_string );

//cfan	return string;
	return (TSIntervalString);  //cfan

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_toString.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_toString.cxx,v 1.2 2006/04/10 16:08:36 xfan Exp $";}
/*  ===================================================  */

}
