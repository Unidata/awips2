//----------------------------------------------------------------------------
// TSIdent.toString - convert to string representation
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	
//
//----------------------------------------------------------------------------
// History:
// 
// 07 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//----------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// id		I	Incoming TSIdent.
//----------------------------------------------------------------------------

#include "resj/TSIdent.h"

char TSIdentString[256];  //cfan

char * TSIdent :: toString ( void )
{	
//cfan  char	string[256];

//cfan 	sprintf( string,
	sprintf( TSIdentString,   //cfan
	"Loc=\"%s\",Src=\"%s\",Type=\"%s\",Int=\"%s\",Scen=\"%s\"",
	_full_location, _full_source, _type, _interval_string, _scenario );

//cfan	return string;
	return (TSIdentString);  //cfan

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_toString.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_toString.cxx,v 1.3 2006/04/10 16:07:55 xfan Exp $";}
/*  ===================================================  */

}
