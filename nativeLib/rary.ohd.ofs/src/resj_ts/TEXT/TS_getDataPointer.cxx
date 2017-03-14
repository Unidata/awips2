//------------------------------------------------------------------------------
// TS::getDataPointer
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 1996	Matthew J. Rutherford, RTi	created member function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

double *TS :: getDataPointer( TSDate& date )
{
	char	routine[]="TS::getDataPointer";

	PrintWarning( 1, routine, 
	"This is a virtual function, redefine in lower classes" );

	return( (double*)NULL );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_getDataPointer.cxx,v $";
 static char rcs_id2[] = "$Id: TS_getDataPointer.cxx,v 1.2 2000/05/19 13:36:20 dws Exp $";}
/*  ===================================================  */

}
