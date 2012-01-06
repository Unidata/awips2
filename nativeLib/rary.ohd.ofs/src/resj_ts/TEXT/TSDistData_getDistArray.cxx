//------------------------------------------------------------------------------
// TSDistData :: getDistArray() - returns the internal array.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 07 Apr 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "TSDistData.h"

float* TSDistData :: getDistArray()
{
	return( _distrib );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSDistData_getDistArray.cxx,v $";
 static char rcs_id2[] = "$Id: TSDistData_getDistArray.cxx,v 1.1 1999/02/18 15:19:15 dws Exp $";}
/*  ===================================================  */

}
