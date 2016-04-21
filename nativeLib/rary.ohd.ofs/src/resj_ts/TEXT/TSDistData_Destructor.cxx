//----------------------------------------------------------------------------
// TSDistData Destructor
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	
//
//----------------------------------------------------------------------------
// History:
// 
// 22 Sep 1997	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	Steven A. Malers, RTi	Change _quality_flag to _data_flag.
//----------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// routine	L	name of function
//----------------------------------------------------------------------------

#include "TSDistData.h"

TSDistData :: ~TSDistData ( )
{
	if( _distrib ){
		delete [] _distrib;
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSDistData_Destructor.cxx,v $";
 static char rcs_id2[] = "$Id: TSDistData_Destructor.cxx,v 1.1 1999/02/18 15:19:15 dws Exp $";}
/*  ===================================================  */

}
