// ----------------------------------------------------------------------------
// TSInterval Destructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	
//
// ----------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------
// Variable	I/O	Description		
//
// routine	L	name of function
// ----------------------------------------------------------------------------

#include "TSInterval.h"

TSInterval :: ~TSInterval ( void )
{	char	routine[] = "TSInterval::~TSInterval";

	delete [] _interval_base_string;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_Destructor.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_Destructor.cxx,v 1.1 1999/02/18 15:19:45 dws Exp $";}
/*  ===================================================  */

}
