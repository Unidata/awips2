//-----------------------------------------------------------------------------
// TSDistData :: init - initialized all the data members to appropriate values
//-----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// History:
// 
// 05 Mar 1998	Daniel Weiler,	Created initial version.
//		Riverside Technology, inc.
//-----------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// routine	L	name of routine.
//-----------------------------------------------------------------------------

#include "TSDistData.h"

int TSDistData :: initialize ( ) 
{	char	routine[] = "TSDistData::initialize";

	_distrib = NULL;
		
	_n_dist = 0;

	return (STATUS_SUCCESS);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSDistData_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: TSDistData_initialize.cxx,v 1.1 1999/02/18 15:19:17 dws Exp $";}
/*  ===================================================  */

}
