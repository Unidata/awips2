//----------------------------------------------------------------------------
// TSDistData Constructors
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------

#include "TSDistData.h"

//----------------------------------------------------------------------------
// TSDistData Constructor
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	
//
//----------------------------------------------------------------------------
// History:
// 
// 05 Mar 1998	Daniel Weiler, Riverside Technology, inc.
//					Initial version.
//----------------------------------------------------------------------------
// Variables:		I/O	Description		
//
//
//----------------------------------------------------------------------------

TSDistData :: TSDistData( ) : TSData()
{
	initialize();
}

TSDistData :: TSDistData( const TSDistData& t ) 
{
	initialize();
	_n_dist = t._n_dist;
	_distrib = t._distrib;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSDistData_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: TSDistData_Constructors.cxx,v 1.1 1999/02/18 15:19:14 dws Exp $";}
/*  ===================================================  */

}
