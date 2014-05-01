//------------------------------------------------------------------------------
// CalcInflow::copy - calls copy constructor for method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Feb 2004	James R. VanShaar, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "CalcInflow.h"

CalcInflow* CalcInflow::copy( Component* owner )
{
	CalcInflow* meth = NULL;
	meth = new CalcInflow( *this, (Reservoir*)owner );
	meth->_isCopy=1;
	return( meth );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_copy.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_copy.cxx,v 1.2 2006/10/26 15:11:42 hsu Exp $";}
/*  ===================================================  */

}
