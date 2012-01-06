//------------------------------------------------------------------------------
// ComboMethod :: ComboMethod - setGroup. Set all groups for this method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 22 Mar 2005	KWZ, OHD	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
// srcCom
//------------------------------------------------------------------------------
#include "ComboMethod.h"

void  ComboMethod:: setGroup( ComboMethod* srcMeth ) 
{	int j;

	_group_n = srcMeth->_group_n;
	for( j = 0; j < _group_n; j++  ){
		_group[j] = _owner->getMethod( srcMeth->_group[j]->getType(),
			srcMeth->_owner->getID(), srcMeth->_group[j]->getID() );	
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/ComboMethod_setGroup.cxx,v $";
 static char rcs_id2[] = "$Id: ComboMethod_setGroup.cxx,v 1.1 2005/06/29 17:33:01 wkwock Exp $";}
/*  ===================================================  */

}
