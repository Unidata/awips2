//------------------------------------------------------------------------------
// ComboMethod :: ComboMethod - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 22 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 16 Feb 2006  JRV, RTi	Generalized the method for different component
//				types.  (It Was Reservoir only.)
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ComboMethod.h"

ComboMethod :: ComboMethod( Component* owner, char* type ) : 
	ComponentMethod( owner, type ) 
{
	initialize();
}

ComboMethod :: ComboMethod( const ComboMethod& meth, Component* owner ) 
	: ComponentMethod( meth, owner )
{
	char	routine[]="ComboMethod :: ComboMethod";
	int i;

	initialize();
	_owner = owner;

/*kwz.r24-33.Can't get group here because method list is not totally ready*/
/*	_group_n = meth._group_n;
	for( i = 0; i < _group_n; i++  ){
		_group[i] = owner->getMethod( meth._group[i]->getType(),
			meth._owner->getID(), meth._group[i]->getID() );
	}
*/

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/ComboMethod_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: ComboMethod_Constructors.cxx,v 1.5 2006/10/26 15:12:32 hsu Exp $";}
/*  ===================================================  */

}
