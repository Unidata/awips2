//------------------------------------------------------------------------------
// SetMax :: SetGet - Set/gets for data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 28 Nov 2001	James R. VanShaar, RTi
//					Created initial version.
// 11 Dec 2002	JRV, RTi	Enhanced setInactiveState.  Added sendToTS
// 27 Mar 2006  JRV, RTi    Added void sendToTS( TSDate date ),
//                          void sendToTS( TSDate date, double scalar ), and
//                          getMyValueOut( double *in, double *out ).
//                          Eliminated double sendToTS( TSDate, double, double).
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetMax.h"
#include "ResJSys.h"

void SetMax :: getMyValueAsOut( double *in, double *out )
{
    int i;

    for( i = 0; i < _group_n; i++ ) {
        if( _group[i]->_Active > 0 ) {
                _group[i]-> getMyValueAsOut( in, out );
    
            // There is only one sub-method worth looking at.
            // We've found it, bail!
            i = _group_n;
        }
    }
}

void SetMax :: sendToTS( TSDate date )
{
    int i;

    for( i = 0; i < _group_n; i++ ) {
        if( _group[i]->_Active > 0 ) {
            _group[i]-> sendToTS( date );
    
            // There is only one sub-method worth looking at.
            // We've found it, bail!
            i = _group_n;
        }
    }
}

void SetMax :: sendToTS( TSDate date, double scalar )
{
    int i;

    for( i = 0; i < _group_n; i++ ) {
        if( _group[i]->_Active > 0 ) {
            _group[i]-> sendToTS( date, scalar );
    
            // There is only one sub-method worth looking at.
            // We've found it, bail!
            i = _group_n;
        }
    }
}

void SetMax :: setInactiveState()
{
	char routine[]="SetMax::setInactiveState";
	int i;

	// SetMax has no states, but may contain sub-methods which do
	for( i = 0; i < _group_n; i++ ) {
		_group[i]->setInactiveState();
	}

	_Active = 0;

	return;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetMax_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: SetMax_SetGet.cxx,v 1.4 2006/10/26 15:33:23 hsu Exp $";}
/*  ===================================================  */

}
