//------------------------------------------------------------------------------
// SetSum :: SetGet - Set/gets for data members.
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
#include "SetSum.h"
#include "ResJSys.h"

void SetSum :: getMyValueAsOut( double *in, double *out )
{
    int i;
        double sum = 0.0;

    for( i = 0; i < _group_n; i++ ) {
        _group[i]-> getMyValueAsOut( in, out );
    }
}

void SetSum :: sendToTS( TSDate date )
{
    int i;

    for( i = 0; i < _group_n; i++ ) {
        _group[i]-> sendToTS( date );
    }
}

void SetSum :: sendToTS( TSDate date, double scalar )
{
    int i;

    for( i = 0; i < _group_n; i++ ) {
        _group[i]-> sendToTS( date, scalar );
    }
}

void SetSum :: setInactiveState()
{
	char routine[]="SetSum::setInactiveState";
	int i;

	// SetSum has no states, but may contain sub-methods which do
	for( i = 0; i < _group_n; i++ ) {
		_group[i]->setInactiveState();
	}

	_Active = 0;

	return;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetSum_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: SetSum_SetGet.cxx,v 1.4 2006/10/26 15:34:43 hsu Exp $";}
/*  ===================================================  */

}
