//------------------------------------------------------------------------------
// SetWithdraw :: SetGet - Set/gets for data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Nov 2001	James R. VanShaar, RTi
//					Created initial version.
// 21 Nov 2001  JRV, RTi        Created setCOstring
// 11 Dec 2002	JRV, RTi	Enhanced setInactiveState. Added sendToTS.
// 18 Dec 2002	JRV, RTi	Revised method carryover to work with entire
// 				method identification set (type, owner_id,
// 				method_id).
// 27 Mar 2006  JRV, RTi    Added void sendToTS( TSDate date ), and
//                          void sendToTS( TSDate date, double scalar ), and
//                          getMyValueAsOut( double *in, double *out ).
//                          Eliminated double sendToTS( TSDate, double, double).
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetWithdraw.h"
#include "ResJSys.h"

int SetWithdraw :: linkCopiedTS( Component *NewOwner, Component *NewRoot )
{
	char oldCompName[MAXC], newCompName[MAXC], temp[MAXC];
	Component *newReceivingComp;
	
	// The caller of this function will be the owner of the copied 
	// ("original") SetWithdraw method.

	// Identify the name of the new _receivingComp
	sprintf( oldCompName, "%s", _receivingComp->_id);
	//kwz	sprintf( newCompName, "%s%s", oldCompName, "_2" );
	sprintf( newCompName, "%s", oldCompName);

	// Find the component, if possible, on the new sub tree.  
	// If it is not in the new subTree, no worries, we simply don't have 
	// to link it!
	newReceivingComp = NewRoot->getComponentPtr( newCompName );
	if( !newReceivingComp ) {
		return( STATUS_SUCCESS );
	}

	// Get the new Method.
	// Note that the method id remains the same without the "_2" suffix.
	//Method *newMethod = NewOwner->getMethod( "SETWITHDRAW", 
	SetWithdraw *newMethod = (SetWithdraw *) NewOwner->getMethod( "SETWITHDRAW", 
		NewOwner->_id, _id );  

	// Set the _receivingComp on the newMethod
	newMethod->_receivingComp = newReceivingComp;

	// Assign unique identifier to the timeseries, esp. the Alias
	HourTS *newComp_ts = &(newMethod->_Comp_ts);
	TSIdent id = newComp_ts->getIdentifier();
	sprintf( temp, ">>SpecialTie<<SetWithdraw-%s_%sTo%s",
		_owner->getID(),_id,_receivingComp->getID() );
	id.setAlias( temp );
	newComp_ts->setIdentifier( id );

	// Set the old Timeseries to be an inflow on the New Receiving component.
	newReceivingComp->setInflowTS( newComp_ts );
	//newReceivingComp->setInflowTS( &(newMethod->_Comp_ts));

	return( STATUS_SUCCESS );
}

void SetWithdraw :: getMyValueAsOut( double *in, double *out )
{
    *out += _myValue;
}

void SetWithdraw :: sendToTS( TSDate date )
{
    // Check to see if we really are sending to a time series.
    if( _receivingComp )
    {
        if( !_toCompMode )
        { 
            // the mode is instantaneous
            _Comp_ts.setDataValue( date, _myValue );
        }
        else
        {
            // The mode is NextStep
            date.addInterval( _t_int, _toCompMode*_t_mult );
            if( date <= getForecastDate2() )
            {
                _Comp_ts.setDataValue( date, _myValue );
            }
            // Otherwise, we are extending beyond the timeseries.
            // Don't attempt to assign the value.
        }
    }
}

void SetWithdraw :: sendToTS( TSDate date, double scalar )
{
	double value;

	value = _myValue * scalar;

	// Check to see if we really are sending to a time series.
	if( _receivingComp ) {
		if( !_toCompMode ) { 
			// the mode is instantaneous
			_Comp_ts.setDataValue( date, value );
		}
		else {
			// The mode is NextStep
			date.addInterval( _t_int, _toCompMode*_t_mult );
			if( date <= getForecastDate2() ) {
				_Comp_ts.setDataValue( date, value );
			}
			// Otherwise, we are extending beyond the timeseries.
			// Don't attempt to assign the value.
		}
	}

	_myValue = value;
}

void SetWithdraw :: setInactiveState()
{
	// Reinitialize as inactive any past blending variables
	_ts_step = _n_blend_ts + 4;
	_tbl_step = 1;

	// Note: PREVIOUSPOOL (a state value required as carry over is stored 
	//	as part of the reservoir info and / or time series.

	_Active = 0;
	_myValue = 0;

	return;
}

int SetWithdraw :: setCOstring( )
{
	char routine[]="SetWithdraw::setCOstring";
	char future[9]="*FUTURE*";
	char COtype[13]="METHOD";
	char tsStep_str[5], tblStep_str[5], transferVal_str[8],future_str[9], 
		value_str[17];
	char temp_str[52+16+8+1];	// Labels, values, future space, end

	// Get current values (values at end of the current time step)
	sprintf( tsStep_str, "%d", _ts_step );
	sprintf( tblStep_str, "%d", _tbl_step );
	sprintf( transferVal_str, "%f", _myValue );

	// Prepare value portion of the string
	sprintf(value_str, "%4.4s%4.4s%8.8s", tsStep_str, tblStep_str, 
		transferVal_str);

	// Prepare future place holders
	sprintf(future_str, "%8.8s", future);
	future_str[8] = '\0';

	// Prepare the full variable string
	COtype[6] = '\0';
	sprintf( temp_str, 
		"%-12.12s%-12.12s%4.4s%-12.12s%-12.12s%16.16s%8.8s",
		COtype, _id, "-999", "SETWITHDRAW", _owner->_id, value_str,
	       	future_str );

	// Write the string to the system CO string
	if( ResJSys::addCOString( temp_str ) ) {
		PrintWarning( 1, routine, 
			"Troubles adding carryover data to "
			"CO array." );
		return( STATUS_FAILURE );
	}

	// Need to develop CO etc.
	
	return( STATUS_SUCCESS );
}

int SetWithdraw :: setCOstring( TSDate& cur_date )
{
	char routine[]="SetWithdraw::setCOstring";
	int success;

	// CO is date independent.  Therefore simply call the non-dated 
	//	setCOstring.
	
	success = setCOstring( );

	return( success );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetWithdraw_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: SetWithdraw_SetGet.cxx,v 1.5 2006/10/26 15:35:06 hsu Exp $";}
/*  ===================================================  */

}
