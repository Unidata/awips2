//------------------------------------------------------------------------------
// Method_SetGet - base SetGet for common scheme data.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 08 Apr 1998	Matthew J. Rutherford, RTi
//					Added functions to set/get the forecast
//					dates.
// 23 Apr 1998	DKW			Added get group ID.
// 13 Nov 2001	James R. VanShaar, RTi	Added setInactiveState()
// 13 Nov 2001	JRV, RTi	Added setCOstring( )
// 11 Dec 2002	JRV, RTi	Enhanced setInactiveState. Added sendToTS
// 27 Mar 2006	JRV, RTi    Added sendToTS( date ),
//                          endToTS( TSDate date, double scalar ),
//                          getMyValueAsOut( double *in, double *out )
//                          Deleted sendToTS( TSDate date, double remaining,
//                          double fraction ) as it was replaced by the other
//                          sendToTS.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "Method.h"
#include "Component.h"

TSDate Method :: getForecastDate1()
{
	return( _forecast_date1 );
}

TSDate Method :: getForecastDate2()
{
	return( _forecast_date2 );
}

int Method :: getGroupID()
{
	return( _group_id );
}

char* Method :: getID()
{
	return( _id );
}

int Method :: getTimeInterval()
{
	return( _t_int );
}

int Method :: getTimeMult()
{
	return( _t_mult );
}

double Method :: getTimeStepSec()
{
	return( _t_step_in_sec );
}

char* Method :: getType()
{
	return( _type );
}

//------------------------------------------------------------------------------
// getMyValueAsOut - Default of virutal method getMyValueAsOut.  Does nothing.
//------------------------------------------------------------------------------
// Does nothing as the default of the virtual Method.
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Does nothing as the default of the virtual Method.
@return none
@param in Double pointer required by the method.
@param out Double pointer required by the method.
*/

void Method :: getMyValueAsOut( double *in, double *out )
{
    // If this is called, instead of the specific method, do nothing.
}

int Method :: linkCopiedTS( Component *NewOwner, Component *NewRoot )
{
	return( STATUS_SUCCESS );
}

//------------------------------------------------------------------------------
// sendToTS - Default of virutal method sendToTS( TSDate ).  Does nothing.
//------------------------------------------------------------------------------
// Does nothing as the default of the virtual Method.
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Does nothing as the default of the virtual Method.
@return none
@param date TSDate required by the method.
*/
void  Method :: sendToTS( TSDate date )
{
    // If this is called, instead of the specific method, do nothing.
}

//------------------------------------------------------------------------------
// sendToTS - Default of virutal method sendToTS( TSDate, double ).  Does
//            nothing.
//------------------------------------------------------------------------------
// Does nothing as the default of the virtual Method.
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Does nothing as the default of the virtual Method.
@return none
@param date TSDate required by the method.
@param scalar Double required by the method.
*/
void  Method :: sendToTS( TSDate date, double scalar )
{
    // If this is called, instead of the specific method, do nothing.
}

int Method :: setForecastDate1( TSDate& d )
{
	_forecast_date1 = d;
	return( STATUS_SUCCESS );
}

int Method :: setForecastDate2( TSDate& d )
{
	_forecast_date2 = d;
	return( STATUS_SUCCESS );
}

int Method :: setID ( char* id )  
{

	char routine[]="Method :: setID";

	if( !strcmp( id, "" ) ) {
		PrintWarning( 1, routine, "Method ID is blank." );
		return( STATUS_FAILURE );
	}
	else {
		strcpy( _id, id );
	}
	return( STATUS_SUCCESS );
}		

void Method :: setInactiveState()
{
	char routine[]="Method::setInactiveState";

	_Active = 0;

	return;
}

int Method :: setCOstring( )
{
	char routine[]="Method::setCOstring";
	
	return( STATUS_SUCCESS );
}

int Method :: setCOstring( TSDate& cur_date )
{
	char routine[]="Method::setCOstring";
	
	return( STATUS_SUCCESS );
}

int Method :: setTimeStep( int mult )
{
	return( setTimeStep( 3600, mult )  );
}

int Method :: setTimeStep( int interval, int mult ) 
{
	if( interval == 0 || mult == 0 ) {
		return( STATUS_FAILURE );
	}
	_t_int = interval;
	_t_mult = mult;
	_t_step_in_sec = (double)_t_int * (double)_t_mult;
	return( STATUS_SUCCESS );
}

void Method :: setUnitType( int type ) 
{
	 _units = type;
	 return;
}
int Method :: getUnitType() 
{
	return( _units );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Method_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Method_SetGet.cxx,v 1.5 2006/10/26 15:27:16 hsu Exp $";}
/*  ===================================================  */

}
