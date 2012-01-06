//------------------------------------------------------------------------------
// Node :: initialize - initializes data members.
//------------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998  Daniel Weiler, Riverside Technology, inc
//                                      Created initial version.
// 16 Oct 1998  DKW, RTi        Added stage TS data member
// 20 Nov 2001  James R. VanShaar, RTi  Added _dischargeCO and 
//                                     _prevDischargeCO
// 10 Jun 2002  JRV, RTi        Added and / or updated inflow and discharge Rule
//                              states (incl. removal of dischargeCO).
// 16 Feb 2006  JRV, RTi    Major overhaul of format and content.
//                          Commented out unused stage work.
//                          Added diversion state, diversion time series, 
//                          minimum remainder functionality.
//                          Revised inflow states, etc. to be compatible.
// 17 Aug 2007 	DJS, RTi    Inititalize rating table and rating curve id
//			    params - used for Reservoir Tools Enhancement
//------------------------------------------------------------------------------
// Variables:   I/O     Description
//
//
//------------------------------------------------------------------------------
#include "Node.h"

//------------------------------------------------------------------------------
// Return: 
//     Integer defining successful initialization
// Calls:
//     strcpy
//     new HourTs()
//     HourTs::setDate( TSDate )
//     HourTs::setDataInterval( TSDate )
//     Method::getTimeInterval()
//     Method::getTimeMult()
//     HourTS::allocateDataSpace()
//     PrintWarning( int, char *, char* )
//     DistributedTS::setRelativeFlag( int );
// Errors:
//     None
// Warnings:
//     Problems allocating space on the _diversion_ts time series. Will result
//       in failure in calling program.
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Initializaes the Node data members.
@return Integer flag of completion success
*/

int Node :: initialize()
{
    char routine[] = "Node :: initialize";
    TSDate t;

    strcpy( _type, "NODE" );

/*****************************
// This section is unnecessary since as of 2006-02-16 no stage calculations
// are done.
//    _stage_ts = new HourTS();
//    _stage_ts->setDate1( Method :: getForecastDate1() );
//    _stage_ts->setDate2( Method :: getForecastDate2() );
//    _stage_ts->setDataInterval( Method :: getTimeInterval(),
//        Method :: getTimeMult() );
//    if( _stage_ts->allocateDataSpace() ) {
//        PrintWarning( 1, routine, "Troubles allocating data space"
//        " for loss TS on %s.", _id );
//        return( STATUS_FAILURE );
//    }
*****************************/

    // Initialize Diversion time series
    _diversion_ts = new HourTS();
    _diversion_ts->setDate1( Method :: getForecastDate1() );
    _diversion_ts->setDate2( Method :: getForecastDate2() );
    _diversion_ts->setDataInterval( Method :: getTimeInterval(),
        Method :: getTimeMult() );
    if( _diversion_ts->allocateDataSpace() ) {
        PrintWarning( 1, routine, "Troubles allocating data space"
        " for diversion TS on %s.", _id );
        return( STATUS_FAILURE );
    }

    _min_ctl.setRelativeFlag( 1 );

    _startInflow = MISSING;
    _startDischarge = 0;
    _startDiversion = 0;

    _prevInflow = MISSING;
    _prevDischarge = 0.0;
    _prevDiversion = 0;

    _endInflow = MISSING;
    _endDischarge = MISSING;

    _diversion = 0;
    _min = 0;

    _mode = NORMAL;

    strcpy(_rating_curve_id,"NULL");
    _has_rating_table = FALSE;

    return(STATUS_SUCCESS);

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Node_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Node_initialize.cxx,v 1.6 2006/10/26 15:28:04 hsu Exp $";}
/*  ===================================================  */

}
