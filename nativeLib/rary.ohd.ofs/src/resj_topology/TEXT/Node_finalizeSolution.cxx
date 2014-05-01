//------------------------------------------------------------------------------
// Node :: finalizeSolution - does solution cleanup at the end of a 
//                            time-step.
//------------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 31 Mar 1998  Matthew J. Rutherford, Riverside Technology, inc
//                                      Created initial version.
// 15 Apr 1998  Daniel Weiler           Added TSDate& argument.
// 28 Feb 2003  James R. VanShaar, RTi  Added update of _endDischarge.
// 19 Feb 2004  JRV, RTi    Replaced calls to sumInflow with calls to 
//                          getTotalInflow.
// 24 Feb 2006  JRV, RTi    Major overhaul.
//                          Added _endDiversion carryover state
//                          Revised _endDishcarge carryover state
//                          Added handling of minimum outflow requirements.
//                          Added handling of diversions ToComp.
//------------------------------------------------------------------------------
// Variables:   I/O     Description
//
//
//------------------------------------------------------------------------------
// Return: 
//     Integer defining successful completion of the method.
// Calls:
//     getTotalInflow( TSDate )
//     DistributedTS::getDataValue( TSDate, int ); 
//     ResJSys::getIPR(); 
//     ResJ_ccwrite( &length, temp, &ipr );
//     HourTS::setDataValue( TSDate, double );
// Errors:
//     None
// Warnings:
//     None
// Notes:
//     Messages for reduction of diversion, one for successful reduction and
//     meeting of minimum constraints, another for insufficient reduction to
//     meet minimum constraints.
// Debug:
//     None
//------------------------------------------------------------------------------
#include "Node.h"
#include "ResJSys.h"

/**
Solves the mass balance (continuity) equation at the Node and addresses any
minimum flow constraints.
@return Integer flag of completion success
@param cur_ate TSDate of current time step
*/


int Node :: finalizeSolution( TSDate& cur_date )
{
    char routine[] = "Node :: finalizeSolution";
    double cur_inflow, cur_outflow;
    double min_allow = 0;
    int i;

    cur_inflow = getTotalInflow( cur_date );

    // We now solve continuity
    cur_outflow = cur_inflow - _diversion;

    // Now check minimum flow requirements and deal with any repercussions
    if( _min )
    {
       min_allow = _min_ctl.getDataValue( cur_date, _mode ); 
    }
    if( cur_outflow < min_allow)
    {
        // Modify the diversion and transfer any flow ToComp
        double prevDivers = _diversion;
        valueToMethod( cur_date, 1 );
        // _diversion may have been revised in valueToMethod.
        // Therefore, recalculate cur_outflow
        cur_outflow = cur_inflow - _diversion;

        // Make appropriate warnings.
        char temp[MAXC];
        int length;
        int ipr = ResJSys::getIPR(); 
        if( cur_outflow < min_allow )
        {
            // We reduced diversions as much as possible but still are not
            // going to meet minimum remainder flows.
            // Print a NOTE regarding the change, as per Design document
            sprintf( temp, "**NOTE** Minimum flow forced revision of diversion "
                "from %f to %f on NODE %s. Minimum flow still not met at %s.",
                prevDivers, _diversion, _id, cur_date.toString() );
            length = strlen( temp );
            ResJ_ccwrite( &length, temp, &ipr );
        }
        else
        {
            // We reduced diversions as much as necessary to meet minimum
            // remainder flows.
            sprintf( temp, "**NOTE** Minimum flow results in revision of "
                "diversion from %f to %f on NODE %s at %s.", prevDivers,
                _diversion, _id, cur_date.toString() );
            length = strlen( temp );
            ResJ_ccwrite( &length, temp, &ipr );
        }
    }
    else
    {
        // Deal with any transfer of Diversion "TOCOMP"
        if ( _specialTieOut )
        {
            // Transfer flow ToComp.  Do not revise diversion.
            valueToMethod( cur_date, 0 );
        }
    }

    _discharge = cur_outflow;

    _outflow_ts.setDataValue( cur_date, cur_outflow );
    _diversion_ts->setDataValue( cur_date, _diversion );

    // Update Rules states
    _endDiversion = _diversion;
    _endDischarge = _discharge;
    //_endInflow is set elsewhere.

    PrintDebug( 5, routine, "TIME  \"%s\".", cur_date.toString() );
    PrintDebug( 5, routine, "Node  \"%s\".", _id );
    PrintDebug( 5, routine, "Inflow   %f", cur_inflow );
    PrintDebug( 5, routine, "Outflow   %f", 
        _outflow_ts.getDataValue( cur_date ) );

    return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Node_finalizeSolution.cxx,v $";
 static char rcs_id2[] = "$Id: Node_finalizeSolution.cxx,v 1.4 2006/10/26 15:27:58 hsu Exp $";}
/*  ===================================================  */

}
