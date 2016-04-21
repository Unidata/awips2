//------------------------------------------------------------------------------
// LagK_SetGet - Set/gets for data members.
//------------------------------------------------------------------------------
// History:
// 
// 06 May 1998  Daniel Weiler, Riverside Technology, inc
//                              Created initial version.
// 12 Nov 2001  James R. VanShaar, RTi  
//                              Added setInactiveState
// 12 Nov 2001  JRV, RTi        Added setCOstring
// 19 Nov 2001  JRV, RTi        Added inclusion of initial outflow in carry
//                              over data
// 04 Dec 2002  JRV, RTi        Revised setCOstring  (TSDate&) to handle inflows
//                              prior to the beginning timestep.
// 11 Dec 2002  JRV, RTi        Enhanced setInactiveState.
// 18 Dec 2002  JRV, RTi        Revised carryover structure to include a type
//                              keyword "LAGK".
// 19 Feb 2004  JRV, RTi        Replaced calls to sumInflow with calls to 
//                              getTotalInflow.
// 02 Feb 2006  Marc L. Baldo   Update setCOstring to match new LagK.  
//                              Also reformatted code to match NWS standard.
//------------------------------------------------------------------------------

/**
@author Daniel Weiler, RTi; James R. VanShaar, RTi; Marc L. Baldo, RTi
*/

#include "LagK.h"
#include "ResJSys.h"

//------------------------------------------------------------------------------
// getCarryOverValues - Populate the empty table passed passed with carryover 
//                      based on the current date argument
//------------------------------------------------------------------------------
// The table must be allocated/sized by the calling routine
//------------------------------------------------------------------------------
// Return: 
//     Modified table
// Calls: 
//     None
// Errors: 
//     None
// Warnings: 
//     None
//------------------------------------------------------------------------------

/**
Populate the empty table passed with carryover based on the current 
date argument.
@return none
*/

void LagK :: getCarryOverValues ( TSDate& cur_date , Table &carryOverTable ,
                                  int offset )
{
    int requiredCO = _sizeInflowCO ;
    char tmp_str3[512];
    int i; 

    // clear
    for(i = 0; i < carryOverTable.getNRows(); i++)
    {
        carryOverTable.populate ( i, FLOWCOLUMN, 0.0 ) ;
        carryOverTable.populate ( i, TIMECOLUMN, 0.0 ) ;
    }

    // The number of time steps between the forecast date and the current
    // simulation date is the number of steps that are available from
    // simulation.
    int nSimulatedDates = 1 ;
    if ( getForecastDate1 ( ) <= cur_date )
    {
        nSimulatedDates = TSDate::getNumIntervals ( 
            getForecastDate1 ( ) , cur_date, _t_int, _t_mult ) + 1 ;

        if ( nSimulatedDates > requiredCO )
            nSimulatedDates = requiredCO;
    }

    // Place numbers into the working array, note that times will be
    // negative.  When nSimulatedDates is larger than the 
    // carryover size, the carryover array will not be used.
    double timeHours = (double) -( requiredCO - 1 ) * _t_mult;
    for ( i = 0 ; i < requiredCO - nSimulatedDates; i++ )
    {
        double currentFlow = _co_inflow[i + nSimulatedDates] ;
        carryOverTable.populate ( i, FLOWCOLUMN, currentFlow ) ;
        carryOverTable.populate ( i, TIMECOLUMN, timeHours ) ;
        timeHours += _t_mult ;
    }

    // nSimulatedDates includes the current date, so go back
    // ( nSimulatedDates - 1 ) periods
    TSDate date1 = cur_date ;
    date1.addInterval ( _t_int, -1 * (int) (nSimulatedDates - 1) * _t_mult ) ;

    // Get the remainder of the values from the calculated inflow time
    // series for this reach.  Move back by nIntervals time steps and save
    // values from that time through the current time.
    // Fill in the remaining values with calculated inflow time series,
    // using getTotalInflow to the get time step inflow for the date.
    for ( i = requiredCO - nSimulatedDates ; i < requiredCO; i++ )
    {
        double currentFlow = _owner->getTotalInflow ( date1 ) ;
        date1.addInterval ( _t_int, _t_mult ) ;
        carryOverTable.populate ( i, FLOWCOLUMN, currentFlow ) ;
        carryOverTable.populate ( i, TIMECOLUMN, timeHours ) ;
        timeHours += _t_mult ;
    }
}

//------------------------------------------------------------------------------
// getCarryOverValuesStr - call getCarryOverValues and populate the 
//                         passed char array with carryover values
//------------------------------------------------------------------------------
// Return: 
//     Populated char array, containing carryover values, 8 characters/value,
//     with no spaces between values
// Calls: 
//     LagK::getCarryOverValues
// Errors: 
//     None
// Warnings: 
//     None
//------------------------------------------------------------------------------

/**
Populate the empty string passed with carryover based on the current 
date argument.
@return none
*/

void LagK :: getCarryOverValuesStr ( TSDate& cur_date , char *coString )
{
    int i;
    Table coTable;
    char tmp_str3[512];

    strcpy ( coString, "") ;
    coTable.allocateDataSpace ( _sizeInflowCO + 1 ) ;
    getCarryOverValues ( cur_date, coTable ) ;

    for( i = 0; i < _sizeInflowCO; i++ )
    {
        sprintf ( tmp_str3, "%f", coTable.lookup ( i, FLOWCOLUMN ) ) ;
        tmp_str3[8] = '\0';
        strcat ( coString, tmp_str3 ) ;
    }
}

//------------------------------------------------------------------------------
// Get - Get the _kconst (constant K) member variable
//------------------------------------------------------------------------------
// Return: _kconst variable contents
// Calls: 
//     None
// Errors: 
//     None
// Warnings: 
//     None
//------------------------------------------------------------------------------

/**
Get the _kconst (constant K) member variable.
@return _kconst (constant K)
*/

double LagK :: getK()
{
	return( _kconst );
}

//------------------------------------------------------------------------------
// getLag - Get the _lag (constant lag) member variable
//------------------------------------------------------------------------------
// Return: _lag variable contents
// Calls: 
//     None
// Errors: 
//     None
// Warnings: 
//     None
//------------------------------------------------------------------------------

/**
Get the _lag (constant lag) member variable.
@return _lag (constant lag)
*/

int LagK :: getLag ( )
{
    return ( _lag ) ;
}


//------------------------------------------------------------------------------
// getLag - Get the _sizeInflowCO member variable
//------------------------------------------------------------------------------
// Return: _sizeInflowCO variable contents
// Calls: 
//     None
// Errors: 
//     None
// Warnings: 
//     None
//------------------------------------------------------------------------------

/**
Get the _sizeInflowCO member variable.
@return _sizeInflowCO 
*/

int LagK :: getSizeInflowCO ( )
{
    return ( _sizeInflowCO ) ;
}

//------------------------------------------------------------------------------
// setInactiveState - Make this object store nothing in member variables
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     None
// Errors: 
//     None
// Warnings: 
//     None
//------------------------------------------------------------------------------

/**
Make this object store nothing in member variables.
@return none
*/

void LagK :: setInactiveState ( )
{
    // No state data for LagK is actually stored here.
    // It is actually part of the reach.
    // Nothing is required here.
    
    _Active = 0 ;
}

//------------------------------------------------------------------------------
// setCOstring - Set the carryover storage using the carryover array
//------------------------------------------------------------------------------
// Return: success or failure of the function
// Calls:
//     ResJSys :: addCOString
// Errors: 
//     None
// Warnings: 
//     None
//------------------------------------------------------------------------------

/**
Set the carryover storage using the carryover array.
@return success or failure of the function
*/
int LagK :: setCOstring ( )
{
    char routine[] = "LagK::setCOstring" ;
    char tmp_str1[52 + 512], tmp_str2[512], tmp_str3[9] ; 
    char future_str[8 * 3 + 4 + 1] ;
    char future[9] = "*FUTURE*" ;
    int i ;
    
    // Prepare name and index portion
    sprintf ( tmp_str1, "%-12.12s%-12.12s%4.4s%-12.12s%-12.12s", 
              "REACH", _owner->_id, "-999", _id, "LAGK" ) ;
    tmp_str1[52] = '\0' ;
    
    // Prepare Inflow data
    tmp_str2[0] = '\0' ;
    for ( i = 0 ; i < _sizeInflowCO ; i++ ) 
    {
        sprintf ( tmp_str3, "%f", _co_inflow[i] ) ;
        strncat ( tmp_str2, tmp_str3, 8 ) ;
    }
    strcat ( tmp_str1, tmp_str2 ) ;
    
    // Now add the current outflow
    sprintf ( tmp_str2, "%f", _owner->_outflow ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;

    // Now add the current storage
    sprintf ( tmp_str2, "%f", _storageCO ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;

    // Now add the lagged inflow
    sprintf ( tmp_str2, "%f", _laggedInflow ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;
    
    // Prepare and add future place holders
    // NOTE that the last one gets trimmed off since we've added the
    // "LAGK" as NewCO in the tmp_str1 above.
    sprintf ( future_str, "%8.8s%4.4s", future, future ) ;
    future_str[28] = '\0' ;
    strcat ( tmp_str1, future_str ) ;
    
    ResJSys :: addCOString ( tmp_str1 ) ;
    return ( STATUS_SUCCESS ) ;
}

//------------------------------------------------------------------------------
// setCOstring - Set the carryover string using the cutoff date supplied
//------------------------------------------------------------------------------
// Return: success or failure of the function
//
// Calls:
//     ResJSys :: addCOString
// Errors: 
//     Not enough carryover storage in the array (_co_inflow)
// Warnings: 
//     Failure ResJSys :: addCOString ( ) function to add this carryover string
//------------------------------------------------------------------------------
// Notes:
// For carryover, the _co_save array contains values from the carryover
// as initially passed to the program.  If the cur_date is within the
// carryover date, the number of values available is counted.  If additional
// carryover is required to get to match the _sizeInflowCO, the remainder
// is taken from getTotalInflow for the reach.
//
// Example:
//   _sizeInflowCO is equal to '8'
//   6 hour data, 1 day into solution (4 samples), asking for carryover save.
//
//   Zero based:     0  1  2  3  4  5  6  7  8  9 10 11
//   Absolute TS#:   1  2  3  4  5  6  7  8  9 10 11 12
//   Cosave Exist:   C1 C2 C3 C4 C5 C6 C7 C8               (_co_save[])
//   Simul. Exist:                           A  B  C  D    (getTotalInflow)
//   Forecast Date:                          ^
//   Current  Date:                                   ^
//   Forecast Date:                        ^
//   Current  Date:                        ^
//   Carryover:                  C5 C6 C7 C8 A  B  C  D

//                                           
//   Num Periods between current date and Forecast Date = 12
//   Num of Carryover required = 8
//   
//------------------------------------------------------------------------------

/**
Set the carryover string using the cutoff date supplied.
@return success or failure of the function
@param cur_date The last date of the carryover, which must be a valid timestep
*/

int LagK :: setCOstring ( TSDate& cur_date )
{
    char routine[] = "LagK::setCOstring" ;
    char tmp_str1[52 + 512], tmp_str2[512], tmp_str3[9] ;
    char future_str[8 * 3 + 4 + 1] ;
    char future[9] = "*FUTURE*" ;
    int i;
    
    // Write the header
    sprintf ( tmp_str1, "%-12.12s%-12.12s%4.4s%-12.12s%-12.12s", "REACH", 
              _owner->_id, "-999", _id, "LAGK" ) ;
    tmp_str1[52] = '\0' ;
    tmp_str2[0] = '\0' ;

    // The construct() member function has the calculated number of 
    // inflow carryover locations required in carryover.
    int requiredCO = _sizeInflowCO ;

    // Get the carryover values into a string and add to the final string
    getCarryOverValuesStr ( cur_date, tmp_str2 );
    strcat ( tmp_str1, tmp_str2 ) ;

    // Add the current outflow
    sprintf ( tmp_str2, "%f", _owner->_outflow ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;

    // Now add the current storage
    sprintf ( tmp_str2, "%f", _storageCO ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;

    // Now add the lagged inflow
    sprintf ( tmp_str2, "%f", _laggedInflow ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;
    
    // Prepare and add future place holders
    // NOTE that the last one gets trimmed off since we've added the
    // "LAGK" as NewCO in the tmp_str1 above.
    sprintf ( future_str, "%8.8s%4.4s", future, future ) ;
    future_str[28] = '\0' ;
    strcat ( tmp_str1, future_str ) ;
    
    if ( ResJSys::addCOString ( tmp_str1 ) ) 
    {
        PrintWarning ( 1, routine, "Troubles adding carryover data to "
                       "CO array." ) ;
        return ( STATUS_FAILURE ) ;
    }
    
    return ( STATUS_SUCCESS ) ;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_SetGet.cxx,v 1.6 2006/10/26 15:22:23 hsu Exp $";}
/*  ===================================================  */

}


