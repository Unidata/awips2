//------------------------------------------------------------------------------
// Lookup3 :: SetGet - Set/gets for data members.
//------------------------------------------------------------------------------
// History:
// 
// 01 Feb 2006  James R. VanShaar, RTi	Created initial version
//------------------------------------------------------------------------------
#include "Lookup3.h"
#include "ResJSys.h"

//------------------------------------------------------------------------------
// Lookup :: getReceivingComp - Returns the _receivingComp data member
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     Component _receivingComp
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
Returns the _receivingComp data member
@return _receivingComp
*/
Component * Lookup3 :: getReceivingComp( )
{
    return _receivingComp;
}

//------------------------------------------------------------------------------
// Lookup :: getMyValueAsOut - Applies _myValue to either a double representing
//                             inflows to or another representing outflows from 
//                             the owning component depending on the meaning of
//                             _myValue as defined by the _tableVar variable and
//                             the sign of _myValue.
//------------------------------------------------------------------------------
// This method is used in preparation for reducing outflow from the component as
// necessary to meet minimum constraints.
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Modifies one of the input parameters as appropriate to the meaning of _myValue.
@return 
@param in double pointer of inflow to the owning component by Lookup3 method(s).
@param out double pointer of outflow from the owning component by Lookup3
 and SetWithdraw method(s).
*/
void Lookup3 :: getMyValueAsOut( double *in, double *out )
{
    if( _tableVar == TVAR_AUGMENTATION )
    {
        if( _myValue >= 0 )
        {
            *in += _myValue;
        }
        else
        {
            *out -= _myValue;
        }
    }
    else
    {
        if( _myValue >= 0 )
        {
            *out += _myValue;
        }
        else
        {
            *in -= _myValue;
        }
    }
}

//------------------------------------------------------------------------------
// Lookup :: sendToTS - As necessary, applies withdrawal or diversion to another
//                      component under non-limited conditions.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     getType()
//     Component::getID()
//     ResJSys::getIPR()
//     ResJ_ccwrite( int*, char*, int* )
//     HourTS.setDataValue( TSDate, double )
//     TSDate.addInterval( int, int )
//     getForecastDate2()
// Errors:
//     None
// Warnings:
//     None
// Warnings:
//     None
// Debug:
//     If negative transfer to another component is attempted, a NOTE message
//     is produced and the transfer is revised to 0.0.
//------------------------------------------------------------------------------
/**
Transfers to another component.
@return 
@param date TSDate of current time step
*/
void Lookup3 :: sendToTS( TSDate date )
{
    // Check to see if the method really is sending to a time series.
    if( _receivingComp )
    {
        // Due to restrictions in Lookup3::construct, an AUGMENTATION-type
        // Lookup3 method will not arrive here.
        // This method may be a DIVERSION or WITHDRAWAL type with negative
        // values, however.
        // Method has to ensure that it is sending a non-negative value
        if ( _myValue < 0 )
        {
            char temp[MAXC];
            // Print a NOTE regarding the change, as per Design document
            sprintf( temp, "**NOTE** Negative ToComp diversion of %f on "
                "%s %s %s put on receiving Component as 0.0.", _myValue,
                getType(), _owner->getID(), _id );
            int length = strlen( temp );
            int ipr = ResJSys::getIPR(); 
            ResJ_ccwrite( &length, temp, &ipr );
            // Now revise _myValue to be non-negative so an erroneous state
            // is not saved in the carryover array.
            _myValue = 0.0;
        }

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
            // Otherwise, date extends beyond the timeseries.
            // Don't attempt to assign the value.
        }
    }
    // Otherwise, do nothing.
    // This will include any TVAR_AUGMENTATION parameterizations.
}

//------------------------------------------------------------------------------
// Lookup :: sendToTS - Scale calculated value, if an outflow, to meet minimum
//                      constraints then, apply withdrawal or diversion to
//                      another component, as necessary.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     getType()
//     Component::getID()
//     ResJSys::getIPR()
//     ResJ_ccwrite( int*, char*, int* )
//     HourTS.setDataValue( TSDate, double )
//     TSDate.addInterval( int, int )
//     getForecastDate2()
// Errors:
//     None
// Warnings:
//     None
// Warnings:
//     None
// Debug:
//     If negative transfer to another component is attempted, a NOTE message
//     is produced and the transfer is revised to 0.0.
//------------------------------------------------------------------------------
/**
Scale outflow to meet minimum constraints, then apply as necessary to another
 component.
@return 
@param date TSDate of current time step
@param scalar double multiplication factor to reduce planned withdrawal or
 diversion as necessary to comply with minimum constraints.
*/
void Lookup3 :: sendToTS( TSDate date, double scalar )
{
    // Check to see if the method really is sending to a time series.
    if( _receivingComp )
    {
        // Method is sending a value to another component's time series.

        // Due to restrictions in Lookup3::construct, an AUGMENTATION-type
        // Lookup3 method will not arrive here.
        // This method may be a DIVERSION or WITHDRAWAL type with negative
        // values, however.
        // Method has to ensure that it is sending a non-negative value
        if( _myValue >= 0 )
        {
            // Simply scale the value
            _myValue *= scalar;
        }
        else
        {
            char temp[MAXC];
            // Print a NOTE regarding the change, as per Design document
            sprintf( temp, "**NOTE** Negative ToComp diversion of %f on "
                "%s %s %s put on receiving Component as 0.0.", _myValue,
                getType(), _owner->getID(), _id );
            int length = strlen( temp );
            int ipr = ResJSys::getIPR(); 
            ResJ_ccwrite( &length, temp, &ipr );
            // Now revise _myValue to be non-negative so an erroneous state
            // is not saved in the carryover array.
            _myValue = 0.0;
        }

        // Now put the value onto the time series.
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
            // Otherwise, date extends beyond the timeseries.
            // Don't attempt to assign the value.
        }
    }
    else
    {
        // Method is not sending a value to another component's time series.

        // Determine if this method extracts water (diversion or withdrawal)
        // since not scale water entering.
        if( ( _tableVar == TVAR_AUGMENTATION && _myValue > 0 ) ||
            ( ( _tableVar == TVAR_DIVERSION || _tableVar == TVAR_WITHDRAWAL ) &&
            _myValue < 0 ) )
        {
            // Method is augmentation.  Scale nothing.
            // Note the above test is a duplication of code elsewhere
        }
        else
        {
            // Simply scale the value
            _myValue *= scalar;
        }
    }
}

//------------------------------------------------------------------------------
// Lookup :: setInactiveState - Reinitialize certain data members due to method
//                              inactivity at the current time step.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Reinitialize certain data members due to method inactivity.
@return 
*/
void Lookup3 :: setInactiveState()
{
    // Reinitialize as inactive any past blending variables
    _ts_step = _n_blend_ts + 4;
    _tbl_step = 1;

    // Note: PREVIOUSPOOL (a state value required as carry over is stored 
    //    as part of the reservoir info and / or time series.

    _Active = 0;
    _myValue = 0;
    _lastValKnown = 0;
    _iValKnown = 0;

    return;
}

//------------------------------------------------------------------------------
// Lookup :: setCOstring - Constructs the carryover string
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying successful construction
// Calls:
//      ResJSys::addCOString( char* )
//      PrintWarning( int, char*, char* )
// Errors:
//     None
// Warnings:
//     None
// Warnings:
//     Failure to addCOString
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Build the carryover string.
@return int specification of method success.
*/
int Lookup3 :: setCOstring( )
{
    char routine[]="Lookup3::setCOstring";
    char future[9]="*FUTURE*";
    char COtype[13]="METHOD";
    char tsStep_str[5], tblStep_str[5];
    char colI_str[5]="SKIP";    // Later trimmed down to 4 characters
    char rowI_str[5]="SKIP";    // Later trimmed down to 4 characters
    char lastVal_str[18];       // Later trimmed down to 8 characters
    char future_str[41], value_str[4+4+4+4+8+1];
    char temp_str[52+24+41+1];    // Labels, values, future space, end

    // Get current values (values at end of the current time step)
    sprintf( tsStep_str, "%d", _ts_step );
    sprintf( tblStep_str, "%d", _tbl_step );

    // Handle the column indexing value--required for blending across columns.
    // This is neccessary only if the user has parameterized a table blend
    // and last time step the method referenced the table.

    // Check to see if the method knows indexing values
    //     _iValKnown:
    //         0 = Not known
    //         1 = both are known
    if( _iValKnown )
    {
        sprintf( colI_str, "%d", _colI );
        sprintf( rowI_str, "%d", _rowI );
    }

    // Handle last calculated value.
    // NOTE that it is possible to have a valid value beginning with -999.
    // This will result in problems in constructing the carryover array.
    // Make special provision for that case.
    sprintf( lastVal_str, "%.8f", _myValue );
    lastVal_str[8] = '\0';
    if( !strncmp( lastVal_str, "-999", 4 ) )
    {
        // Replace with a keyword.
        strncpy( lastVal_str, "MISS", 4 );
    }

    // Prepare value portion of the string
    sprintf(value_str, "%4.4s%4.4s%4.4s%4.4s%8.8s", tsStep_str, tblStep_str, 
        colI_str, rowI_str, lastVal_str);

    // Prepare future place holders
    sprintf(future_str, "%8.8s%8.8s%8.8s%8.8s%8.8s", future,future,future,
        future,future);
    future_str[40] = '\0';

    // Prepare the full variable string
    COtype[6] = '\0';
    sprintf( temp_str, 
        "%-12.12s%-12.12s%4.4s%-12.12s%-12.12s%24.24s%40.40s",
        COtype, _id, "-999", "LOOKUP3", _owner->_id, value_str,
        future_str );

    // Write the string to the system CO string
    if( ResJSys::addCOString( temp_str ) ) {
        PrintWarning( 1, routine, 
            "Troubles adding carryover data to CO array." );
        return( STATUS_FAILURE );
    }

    return( STATUS_SUCCESS );
}

//------------------------------------------------------------------------------
// Lookup :: setCOstring - Constructs the carryover string
//------------------------------------------------------------------------------
// Simply calls the non-date-dependent setCOstring method.
//------------------------------------------------------------------------------
// Return: 
//     integer specifying successful construction
// Calls:
//      setCOstring()
// Errors:
//     None
// Warnings:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Build the carryover string.
@return int specification of method success.
@param cur_date TSDate current time step
*/
int Lookup3 :: setCOstring( TSDate& cur_date )
{
    //char routine[]="Lookup3::setCOstring";
    int success;

    // CO is date independent.  Therefore simply call the non-dated 
    //    setCOstring.
    
    success = setCOstring( );

    return( success );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Lookup3_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Lookup3_SetGet.cxx,v 1.1 2006/10/26 15:24:17 hsu Exp $";}
/*  ===================================================  */

}
