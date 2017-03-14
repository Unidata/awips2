//------------------------------------------------------------------------------
// LagK_solveMethod.cxx - Module for Lag and K calculation
//------------------------------------------------------------------------------
// Notes:
//     The lag occurs by working in the current timestep with inflows to the
//     reach which occured '_lag' hours (or other time unit ) and
//     '_lag' + '_t_mult' hours (or other time unit ) ago.
//
//------------------------------------------------------------------------------
// History:
//
// 23 Mar 1998  Daniel Weiler, Riverside Technology, inc
//                             Created initial version.
// 10 May 2001  James R. VanShaar, RTi  
//                             Revamped the algorithm ordering to more cleanly 
//                             deal with solutions that require carryover values.
// 11 May 2001  JRV, RTi        
//                             Corrected carryover loop to save
//                             appropriate number of values if _lag %
//                             _t_mult != 0.
// 12 Nov 2001  JRV, RTi        
//                             Removed CO string preparation to
//                             LagK_SetGet.cxx as LagK::setCOstring
// 03 Dec 2002  JRV, RTi        
//                             Revised non-variable K equation to
//                             eliminate instability introduced by
//                             earlier handling of K = 0.
// 04 Dec 2002  JRV, RTi        
//                             Revised to use _sizeInflowCO.
// 11 Dec 2002  JRV, RTi        
//                             Added assignment of _Active.
// 19 Feb 2004  JRV, RTi        
//                             Replaced calls to sumInflow with calls
//                             to getTotalInflow.
// 15 Mar 2006  Marc L. Baldo,  RTi 
//                             Completely revamped algorithm to match
//                             Lag/K operation #7 (flag7, fka7, fk7).
//                             The exception is carryover handling,
//                             where the RES-J LAGK method uses
//                             reach inflow values instead of lagged
//                             inflow values (as seen in NWSRFS
//                             LAG/K operation).
//------------------------------------------------------------------------------

/**
@author Daniel Weiler, RTi; James R. VanShaar, RTi; Marc L. Baldo, RTi
*/

#include "LagK.h"
#include "ResJSys.h"

//------------------------------------------------------------------------------
// ComputeSegmentRouting - Iterate to calculate routing for the passed segment
//------------------------------------------------------------------------------
// This routine computes the K values by iteration, with four possible results
//  - Success within 21 iterations, returning calculated outflow
//  - A decision to split the interval into quarters - 1/8 of orig interval
//  - If the interval is already split, and the interval is still too large
//    for the K calculated, set outflow = inflow
//  - Failure to converge within 21 iterations, returning the 
//    value at the 21st iteration
//------------------------------------------------------------------------------
// Compare this routine and the Line#: labels to fcmpk7.f.
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     If convergence is not achieved in 21 iterations, a the last
//     value is picked and the routine emits a warning.
// Debug:
//     If the debug level is at 10, iteration by iteration calculations
//     are printed by PrintDebug.
//------------------------------------------------------------------------------

/**
Iterate to calculate routing for the passed segment.
@param x1 The lagged inflow at the start of the timestep
@param x2 The lagged inflow at the end of the timestep t=now
@param y0 The outflow value at the start of the timestep
@param y1 The outflow value at the end of the timestep t=now
@param y2 The calculated Y value at the end of the timestep
@param xta The timestep size
@param qdt The flag for quarter time step, TRUE if quartering is required
*/

void LagK :: ComputeSegmentRouting ( double x1, double x2, double y0, double y1,
                                     double &y2, double xta, int &qdt )
{
    char *routine = "LagK :: ComputeSegmentRouting";
    double ye = ( ( 3.0 * y1 ) - y0 ) / 2.0 ;
    int knt = 0 ;

    // Maximum 21 iterations to get solution
    for ( knt = 0 ; knt <= 20 ; knt++ )
    {
        double xk = _out_k_tbl.lookup ( ye, KCOLUMN, ALLOW_BOUNDS ) ;
    
        // If constant k - i.e. if conk = true - and k lt routing
        // interval/4.0 - do not attenuate inflow.
Line10:
        if ( !( ( xk > xta/4.0 ) || ( _k_mode == VARIABLE_K ) ) )
        {
            y2 = x2 ;
            break ;
        }
    
        // If qdt true - i.e. if calling ComputeSegmentRouting from 
        // CalculateQuarterDT, already quartered the routing interval - 
        // so no more can be done.  Do not attenuate inflow.
        //
        // If qdt is false - i.e. if calling ComputeSegmentRouting from fk - 
        // set qdt = true which will cause CalculateQuarterDT to be called.
        if ( xk <= ( xta / 4.0 ) )
        {
            if ( qdt ) y2 = x2 ;
            qdt = TRUE ;
            break ;
        }
    
        // Store outflow in y2 and check convergence with ye.
Line20:
        double tr = 0.5 * xta ;
        y2 = ( tr * ( x1 + x2 ) + y1 * ( xk - tr ) ) / ( xk + tr ) ;
        if ( ( ( 1.02 * ye ) > y2 ) && ( ( 0.98 * ye ) < y2 ) ) break ;
        ye = y2 ;
        PrintDebug ( 10, routine, 
                       "KNT+1,TR,X1,X2,Y1,Y2 = %d,%lf,%lf,%lf,%lf,%lf\n", 
                       knt+1, tr, x1, x2, y1, y2 );
    }

    if ( knt >= 20 ) 
    {
        PrintWarning ( 1, routine, "RESJ-ComputeSegmentRouting - Did Not Converge ." ) ;
    }
}

//------------------------------------------------------------------------------
// Interpolate - Interpolate from table to get value
//------------------------------------------------------------------------------
// Compare this routine to fintp7.f.
//------------------------------------------------------------------------------
// Return: 
//     interpolated value
// Calls:
//     Table::lookup
// Errors:
//     None
// Warnings:
//     None
//------------------------------------------------------------------------------

/**
Interpolate from table to get value.
@return interpolated value
@param value position in the array tsTable to interpolate
@param tsTable table of 2 values that contains the value
*/

double LagK :: Interpolate ( double value, Table &sTable )
{
    double ratio =
       ( sTable.lookup ( 1, FLOWCOLUMN ) - sTable.lookup ( 0, FLOWCOLUMN ) ) /
       ( sTable.lookup ( 1, TIMECOLUMN ) - sTable.lookup ( 0, TIMECOLUMN ) ) ;

    return sTable.lookup ( 0, FLOWCOLUMN ) + ratio *
       ( value - sTable.lookup ( 0, TIMECOLUMN ) ) ;
}


//------------------------------------------------------------------------------
// CalculateFortWorthLoss - Calculate the Fort Worth Loss Recession
//------------------------------------------------------------------------------
// This routine is called only when MCP2 calculated K is used.  This routine 
// applies a factor to flows on the recession portion of the hydrograph when 
// the resulting flow is under the transmission loss level.
//------------------------------------------------------------------------------
// Compare this routine to ftwtl7.f.
//------------------------------------------------------------------------------
// Return: 
//     Q after Ft Worth LR calculation
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     When the debug level is set at 10, values entering and leaving are
//     printed by PrintDebug.
//------------------------------------------------------------------------------

/**
Calculate the Fort Worth Loss Recession.
@return Q after Ft Worth LRC calculation
@param qli Current lagged inflow (Q)
@param qprev Previous flow (Q)
@param qk Current calculated outflow
*/

double LagK :: CalculateFortWorthLoss ( double qli, double qprev, double qk )
{
    char *routine = "RESJ Lagk CalculateFortWorthLoss";
    double qtl = -999.;
    double q = qk;

    PrintDebug ( 10, routine, 
                 "Enter CalculateFortWorthLoss tlrc=%lf, qbntl=%lf, qli=%lf, "
                 "qprev=%lf, qk=%lf\n", _transLossCoef, _transLossLevel, qli, 
                 qprev, qk ) ;

    if ( qli < qprev )
    {
        qtl = qprev * _transLossCoef;
        if ( !( ( qtl >= qk ) || ( qtl <= _transLossLevel ) ) )
        {
            q = qtl;
        }
    }

    PrintDebug (10, routine, "Exit CalculateFortWorthLoss q=%lf, qtl=%lf\n", 
                q, qtl);
    return q;
}

//------------------------------------------------------------------------------
// CalculateQuarterDT - Calculate the outflow, using 1/4 sized DT
//------------------------------------------------------------------------------
// This routine takes the routing interval and quarters it, calculating the 
// outflow (Y2) after lagging and K.
//------------------------------------------------------------------------------
// Compare this routine to fqdt7.f.
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     LagK::ComputeSegmentRouting
// Errors:
//     None
// Warnings:
//     None
//------------------------------------------------------------------------------

/**
Calculate the outflow, splitting the passed interval into quarters.
@param x1 The lagged inflow at the start of the timestep
@param x2 The lagged inflow at the end of the timestep t=now
@param y0 The outflow value at the start of the timestep
@param y1 The outflow value at the end of the timestep t=now
@param y2 The calculated Y value at the end of the timestep
@param xta The timestep size
@param qdt The flag for quarter time step, TRUE if quartering is required
*/

void LagK :: CalculateQuarterDT ( double &x1, double &x2, double &y0, 
                                  double &y1, double &y2, double &xta, 
                                  int &qdt )
{
    char *routine = "RESJ Lagk CalculateQuarterDT";
    int i ;
    double x1t = 0 ;
    double y0t = 0 ;
    double xtat = xta / 4.0 ;
    double x2t = x1 ;
    double y1t = y0 * 0.25 + y1 * 0.75 ;
    double y2t = y1 ;

    // Loop through routing interval one quarter of the interval at a time.
    // If k is still greater than routing interval do not attenuate this
    // interval.
    for ( i = 1 ; i <= 4 ; i++ )
    {
        double frac = i / 4.0;
        x1t = x2t;
        x2t = x1 * ( 1.0 - frac ) + ( x2 * frac ) ;
        y0t = y1t ;
        y1t = y2t ;

        ComputeSegmentRouting ( x1t, x2t, y0t, y1t, y2t, xtat, qdt ) ;
    }

    y2 = y2t ;
    qdt = FALSE ;
}

//------------------------------------------------------------------------------
// solveAtlantaK - Use the Atlanta K algorithm to solve the K routing
//------------------------------------------------------------------------------
// This routine solves the Atlanta K method.  This method handles non-linear
// and non-unique K versus Time relationships.  AtlantaK depends on a 
// previously defined Outflow versus storage table, both at the normal xita
// time step and at the 1/4 xita timestep.  The routines that must be
// called before this routine are:  
//    makeOutflowVSStorageTableQuarter and makeOutflowVSStorageTable
//------------------------------------------------------------------------------
// Compare this routine and the Line#: labels to fka7.f.
//------------------------------------------------------------------------------
// Return: 
//     Attenuated flow for this time step
// Calls:
//     Table::lookup
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     When the debug level is 10, the calculations for each time step
//     are printed by PrintDebug.
//------------------------------------------------------------------------------

/**
Calculate the outflow, splitting the passed interval into quarters.
@return Attenuated flow for this time step
@param LagdQin1 The lagged flow at t-1
@param LagdQin2 The lagged flow at t
@param group_val The group values storage location - note that other routines 
in the RESJ-Lag/K require that group_val be unset (not in a combo method)
*/

double LagK :: solveAtlantaK ( double LagdQin1, double LagdQin2, 
                               double** group_val )
{
    double dx = 0. ;              // Delta X
    double dxovr4 = 0. ;          // Delta X / 4
    double fact = 1.0  ;          // Current factor, either 1.0 or 0.25
    int i = 0 ;                   // Counter variable
    int ipwarn = 0 ;              // >0 when K < 1/2 dt
    int j = 0 ;                   // Counter variable
    char   routine[] = "LagK :: solveAtlantaK" ; // ID for PrintWarning/Error
    double s2Odt = 0. ;           // Storage term
    double value = 0. ;           // Right hand side of routing equation
    double x1 = 0. ;              // Initial inflow (post lag)
    double x14 = 0. ;             // Initial inflow for current quarter
    double x2 = 0. ;              // Final inflow
    double xita = 0. ;            // Time step
    double xitao2 = 0. ;          // Time step/2
    double xk1 = 0. ;             // K for first half segment
    double xk14 = 0. ;            // K for first half of current quarter segment
    double xk2 = 0. ;             // K for first half segment
    double xk24 = 0. ;            // K for first half of current quarter segment
    double y1 = 0. ;              // Initial outflow
    double y2 = 0. ;              // Solved outflow

    // Constant K is handled by providing this routine a short K table.
    xita = _t_mult / 4. ;

    y1 = _owner->_outflow ;              // Carryover value
    s2Odt = _storageCO * 2.0 / _t_mult ; // Carryover Value

    x1 = LagdQin1 ;
    x2 = LagdQin2 ;

    // value is rhs_value
    value = x1 + x2 + s2Odt - y1 ;
    if ( value < 1.0e-7 ) value = 0.0 ;

    // Determine the initial outflow value
    y2 = _stor_out_tbl.lookup ( value, STOR_OUTFLOWCOLUMN, ALLOW_BOUNDS ) ;

    // Do a K lookup for left and right halves
    xk1 = _out_k_tbl.lookup ( y1, KCOLUMN, ALLOW_BOUNDS ) ;
    xk2 = _out_k_tbl.lookup ( y2, KCOLUMN, ALLOW_BOUNDS ) ;

    PrintDebug ( 10, routine, "x1, x2, value, y2, xk1, xk2, y1  = "
        "x1=%.2lf, %.2lf, %.2lf, y2=%.2lf, %.2lf, %.2lf,y1= %.2lf\n", 
        x1, x2, value, y2, xk1, xk2, y1 ) ;

    // Determine whether left and right K values are both within
    // timestep/2, are both above timestep/2, or one is inside and
    // one is outside.  In the last case, split the region into
    // quarters and calculate again.
    if ( ( xk1 < xita / 2.0 ) && ( xk2 < xita / 2.0 ) )
    {
        // get here if k for y1 and k for y2 are both < dt/2
        // set outflow = minimum of (inflow,value)
        y2 = x2 ;
        if ( value < y2 )
        {
            y2 = value ;
        }
        goto Line9 ;
    }
    else if ( ( xk1 >= xita * 2.0 ) && ( xk2 >= xita * 2.0 ) )
    {
        goto Line9 ;
    }
    else if ( ( xk1 > xita / 2.0 ) || ( xk2 > xita / 2.0 ) )
    {
        // Do this code if K for y1 is greater than dt/2 OR K for y2 <= dt/2
        // solve equations in this loop with dt=(original dt)/4
        s2Odt = s2Odt * 4.0 ;
        if ( s2Odt < -0.5 ) ipwarn++ ;
        dx = x2 - x1 ;
        
        dxovr4 = dx / 4.0 ;
        xitao2 = xita / 2.0 ;

        // Fortran loop called "DO 5"
        // This loop calculates across 1/4 time steps, performing the 
        // same K lookups as the non-quarter time algorithm does.
        for ( j = 0 ; j < 4 ; j++ )
        {
            x14 = x1 + ( j * dxovr4 ) ;
            x2 = x14 + dxovr4 ;
            
            value = x14 + x2 + s2Odt - y1 ;
            
            y2 = _stor_out_tbl4.lookup ( value, STOR_OUTFLOWCOLUMN, 
                                         ALLOW_BOUNDS ) ;
            
            // Look up K at y1 and y2
            if ( _k_mode == VARIABLE_K )
            {
                xk14 = _out_k_tbl.lookup ( y1, KCOLUMN, ALLOW_BOUNDS ) ;
                xk24 = _out_k_tbl.lookup ( y2, KCOLUMN, ALLOW_BOUNDS ) ;
            }
            
            if ( xk14 < xitao2 || xk24 < xitao2 )
            {
                // if either k for y1 or k for y2 is still < new dt/2 
                // (i.e. original dt/8) set outflow = min (inflow, value) 
                // and continue in quarter period loop.
                y2 = x2 ;
                if ( value < y2 ) 
                {
                    y2 = value ;
                }
            }
            
            s2Odt = value - y2 ;
            if ( s2Odt < -0.5 )
            {
                ipwarn++;
            }


            PrintDebug ( 10, routine,  "j, x14, x2, value, y2, s2Odt -- "
                "%d, %.2lf, %.2lf, %.2lf, %.2lf, %.2lf\n", 
                j, x14, x2, value, y2, s2Odt ) ;
            y1 = y2;
        }
        
        fact = 4.0;
    }

Line9:
    _owner->_outflow = y2 ;
    s2Odt = ( value - y2 ) / fact;
    if ( s2Odt < -0.5 ) 
    {
        ipwarn++;
    }
    fact = 1.0;

    y1 = y2;

    // If it is not in a combo method, it is OK to set members for carryover.
    if ( !group_val )
    {
        _storageCO = s2Odt * _t_mult / 2.0;
        _laggedInflow  = x2 ;
    }

    return y2;
}

//------------------------------------------------------------------------------
// solveMCP2K - Use the MCP2 K algorithm to solve the K routing
//------------------------------------------------------------------------------
// This routine solves the MCP2 K method.  This method does not handle
// non-unique K versus Time relationships.  This method is only called
// when the Fort Worth Loss Recession coefficients are non-zero.
//------------------------------------------------------------------------------
// Compare this routine and the Line#: labels to fk7.f.
//------------------------------------------------------------------------------
// Return: 
//     Attenuated flow for this time step
// Calls:
//     LagK::ComputeSegmentRouting
//     LagK::CalculateQuarterDT 
//     LagK::CalculateFortWorthLoss 
// Errors:
//     None
// Warnings:
//     None
//------------------------------------------------------------------------------

/**
Calculate the outflow, splitting the passed interval into quarters.
@return Attenuated flow for this time step
@param LagdQin1 The lagged flow at t-1
@param LagdQin2 The lagged flow at t
@param group_val The group values storage location - note that other 
routines in the RESJ-Lag/K require that group_val be unset (not in a 
combo method)
*/

double LagK :: solveMCP2K ( double LagdQin1, double LagdQin2, 
                            double** group_val )
{
    int qdt = FALSE ;                // Quarter flag
    double qprev = _owner->_outflow; // last outflow for Fort Worth calc
    double x1 = LagdQin1 ;           // lagged inflow (t-1)
    double x2 = LagdQin2 ;           // lagged inflow (t)
    double xta = _t_mult / 2.0 ;     // Time step for K operation
    double y1 = _owner->_outflow ;   // last outflow
    double y0 = _storageCO;          // last storage
    double y12 = 0. ;                // Average across current segment
    double y2 = 0 ;                  // Attenuated outflow

    // perform attenuation on inflow time series.
    // actually route at one half original interval - but only store
    // outflow at whole intervals.

    double x12 = ( x1 + x2 ) / 2.0 ;
    double y01 = ( y0 + y1 ) / 2.0 ;

    if ( _k_mode == VARIABLE_K )
    {
    
        // Compute once for each half routing interval with variable k.  
        // If k < (half interval)/4.0 - call CalculateQuarterDT which routes 
        // at one quarter of current routine interval - i.e. at one eighth 
        // of whole (original) routing interval.
    
        ComputeSegmentRouting ( x1, x12, y01, y1, y12, xta, qdt ) ;
    
        if ( qdt ) 
        {
            CalculateQuarterDT ( x1, x12, y01, y1, y12, xta, qdt ) ;
        }
    
        ComputeSegmentRouting ( x12, x2, y1, y12, y2, xta, qdt ) ;
    
        if ( qdt ) 
        {
            CalculateQuarterDT ( x12, x2, y1, y12, y2, xta, qdt ) ;
        }
    }
    else
    {
Line20:
        // Compute once for each half routing interval with constant k.
        ComputeSegmentRouting ( x1, x12, y01, y1, y12, xta, qdt ) ;
        ComputeSegmentRouting ( x12, x2, y1, y12, y2, xta, qdt ) ;
    }


Line90:
    // If a transmission loss coefficient exists, update y2.
    if ( _transLossCoef > 0. )
    {
        y2 = CalculateFortWorthLoss ( x2, qprev, y2 ) ;
    }

    // Store carryover values for k operation.
    if ( !group_val )
    {
        _storageCO = y1 ;
        _laggedInflow = x2 ;
    }

    return y2;
}

//------------------------------------------------------------------------------
// solveLag - Solve the lag for both variable and constant lagging
//------------------------------------------------------------------------------
// This routine solves Lag with the ability to have non-linear and 
// non-unique relationships between lag and inflow.
//------------------------------------------------------------------------------
// Compare this routine and the Line#: labels to flag7.f.
//------------------------------------------------------------------------------
// Return: 
//     Attenuated flow for this time step
// Calls:
//     Table::allocateDataSpace
//     Table::lookup
//     Table::populate
// Errors:
//     When interpolation fails, an error message is printed.
// Warnings:
//     None
//------------------------------------------------------------------------------

/**
Calculate the outflow, splitting the passed interval into quarters.
@return Attenuated flow for this time step
@param cur_date The current date being solved
@param group_val Non-null if inside a combo method - note that other 
routines in the RESJ-Lag/K require that group_val be unset (not in a 
combo method)
*/

double LagK :: solveLag ( TSDate& cur_date, double** group_val )
{
    _Active = 1 ;

    // Note: A goodly portion of this code has been copied to
    // LagK_SetGet.cxx.  Changes here need to be duplicated there.
    bool   add = TRUE ;      // State variable - inside/outside of lagged TS
    Table  calcTable ;       // Q,T pairs, after repeats/missing are removed
    double currentFlow = 0. ;// Flow variable used for lagging calcTable
    TSDate date1 ;           // date of the last carryover
    int    i ;               // loop counter
    int    j ;               // loop counter
    Table  laggedTable ;     // The lagged Q,T pairs using lag and calcTable
    double lagHours = 0. ;   // calculated Lag, either constant or variable
    int    lastJ = 0 ;       // Last J value where a function hit was calculated
    double lastT = 0.0 ;     // Last T value where a function hit was calculated
    int    lastValue = 0 ;   // The ending position of a string of repeats
    int    nMissing = 0 ;    // The number of missing values in the Q,T pairs
    //int    nIntervals = 0 ;  // nIntervals available from getTotalInflow
    double nValues = 0.0 ;   // nValues in a string of repeats
    double qtj = 0.0 ;       // Time T at current j
    double qtj1 = 0.0 ;      // Time T at j - 1
    double qtj2 = 0.0 ;      // Time T at j + 1
    int    requiredCO ;      // Number of Q,T pairs in either calcTable or
                             // laggedTable, whichever is in use
    double result ;          // Results of the lagging, at one timestep back and
                             // at the current timestep
    char   routine[] = "LagK :: solveLag" ; // ID for PrintWarning/Error
    Table  smallTable ;      // Table used for interpolation when qtj <> tStar
    double timeHours = 0. ;  // Time counter = T in Q,T initial pairs
    double valueSum = 0.0 ;  //
    double tStar = 0.0 ;     // Time T where Q is sampled from laggedTable

    smallTable.allocateDataSpace ( 2 ) ;

    requiredCO = _sizeInflowCO ;
    laggedTable.allocateDataSpace ( requiredCO + 1 ) ;

    // Populate laggedTable with the current carryover values, based on
    // carryover from last time's date.
    getCarryOverValues ( cur_date , laggedTable ) ;

    // Lag the values in the working array
    for ( i = 0 ; i < requiredCO ; i++ )
    {
        currentFlow = laggedTable.lookup ( i, GETCOLUMN_1 ) ;
        timeHours = laggedTable.lookup ( i, GETCOLUMN_2 ) ;
        lagHours = _lag ;
        if ( _lag_mode == VARIABLE_LAG )
        {
            lagHours = _in_lag_tbl.lookup ( currentFlow, GETCOLUMN_2, 
                                            ALLOW_BOUNDS ) ;
        }

        laggedTable.populate ( i, GETCOLUMN_2, timeHours + lagHours ) ;
    }

    // If any subsequent lagged values are the same and not missing, 
    // average them and set the remainder to missing.  
    // Keep track of the number of missing.
    for ( i = 0 ; i < requiredCO - 1 ; i++ )
    {
        if ( ( laggedTable.lookup ( i, TIMECOLUMN ) == 
               laggedTable.lookup ( i + 1, TIMECOLUMN ) ) &&
             ( laggedTable.lookup ( i, FLOWCOLUMN ) > 0 ) )
        {
            valueSum = laggedTable.lookup ( i, FLOWCOLUMN ) ;
            nValues = 1.0 ;
            for ( j = i ; j < requiredCO - 1 ; j++ )
            {
                if ( laggedTable.lookup ( j, TIMECOLUMN ) == 
                     laggedTable.lookup ( j + 1, TIMECOLUMN ) )
                {
                    valueSum += laggedTable.lookup ( j + 1, FLOWCOLUMN ) ;
                    nValues += 1.0 ;
                    // set flow to missing
                    laggedTable.populate ( j, FLOWCOLUMN, -999 ) ; 
                    lastValue = j + 1 ;
                }
                else
                {
                    break ;
                }
            }
            if ( nValues > 0 )
            {
                laggedTable.populate ( lastValue, FLOWCOLUMN, 
                                       valueSum / nValues ) ;
            }
        }

        if ( laggedTable.lookup ( i, FLOWCOLUMN ) < 0 )
        {
            nMissing++ ;
        }
    }

    // Remove all missing values by copying non-missing to calcTable
    calcTable.allocateDataSpace ( requiredCO - nMissing ) ;
    nMissing = 0 ;
    for ( i = 0 ; i < requiredCO ; i++ )
    {
        if ( laggedTable.lookup ( i, FLOWCOLUMN ) > -998.5 )
        {
            calcTable.populate ( i - nMissing, FLOWCOLUMN,
            laggedTable.lookup ( i, FLOWCOLUMN ) ) ;
            calcTable.populate ( i - nMissing, TIMECOLUMN,
            laggedTable.lookup ( i, TIMECOLUMN ) ) ;
        }
        else
        {
            nMissing++ ;
        }
    }
    requiredCO -= nMissing ;
    laggedTable.freeDataSpace ( ) ;

    // Loop through the array, checking for double backs
    int doubleBack = 0 ;
    for ( i = 0 ; i < requiredCO - 1 ; i++ )
    {
        if ( calcTable.lookup ( i, TIMECOLUMN ) >= 
             calcTable.lookup ( i + 1, TIMECOLUMN ) )
        {
            doubleBack++ ;
        }
    }

    // If no double backs, perform lookup to get the value at t = 0
    if ( !doubleBack )
    {
        result = calcTable.lookup ( 0.0, FLOWCOLUMN, ALLOW_BOUNDS ) ;
    }
    else // otherwise, calculate resulting flow "inside" curve
    {
        // The blocks of code in this code are based on the NWSRFS code
        // For the most complex portions of the code, the line numbers from
        // the fortran code were retained as labels.  All of the if...goto
        // statements were changed if possible to remove the goto, using
        // if/else instead.

        lastT = 0.0 ;
        lastJ = 0 ;
        add = TRUE ;

        // Solve at a single time step, t = 0
        tStar = 0.;
        i = 1;
        result = 0.0 ;
        for ( j = 0 ; j < requiredCO ; j++ )
        {
            qtj = calcTable.lookup ( j, TIMECOLUMN ) ;

            // Check if exactly at tStar, meaning no interp required
            if ( qtj == tStar )
            {

                // If at first position, avoid using qtj1
                if ( j == 0 )
                {
                    // If table is only sized at 1, avoid using qtj2
                    if ( j >= requiredCO - 1 ) // rarely true
                    {
                        result += calcTable.lookup ( j, FLOWCOLUMN ) ;
                        goto Line295 ;
                    }

                    // Function is moving from left to right --> Add
                    qtj2 = calcTable.lookup ( j + 1, TIMECOLUMN ) ;
                    if ( qtj2 > tStar )
                    {
                        result += calcTable.lookup ( j, FLOWCOLUMN ) ;
                        add = TRUE ;
                        continue ;
                    }

                    // Function is moving from right to left --> Subtract
                    if ( qtj2 < tStar )
                    {
                        result -= calcTable.lookup ( j, FLOWCOLUMN ) ;
                        add = FALSE ;
                        lastJ = j;
                        lastT = tStar ;
                        continue ;
                    }
                    continue ;
                }
Line247:
                // If at last pair, avoid using qtj2
                qtj1 = calcTable.lookup ( j - 1, TIMECOLUMN ) ;
                if ( j >= requiredCO - 1 )
                {

#if 0
                    // The code in flag7.f contains this block.
                    //
                    // This code matches code in flag7.f, but because
                    // this code has problems, it is ifdef'ed out.
                    //
                    // This code causes incorrect solutions for point 
                    // hits on the last point of the solved time series.
                    // Because RES-J solves a time step at a time, 
                    // this problem hits at almost every time step.
                    // 
                    // This is a change in algorithm, because the
                    // original algorithm represented in flag7.f had
                    // times when it either ignored the last point or
                    // averaged it when carryover values were removed.
                    if ( qtj1 > tStar )
                    {
                        result -= calcTable.lookup ( j - 1, 
                                                         FLOWCOLUMN ) ;
                        add = FALSE ;
                        lastJ = j ;
                        lastT = tStar ;
                        continue ;
                    }
#endif

Line248:
                    // The function is moving from left to right --> Add
                    if ( qtj1 < tStar )
                    {
                        result += calcTable.lookup ( j - 1, 
                                                         FLOWCOLUMN ) ;
                        add = TRUE ;
                        continue ;
                    }
                    continue;
                }

Line251:
                // qtj == tStar and in the middle of calcTable
                // The function is moving through qtj from
                // left to right --> Add
                qtj2 = calcTable.lookup ( j + 1, TIMECOLUMN ) ;
                if ( ( qtj1 < tStar ) && ( qtj2 > tStar ) )
                {
                    result += calcTable.lookup ( j, FLOWCOLUMN ) ;
                    add = TRUE ;
                    continue ;
                }
Line252:
                // The function is moving through qtj from
                // right to left --> Subtract
                if ( ( qtj1 > tStar ) && ( qtj2 < tStar ) )
                {
                    result -= calcTable.lookup ( j, FLOWCOLUMN ) ;
                    add = FALSE ;
                    lastJ = j ;
                    lastT = tStar ;
                    continue ;
                }
                continue ;
            }
Line250:
            // Cannot interpolate - the segment is has only one point.
            if ( j == requiredCO - 1 )
            {
                continue ;
            }

            // if qtj < tStar and qtj2 > tStar, add current flow
            // if at the end of the qt array, continue
            qtj2 = calcTable.lookup ( j + 1, TIMECOLUMN ) ;

            // When tStar is betwen qtj and qtj2, and the function
            // is moving from left to right --> Add
            if ( ( qtj < tStar ) && ( qtj2 > tStar ) )
            {
                smallTable.populate ( 0, FLOWCOLUMN, 
                                calcTable.lookup ( j, FLOWCOLUMN ) ) ;
                smallTable.populate ( 1, FLOWCOLUMN, 
                                calcTable.lookup ( j + 1, FLOWCOLUMN ) ) ;
                smallTable.populate ( 0, TIMECOLUMN, 
                                calcTable.lookup ( j, TIMECOLUMN ) ) ;
                smallTable.populate ( 1, TIMECOLUMN, 
                                calcTable.lookup ( j + 1, TIMECOLUMN ) ) ;
                result += Interpolate ( tStar, smallTable ) ;
                add = TRUE ;
                continue ;
            }
Line260:
            // When tStar is betwen qtj2 and qtj, and the function
            // is moving from right to left --> Subtract
            if ( ( qtj > tStar ) && ( qtj2 < tStar ) )
            {
                smallTable.populate ( 0, FLOWCOLUMN, 
                                calcTable.lookup ( j, FLOWCOLUMN ) ) ;
                smallTable.populate ( 1, FLOWCOLUMN, 
                                calcTable.lookup ( j + 1, FLOWCOLUMN ) ) ;
                smallTable.populate ( 0, TIMECOLUMN, 
                                calcTable.lookup ( j, TIMECOLUMN ) ) ;
                smallTable.populate ( 1, TIMECOLUMN, 
                                calcTable.lookup ( j + 1, TIMECOLUMN ) ) ;
                result -= Interpolate ( tStar, smallTable ) ;
                add = FALSE ;
                lastJ = j;
                lastT = tStar ;
                continue ;
            }
        }

Line295:
        // If the function subtracted last, add the function at lastT.
        if ( !add )
        {
            if ( calcTable.lookup ( j, TIMECOLUMN ) > 
                 calcTable.lookup ( j + 1, TIMECOLUMN ) )
            {
                PrintError ( routine, "Bad interpolation j=%d, .", 
                             cur_date.toString ( ) ) ;
            }
            smallTable.populate ( 0, FLOWCOLUMN, 
                            calcTable.lookup ( lastJ, FLOWCOLUMN ) ) ;
            smallTable.populate ( 1, FLOWCOLUMN, 
                            calcTable.lookup ( lastJ + 1, FLOWCOLUMN ) ) ;
            smallTable.populate ( 0, TIMECOLUMN, 
                            calcTable.lookup ( lastJ, TIMECOLUMN ) ) ;
            smallTable.populate ( 1, TIMECOLUMN, 
                            calcTable.lookup ( lastJ + 1, TIMECOLUMN ) ) ;
            result += smallTable.lookup ( lastT, FLOWCOLUMN, 
                                              ALLOW_BOUNDS ) ;
        }
    }

    return result;
}

//------------------------------------------------------------------------------
// solveMethod - Solve Lag and K for this reach
//------------------------------------------------------------------------------
// This routine calls the lag and K methods to lag and attenuate the 
// flow coming into the reach.  This routine produces a single outflow
// and calculates flows that are carried over for future LagK calculations.
//------------------------------------------------------------------------------
// Return: 
//     Overall Success or Failure
// Calculations:
//     This method updates _outflow with the results of Lag and K on the inflow
//     values.
// Calls:
//     LagK::solveLag
//     LagK::solveAtlantaK
//     LagK::solveMCP2K
// Errors:
//     If the method is inside a combo method, an error message is printed
//     and this method returns failure.
// Warnings:
//     None
// Debug:
//     When the debug level is 5, the lagged inflow and outflow are printed.
//------------------------------------------------------------------------------

/**
Calculate the outflow, splitting the passed interval into quarters.
@return Success or failure of the routine
@param cur_date The current date being solved
@param isPrimarySolution Not used
@param group_val Non-null if are inside a combo method - note that this
routine requires that group_val be unset (not in a combo method).
*/

int LagK :: solveMethod ( TSDate& cur_date, int isPrimarySolution,
        double** group_val )
{
    char   routine[] = "LagK :: solveMethod" ; // ID for PrintWarning/Error
    double LagdQin1 = 0.0 ;  // Calulated lagged outflow before K (t-1) timestep
    double LagdQin2 = 0.0 ;  // Calulated lagged outflow before K (t) timestep
    double Qout1 = 0.0;      // Outflow after K (t-1) timestep
    double Qout2 = 0.0;      // Outflow after K (t) timestep

    // The K routines require two lagged inflow values.  The solved
    // value is at position 1, the other value is in carryover.
    LagdQin1 = _laggedInflow ;
    LagdQin2 = solveLag ( cur_date, group_val ) ;

    // Qout1 is the outflow calculated from the last timestep
    // Qout2 is either the result of lagging or the result of K
    Qout1 = _owner->_outflow ;
    Qout2 = LagdQin2 ;

    // Choose algorithm, we prefer Atlanta, except when doing
    // the Fort Worth Transmission Loss Recession calculation
    if ( _k_mode == VARIABLE_K && _transLossCoef == 0)
    {
        Qout2 = solveAtlantaK ( LagdQin1, LagdQin2, group_val ) ;
    }
    else if ( _k_mode == VARIABLE_K && _transLossCoef > 0)
    {
        Qout2 = solveMCP2K ( LagdQin1, LagdQin2, group_val ) ;
    }

    // If Qout1 is still missing, then we are in the first timestep.
    // We assume outflow was the same as the inflow at the oldest inflow
    // ( LagdQin1 ) .  It is now included in carryover 11/27/01.
    if ( Qout1 == MISSING )
    {
        Qout1 = LagdQin1 ;
    }

    // Only set the _owner members if calling function is not
    // a ComboMethod.
    if ( !group_val )
    {
        _owner->_outflow = Qout2 ;
    }
    else
    {
        *group_val[0] = Qout2 ;
        PrintError ( routine, "Using LagK within a ComboMethod is "
                              "not currently supported." ) ;
        return ( STATUS_FAILURE ) ;
    }

    PrintDebug ( 5, routine, "Current date: %s  QI1:  %f  QI2:  %f",
                             cur_date.toString ( ) , LagdQin1, LagdQin2 ) ;
    PrintDebug ( 5, routine, "Setting outflow on %s to %f.",
                             _owner->_id, Qout2 ) ;

    return ( STATUS_SUCCESS ) ;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_solveMethod.cxx,v 1.7 2006/10/26 15:23:02 hsu Exp $";}
/*  ===================================================  */

}

