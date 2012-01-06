//------------------------------------------------------------------------------
// makeStoragVSOutflowTable - Create storage versus outflow table
//------------------------------------------------------------------------------
// Create 2S/dt+O2 vs O and 2S/(dt/4)+O2 vs O table used in Atlanta version 
// of K attenuation calculations (like pina7.f).
//------------------------------------------------------------------------------
// History:
// 
// 24 Mar 1998  Daniel Weiler, Riverside Technology, inc
//     Created initial version.
// 12 Jan 2003  James R. VanShaar, RTi  
//     Added enhancement of table to be consistent with the value
//     calcuated as _n_OutStorVals.  Based on LagK operation.
// 03 Feb 2006  Marc L. Baldo, RTi      
//     Revamped to handle dt/4 and not strictly increasing K in the 
//     outflow versus K table.
//------------------------------------------------------------------------------

#include "LagK.h"

//------------------------------------------------------------------------------
// makeStorageVSOutflowTableBase - Calculate 2S/dt+O2 versus O2 table
//------------------------------------------------------------------------------
// This routine takes the Outflow_K table and samples it, creating a 
// 2S/dt+O2 versus O2 table.  The atlanta routing method requires this 
// table to calculate.
//------------------------------------------------------------------------------
// Return:
//     Success or failure
// Calls:
//     Table::allocateDataSpace()
//     Table::lookup()
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     If the debug level is at 10, iteration by iteration calculations
//     are printed by PrintDebug.
//------------------------------------------------------------------------------

/**
Calculate 2S/dt+O2 versus O2 table.
@return success or failure
@param outTable resulting table
@param divisor either 1.0 or 4.0 representing whether table is using full or 1/4
dt values
*/

int LagK :: makeStorageVSOutflowTableBase ( Table &outTable, double divisor ) 
{
    // This routine will loop through and construct a O2 vs.
    // 2S/dt+O2 table for as many O2 values as specified in the 
    // _k_out_tbl.

    double c1 = 12 ;          // Constant for intermediate points
    double c2 = 100 ;         // Constant for intermediate points
    double deltaK = 0.0 ;     // Delta K for i and i+1
    double deltaQ = 0.0 ;     // Delta Q for i and i+1
    int i ;                   // loop variable
    int ipart = 0 ;           // loop variable for intermediate point
    int isegs = 0 ;           // total number of intermediate points
    int npkq = _n_kval ;      // number of k vs. q pairs
    int nPos = 0 ;            // current number of K/Outflow pairs
    double q1 = 0.0 ;         // flow at i
    double q2 = 0.0 ;         // flow at i+1
    double qbar = 0.0 ;       // average flow across period
    Table result_table;       // Results storage before copy to outTable
    char routine[] =          // Routine name for error and warning messages
        "LagK :: makeStorageVSOutflowTable" ;
    double storage = 0.0 ;    // Storage term
    double xita = _t_mult / divisor; // time step

    PrintDebug ( 10, routine, "Lag-K Storage-Outflow Table &"
        " Outflow-K table." );

    // To avoid pre-counting, allocate maximum possible space for local
    // results table
    result_table.allocateDataSpace ( npkq * MAXISEGS + 2 );

    // q1/q2 are used in the algorithm in place of o1/o2 in the
    // external documentation.
    for ( i = 0 ; i < npkq ; i++ )
    {
        if ( i < npkq - 1 )
        {
            deltaK = fabs ( _out_k_tbl.lookup ( i, KCOLUMN ) - 
                            _out_k_tbl.lookup ( i + 1 , KCOLUMN ) ) ;
            deltaQ = fabs ( _out_k_tbl.lookup ( i, OUTFLOWCOLUMN ) - 
                            _out_k_tbl.lookup ( i + 1 , OUTFLOWCOLUMN ) ) ;
            
            isegs = 1 ;
            if ( deltaK ) isegs = (int)( ( ( deltaQ + ( c1 * deltaK ) ) / c2 ) +
                                         1.5 ) ;
            if ( isegs > MAXISEGS ) isegs = MAXISEGS ;
        
        
Line20:
            // For each sample point, interpolate to get the 2S/dt+O2 vs. 
            // O2 table.
            for ( ipart = 0 ; ipart < isegs ; ipart++ )
            {
                q2 = _out_k_tbl.lookup ( i, OUTFLOWCOLUMN ) + 
                     deltaQ * ipart / isegs ;
                result_table.populate ( nPos, STOR_OUTFLOWCOLUMN, q2 ) ;
                qbar = ( q2 + q1 ) / 2.0 ;
                storage = _out_k_tbl.lookup ( qbar, KCOLUMN, ALLOW_BOUNDS ) * 
                          ( q2 - q1 ) + storage ;
                result_table.populate ( nPos, STOR_SDTCOLUMN, 
                                        ( 2.0 * storage / xita ) + q2 ) ;

                q1 = q2 ;
                nPos++ ;
            }
        }
        else
        {
            q2 = _out_k_tbl.lookup ( i, OUTFLOWCOLUMN ) ;
            result_table.populate ( nPos, STOR_OUTFLOWCOLUMN, q2 ) ;
            qbar = ( q2 + q1 ) / 2 ;
            storage = _out_k_tbl.lookup ( qbar, KCOLUMN, ALLOW_BOUNDS ) * 
                      ( q2 - q1 ) + storage ;
            result_table.populate ( nPos, STOR_SDTCOLUMN, 
                                   ( 2.0 * storage / xita ) + q2 ) ;

            q1 = q2 ;
            nPos++ ;
        }
    }

Line100:
    result_table.populate ( nPos, STOR_OUTFLOWCOLUMN, q2 ) ;
    qbar = ( q2 + q1 ) / 2 ;
    storage = _out_k_tbl.lookup ( qbar, KCOLUMN, ALLOW_BOUNDS ) * ( q2 - q1 ) + 
              storage ;
    result_table.populate ( nPos, STOR_SDTCOLUMN, ( 2.0 * storage / xita ) + 
                                                  q2 ) ;

    // Verify this - A table without zero should return a non-zero value
    // or a failure for this to work.
    if ( result_table.lookup ( nPos, STOR_SDTCOLUMN, ALLOW_BOUNDS ) == 0.0 ) 
    {
        goto Line200;
    }

    // Add a (0, 0) point to the result_table.  Allocate and copy because 
    // table resizing is not available.

    if ( ( result_table.lookup ( 0, STOR_SDTCOLUMN ) > 0.01 ) || 
         ( result_table.lookup ( 0, STOR_OUTFLOWCOLUMN ) > 0.01 ) )
    {
        for ( i = nPos - 1 ; i >= 0 ; i-- )
        {
            result_table.populate ( i + 1, STOR_SDTCOLUMN, 
                result_table.lookup ( i, STOR_SDTCOLUMN ) ) ;
            result_table.populate ( i + 1, STOR_OUTFLOWCOLUMN, 
                result_table.lookup ( i, STOR_OUTFLOWCOLUMN ) ) ;
        }
        result_table.populate ( 0, STOR_SDTCOLUMN, 0.0 ) ;
        result_table.populate ( 0, STOR_OUTFLOWCOLUMN, 0.0 ) ;
        nPos++;
    }

Line200:
    q2 = 1.0e+6 ;
    result_table.populate ( nPos, STOR_OUTFLOWCOLUMN, q2 ) ;
    qbar = ( q2 + q1 ) / 2 ;
    storage = _out_k_tbl.lookup ( qbar, KCOLUMN, ALLOW_BOUNDS ) * ( q2 - q1 ) +
              storage ;
    result_table.populate ( nPos, STOR_SDTCOLUMN, ( 2.0 * storage / xita ) + 
                            q2 ) ;
    nPos++;


    // For the final result, copy to the outTable
    outTable.allocateDataSpace( nPos ) ;
    for ( i = 0 ; i < nPos ; i++ )
    {
        outTable.populate ( i, STOR_OUTFLOWCOLUMN, 
                            result_table.lookup ( i, STOR_OUTFLOWCOLUMN ) ) ;
        outTable.populate ( i, STOR_SDTCOLUMN, 
                            result_table.lookup ( i, STOR_SDTCOLUMN ) ) ;
        PrintDebug ( 10, "", "Stor:  %f  Out:  %f",
            outTable.lookup ( i, STOR_SDTCOLUMN ),
            outTable.lookup ( i, STOR_OUTFLOWCOLUMN ) ) ;
    }
    
    return ( STATUS_SUCCESS );
}


//------------------------------------------------------------------------------
// makeStorageVSOutflowTable - Use makeStorageVSOutflowTableBase with DT=1.0.
//------------------------------------------------------------------------------
// Return:
//     Success or failure
// Calls:
//     LagK::makeStorageVSOutflowTableBase()
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Use makeStorageVSOutflowTableBase with DT=1.0.
@return success or failure
*/

int LagK :: makeStorageVSOutflowTable (  ) 
{
    int rValue = 0 ;
    rValue = makeStorageVSOutflowTableBase ( _stor_out_tbl, 1.0 ) ;
    _n_OutStorVals = _stor_out_tbl.getNRows ( ) ;
    return rValue ;
}

//------------------------------------------------------------------------------
// makeStorageVSOutflowTable - Use makeStorageVSOutflowTableBase with DT=0.25
//------------------------------------------------------------------------------
// Return:
//     Success or failure
// Calls:
//     LagK::makeStorageVSOutflowTableBase()
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Use makeStorageVSOutflowTableBase with DT=0.25
@return success or failure
*/

int LagK :: makeStorageVSOutflowTableQuarter ( ) 
{
  return ( makeStorageVSOutflowTableBase ( _stor_out_tbl4, 4.0 ) );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_makeStorageVSOutflowTable.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_makeStorageVSOutflowTable.cxx,v 1.1 2006/10/26 15:22:52 hsu Exp $";}
/*  ===================================================  */

}

