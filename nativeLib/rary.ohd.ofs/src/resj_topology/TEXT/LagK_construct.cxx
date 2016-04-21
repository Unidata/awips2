//------------------------------------------------------------------------------
// LagK :: construct - Reads a stringlist and constructs the LagK method object.
//------------------------------------------------------------------------------
// History:
//
// 13 Feb 1998    Daniel K. Weiler, Riverside Technology, inc
//                    Created initial version.
// 19 Nov 2001    James R. VanShaar, RTi    
//                    Adjusted carry over handling and 
//                    added the INITIALOUTFLOW parameter.
// 19 Nov 2001    JRV, RTI    
//                    Improved some error handling.
// 27 Nov 2001    JRV, RTI    
//                    Introduced assignment and use of _sizeInflowCO
//                    in some places
// 27 Nov 2001    JRV, RTi    
//                    Revised useage of COINFLOW and INITIALOUTFLOW
//                    values to to handle using defaults if they
//                    are not explicity defined.
// 03 Dec 2002    JRV, RTi    
//                    Revised handling of INITIALOUTFLOW to ensure
//                    consistency with COINFLOW values if K = 0.
// ** *** 2003    JRV, RTi    
//                    Plugged minor memory leak associated with
//                    stringLists list and value_list.
// 21 Nov 2003    Luiz Teixeira, RTi
//                    Added list = FreeStringList ( list ) before
//                    one of STATUS_FAILURE returns in the constructor
// ** Jan 2006    Marc L. Baldo, RTi 
//                    Reformatted routine to follow new formatting 
//                    conventions (no tabs, spacing changed)
// ** Jan 2006    MLB, RTi    
//                    Added support for new table type: INFLOW_LAG
// ** Feb 2006    MLB, RTi    
//                    Added support for Fort Worth loss and new 
//                    carryover values for Lag (INITIALLAGGEDINFLOW,
//                    INITIALSTORAGE,KFTWLOSS)
//------------------------------------------------------------------------------

/**
@author Daniel Weiler, RTi; James R. VanShaar, RTi; Luiz Teixeira, RTi; 
Marc L. Baldo, RTi
*/

#include "LagK.h"
#include "ResJSys.h"
#include "TSUtil.h"

//------------------------------------------------------------------------------
// construct - Set internal values based on an array of commands
//------------------------------------------------------------------------------
// This routine works with a passed array of strings, one location per line 
// in the user's original file.  The white space ahead of each command has 
// been removed, allowing simple string comparison.
//
// All commands are case insensitive.  The lexical grammar for this command is:
//
// "LagK" -> LAGKCOMMANDS -> "EndLagK"
//
// LAGKCOMMANDS:: (limit one of "Lag"/TABLECOMMANDLAG, "K"/TABLECOMMANDK, 
//                 "COInflow", InitialOutflow", 
//                 "InitialLaggedInflow", "KFtWLoss" )
//                ( When using an INFLOW_LAG table, LAG..ENDLAG is optional )
//                ( When using an OUTFLOW_K table, K..ENDK is optional )
//     "Lag" PositiveInteger
//     "Lag" -> TABLECOMMANDLAG -> "EndLag"
//     TABLECOMMANDLAG
//     "K" Double
//     "K"   -> TABLECOMMANDK -> "EndK"
//     TABLECOMMANDK
//     "COInflow" -> "Values" -> DOUBLE_LIST -> "EndValues" -> "EndCOInflow"
//     "InitialOutflow" Double 
//     "INITIALSTORAGE" Double
//     "INITIALLAGGEDINFLOW" Double
//     "KFTWLOSS" Double Double
//   
// TABLECOMMANDLAG:: (EndTable is optional)
//     "Table Inflow_Lag" -> DOUBLE_LIST -> "EndTable"
//     "Table Inflow_Lag" -> DOUBLE_LIST 
//
// TABLECOMMANDK:: (EndTable is optional)
//     "Table Outflow_K" -> DOUBLE_LIST -> "EndTable"
//     "Table Outflow_K" -> DOUBLE_LIST 
//
// DOUBLE_LIST::
//     Double -> Double*
//
// Note that this routine enforces the full grammar.
//------------------------------------------------------------------------------
// Return:
//     None
// Calls:
//     BreakStringList
//     FreeStringList
//     GetSubStringList
//     Table::populate
// Errors:
//     - Missing commands (missing EndCOInflow, etc.)
//     - Bad data for commands
//        - too many arguments
//        - too few arguments
//        - bad argument type (like non-int after 'LAG' command)
//        - repeated commands inside a single LAGK
//     - Unknown commands
// Warnings:
//     Warnings are produced when
//     - Memory allocation is not possible
//     - Problems occur reading the Table command
//     - InitialOutflow, InitialStorage, InitialLaggedInflow are not followed
//       by a single double
//     - InitalOutflow is not consistent with carryover values
// Debug: 
//     None
//------------------------------------------------------------------------------

/**
Set internal values based on an array of commands.
@return success or failure of the construct routine
@param lagk_list The list of strings containing LagK commands
@param n_items The length of lagk_list
*/

int LagK :: construct ( char** lagk_list, int n_items )
{
    double ffactor = 1.0 ;
    int i ;
    int j ;
    char **lagk_val = NULL ;
    char **list = NULL ;
    int nlist = 0 ;
    int numErrs = 0 ;
    int n_lagk = 0 ;
    int n_value = 0 ;

    int limitLag = 0;
    int limitLagTable = 0;
    int limitK = 0;
    int limitKTable = 0;
    int limitInitOutflow = 0;
    int limitInitStorage = 0;
    int limitInitLagged = 0;
    int limitCOInflow = 0;
    int limitFortWorth = 0;

    int requireEndCOInflow = 0 ;
    int requireEndValues = 0 ;
    int requireInflowLag = 0 ;
    int requireOutflowK = 0 ;
    int requireValues = 0 ;
    char routine[] = "LagK :: construct" ;
    int warnTableK = 0 ;
    int warnTableLag = 0 ;
    char **value_list = NULL ;
    double valueOfK ;

    ffactor = Component::_ffactor ;

    // Initialize outflow on owner
    _owner->_outflow = _outflowCO ;

    _out_k_tbl.allocateDataSpace ( 1 ) ;
    valueOfK = 0 ;
    _kconst = valueOfK ;
    _n_kval = 1 ;
    _n_lagval = 1 ;
    _out_k_tbl.populate ( 0, 0, atof ( "1000000.0") ) ;
    _out_k_tbl.populate ( 0, 1, valueOfK ) ;
    _transLossCoef = 0.;
    _transLossLevel = 0.;

    // Process lagk_list, enforcing the syntax
    for ( i = 0 ; i < n_items ; i++ )
    {
        if ( list )
        {
            list = FreeStringList ( list ) ;
        }

        if ( strlen ( lagk_list[i] ) == 0 || lagk_list[i][0] == '#' )
        {
            continue ;
        }

        list = BreakStringList ( lagk_list[i], " \n\t", DELIM_SKIP_BLANKS,
                                &nlist ) ;
        if ( nlist == 0 || list == NULL )
        {
            PrintError ( routine, "Troubles getting "
                         "data for %s %s LagK.",
                         _owner->getID ( ) , _id ) ;
            if ( list )
            {
                list = FreeStringList ( list ) ;
            }
            return ( STATUS_FAILURE ) ;
        }

        // Check for extra values after commands:
        if ( ( !strcasecmp ( "LAG", list[0] ) && ( nlist > 2 ) ) ||
             ( !strcasecmp ( "K", list[0] ) && ( nlist > 2 ) ) ||
             ( !strcasecmp ( "ENDLAG", list[0] ) && ( nlist > 1 ) ) ||
             ( !strcasecmp ( "ENDK", list[0] ) && ( nlist > 1 ) ) ||
             ( !strcasecmp ( "TABLE", list[0] ) && ( nlist > 2 ) ) ||
             ( !strcasecmp ( "COINFLOW", list[0] ) && ( nlist > 1 ) ) ||
             ( !strcasecmp ( "ENDCOINFLOW", list[0] ) && ( nlist > 1 ) ) ||
             ( !strcasecmp ( "ENDVALUES", list[0] ) && ( nlist > 1 ) ) ||
             ( !strcasecmp ( "VALUES", list[0] ) && ( nlist > 1 ) ) ||
             ( !strcasecmp ( "INITIALOUTFLOW", list[0] ) && ( nlist > 2 ) ) ||
             ( !strcasecmp ( "INITIALSTORAGE", list[0] ) && ( nlist > 2 ) ) ||
             ( !strcasecmp ( "INITIALLAGGEDINFLOW", list[0] ) && ( nlist > 2 ) ) ||
             ( !strcasecmp ( "KFTWLOSS", list[0] ) && ( nlist > 3 ) ) ||
             ( !strcasecmp ( "ENDTABLE", list[0] ) && ( nlist > 1 ) ) )
        {
            PrintError ( routine, "The '%s' keyword has too many arguments " 
                                  "on %s %s.", list[0], _owner->getID (), _id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }

        // Check for inappropriate values in LAG keyword:
        if ( !strcasecmp ( "LAG", list[0] ) && ( nlist == 2 ) && 
             ( !IsInteger( list[1] ) ) )
        {
            PrintError ( routine, 
                "The '%s' keyword requires an integer argument, not '%s', " 
                "inside %s %s.", list[0], list[1], _owner->getID (), _id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }

        // Guarantee that the argument for single-argument functions is a double
        if ( ( nlist == 2 ) && 
             ( !IsDouble( list[1] ) ) &&
             ( ( !strcasecmp ( "K", list[0] ) ) ||
               ( !strcasecmp ( "INITIALOUTFLOW", list[0] ) ) ||
               ( !strcasecmp ( "INITIALSTORAGE", list[0] ) ) ||
               ( !strcasecmp ( "INITIALLAGGEDINFLOW", list[0] ) ) ) )
        {
            PrintError ( routine, 
                "The '%s' keyword requires a double argument, not '%s', " 
                "inside %s %s.", list[0], list[1], _owner->getID (), _id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }

        // Guarantee that the argument for two-argument functions 
        // is two doubles
        if ( ( nlist == 3 ) && 
             ( !IsDouble( list[1] ) ) &&
             ( !IsDouble( list[2] ) ) &&
             ( !strcasecmp ( "KFTWLOSS", list[0] ) ) )
        {
            PrintError ( routine, 
                "The '%s' keyword requires a two double arguments, "
                "not '%s %s', inside %s %s.", list[0], list[1], 
                list[2], _owner->getID (), _id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }

        // Make sure commands are defined only once in this lagk.
        if ( ( limitLag > 0 && !strcasecmp ( "LAG", list[0] ) ) ||
             ( limitLagTable > 0 && nlist > 1 && list[0] && list[1] &&
               !strcasecmp ( "TABLE", list[0] ) && 
               !strcasecmp ( "INFLOW_LAG", list[1] ) ) ||
             ( limitK > 0 && !strcasecmp ( "K", list[0] ) ) ||
             ( limitKTable > 0 && nlist > 1 && list[0] && list[1] &&
               !strcasecmp ( "TABLE", list[0] ) && 
               !strcasecmp ( "OUTFLOW_K", list[1] ) ) ||
             ( limitInitOutflow > 0 && !strcasecmp ( "INITIALOUTFLOW", list[0] ) ) ||
             ( limitInitStorage > 0 && !strcasecmp ( "INITIALSTORAGE", list[0] ) ) ||
             ( limitInitLagged > 0 && !strcasecmp ( "INITIALLAGGEDINFLOW", list[0] ) ) ||
             ( limitCOInflow > 0 && !strcasecmp ( "COINFLOW", list[0] ) ) ||
             ( limitFortWorth > 0 && !strcasecmp ( "KFTWLOSS", list[0] ) ) )
        {
            PrintError ( routine, 
                "The '%s' keyword may not be repeated in a single LAGK method. "
                "inside %s %s.", list[0], _owner->getID (), _id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }

        // Because LAG..ENDLAG and K..ENDK are optional, make sure table is not redefined.
        // or the table is followed by the section
        if ( ( limitLagTable > 0 && !strcasecmp ( "LAG", list[0] ) ) )
        {
            PrintError ( routine, 
                "The '%s' keyword may not appear after INFLOW_LAG table is defined, "
                "for %s %s.", list[0], _owner->getID (), _id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }

        if ( ( limitKTable > 0 && !strcasecmp ( "K", list[0] ) ) )
        {
            PrintError ( routine, 
                "The '%s' keyword may not appear after OUTFLOW_K table is defined, "
                "for %s %s.", list[0], _owner->getID (), _id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }

        if ( !strcasecmp ( "LAG", list[0] ) )
        {
            limitLag = 1;
            warnTableK = 0 ;
            if ( nlist < 2 )
            {
                // LAG without a following value means that
                // a table of inflow versus lag values follows.
                // 
                requireInflowLag = 1;
                list = FreeStringList ( list ) ;
                continue ;
            }

            // We have two arguments
            warnTableLag = 1 ;
            _lag = 0;

            if( list[1] && strlen( list[1] ) > 0 )
            {
                _lag = atoi ( list[1] ) ;
            }

            if ( _lag < 0 )
            {
                PrintError ( routine, 
                    "The '%s' keyword must be followed by a positive integer, "
                    "not ('%s'), inside %s %s.", list[0], list[1], 
                    _owner->getID (), _id ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }

            list = FreeStringList ( list ) ;
            // Need to check if the lag value is a multiple of the
            //    time step multiplier...
            if ( fmod ( _lag, _t_mult ) != 0 )
            {
                _interp = 1 ;
            }
            continue ;
        }
        else if ( !strcasecmp ( "K", list[0] ) )
        {
            limitK = 1;
            warnTableLag = 0 ;
            if ( nlist < 2 )
            {
                // K without a following value means that
                // a table of outflow versus K values follows.
                // 
                list = FreeStringList ( list ) ;
                requireOutflowK = 1;
                continue ;
            }

            // We have two arguments
            warnTableK = 1 ;

            _out_k_tbl.allocateDataSpace ( 2 ) ;
            valueOfK = atof ( list[1] ) ;
            if ( ( _out_k_tbl.populate ( 0, FLOWCOLUMN, 0.0 ) ) ||
                 ( _out_k_tbl.populate ( 0, KCOLUMN, valueOfK ) ) ||
                 ( _out_k_tbl.populate ( 1, FLOWCOLUMN, 1000000.0 ) ) ||
                 ( _out_k_tbl.populate ( 1, KCOLUMN, valueOfK ) ) )
            {
                PrintError ( routine, "Troubles filling Outflow-"
                             "K table on %s %s.", _owner->getID (), _id ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }
            _n_kval = 2 ;
            _k_mode = VARIABLE_K ;
            continue ;
        }
        else if ( ( !strcasecmp ( "TABLE", list[0] ) && nlist > 1 &&
            !strcasecmp( "INLFOW_LAG", list[1]) ) && warnTableLag )
        {
            PrintError ( routine, "'TABLE' keyword not allowed directly "
                         "after 'LAG' keyword with constant lag: %s on %s.",
                         _id, _owner->_id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }
        else if ( ( !strcasecmp ( "TABLE", list[0] ) && nlist > 1 &&
            !strcasecmp( "OUTFLOW_K", list[1]) ) && warnTableK )
        {
            PrintError ( routine, "'TABLE' keyword not allowed directly "
                         "after 'K' keyword with constant K: %s on %s.",
                         _id, _owner->_id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }
        else if ( !strcasecmp ( "TABLE", list[0] ) && nlist < 2 )
        {
            PrintError ( routine, "'TABLE' keyword found inside "
                                  "'K'..'ENDK' is missing "
                                  "the required argument 'OUTFLOW_K', in "
                                  "%s on %s.", _id, _owner->_id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }
        else if ( !strcasecmp ( "TABLE", list[0] ) && nlist < 2 )
        {
            PrintError ( routine, "'TABLE' keyword found inside "
                                  "'LAG'..'ENDLAG' is missing "
                                  "the required argument 'INFLOW_LAG', in "
                                  "%s on %s.", _id, _owner->_id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }
        else if ( !strcasecmp ( "TABLE", list[0] ) && nlist < 2 )
        {
            PrintError ( routine, "'TABLE' keyword is missing its required"
                                  "argument, 'OUTFLOW_K' or INFLOW_LAG'"
                                  "%s on %s.", _id, _owner->_id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }
        else if ( !strcasecmp ( "TABLE", list[0] ) && 
                  !strcasecmp ( "outflow_k", list[1] ) ) 
        {
            limitKTable = 1;
            lagk_val = GetSubStringList ( lagk_list + i , n_items, "TABLE",
                                         &n_lagk, RETURN_NO_KEYWORDS ) ;
            // Verify that TABLE was found
            if ( lagk_val == NULL || n_lagk == 0 )
            {
                PrintError ( routine, "Troubles getting "
                             "outflow-K table from control file.\n"
                             "Typically this is caused by a missing ENDTABLE"
                             "or misformatted file." ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }

            // There is table data, populate the table
            requireOutflowK = 0;
            if ( _out_k_tbl.populate ( lagk_val, n_lagk, ffactor, 1.0 ) )
            {
                PrintError ( routine, "Troubles filling Outflow-"
                             "K table on %s %s.  \nTypically this is caused\n"
                             "by a syntax error in the table", 
                             _owner->getID (), _id ) ;
                lagk_val = FreeStringList ( lagk_val ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }

            // Enforce flow values are increasing with table position
            for ( j = 0 ; j < _out_k_tbl.getNRows ( ) - 1; j++ )
            {
                if ( _out_k_tbl.lookup( j, FLOWCOLUMN ) >
                     _out_k_tbl.lookup( j + 1, FLOWCOLUMN ) )
                {
                    PrintError ( routine, "Flow values in OUTFLOW_K table "
                                          "must be increasing, "
                                          "on %s %s.  ",
                                          _owner->getID (), _id ) ;
                    lagk_val = FreeStringList ( lagk_val ) ;
                    list = FreeStringList ( list ) ;
                }
            }

            // This next section adds to the K table if the table either
            // starts with non-zero flow or ends with a small flow < 1e6.
            int flagNeedZero = 0 ;
            int flagNeedInf = 0 ;
            double zeroValue = 0.0 ;
            double infValue = 0.0 ;

            // REVISIT: when table has getFirst and getLast, update
            // this code to use it.  (Table.getFirst, Table.getLast)
            if ( _out_k_tbl.lookup( 0, FLOWCOLUMN ) > .001 )
            {
                flagNeedZero = 1 ;
                zeroValue = _out_k_tbl.lookup( 0, KCOLUMN ) ;
            }
            // Detect if the last value is below 1e6, keeping last value
            if ( _out_k_tbl.lookup( 0, FLOWCOLUMN ) < 1000000.0 )
            {
                flagNeedInf = 1 ;
                infValue = _out_k_tbl.lookup( _out_k_tbl.getNRows ( ) - 1,
                                              KCOLUMN ) ;
            }

            // REVISIT: when table has addFirst and addLast, update
            // this code to use it.  (Table.addFirst, Table.addLast)
            Table tmpTable;
            tmpTable.allocateDataSpace ( _out_k_tbl.getNRows () + 
                                         flagNeedInf + flagNeedZero );
            
            tmpTable.populate ( 0, FLOWCOLUMN, 0.0 ) ;
            tmpTable.populate ( 0, KCOLUMN, zeroValue ) ;

            for ( j = 0 + flagNeedZero; 
                  j < _out_k_tbl.getNRows ( ) + flagNeedZero; j++ )
            {
                tmpTable.populate ( j, FLOWCOLUMN,
                    _out_k_tbl.lookup ( j - flagNeedZero, FLOWCOLUMN ) ) ;
                tmpTable.populate ( j, KCOLUMN,
                    _out_k_tbl.lookup ( j - flagNeedZero, KCOLUMN ) ) ;
            }

            if ( flagNeedInf > 0 )
            {
                int tablePosition = _out_k_tbl.getNRows ( ) + flagNeedZero ;
                tmpTable.populate( tablePosition, KCOLUMN , infValue ) ;
                tmpTable.populate( tablePosition, FLOWCOLUMN, 1000000.0 ) ;
            }

            _out_k_tbl = tmpTable;

            _n_kval = _out_k_tbl.getNRows ( ) ;
            lagk_val = FreeStringList ( lagk_val ) ;
            i += n_lagk ;
            _kconst = _out_k_tbl.lookup( 0 , 1 ) ;
            continue ;
        }
        else if ( !strcasecmp ( "TABLE", list[0] ) && 
                  !strcasecmp ( "inflow_lag", list[1] ) ) 
        {
            limitLagTable = 1;
            lagk_val = GetSubStringList ( lagk_list + i , n_items, "TABLE",
                                         &n_lagk, RETURN_NO_KEYWORDS ) ;
            // Check to make sure something was found ...
            if ( lagk_val == NULL || n_lagk == 0 )
            {
                PrintError ( routine, "Troubles getting "
                             "Inflow-LAG table from control file.\n"
                             "Typically this is caused by a missing ENDTABLE"
                             "or misformatted file." ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }

            // There is table data, populate the table
            requireInflowLag = 0;
            if ( _in_lag_tbl.populate ( lagk_val, n_lagk, ffactor, 1.0 ) )
            {
                PrintError ( routine, "Troubles filling Inflow-"
                             "LAG table on %s %s.  \n"
                             "Typically this is caused\n"
                             "by a syntax error in the table", 
                             _owner->getID (), _id ) ;
                lagk_val = FreeStringList ( lagk_val ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }
            _n_lagval = n_lagk ;
            lagk_val = FreeStringList ( lagk_val ) ;

            int j;
            _lag = 0;
            double currVal = 0.;
            for(j = 0; j < n_lagk; j++)
            {
                currVal = _in_lag_tbl.lookup(j, GETCOLUMN_2) ;
                if( currVal > _lag ) 
                { 
                    _lag = (int) ( currVal + .5 ) ;
                }
            }

            i += n_lagk ;
            continue ;
        }
        else if ( !strcasecmp ( "ENDK", list[0] ) ) 
        {
        }
        else if ( !strcasecmp ( "ENDLAG", list[0] ) ) 
        {
        }
        else if ( !strcasecmp ( "TABLE", list[0] ) )
        {
            PrintError ( routine, "'TABLE' keyword is outside "
                                  "'LAG'..'ENDLAG' or 'K'..'ENDK' section, (%s %s) "
                                  "%s, filling table on %s.", list[0], list[1], 
                                  _owner->getID (), _id ) ;
        }
        else if ( !strcasecmp ( "INITIALOUTFLOW", list[0] ) )
        {
            limitInitOutflow = 1;
            if ( nlist < 2 )
            {
                PrintError ( routine, "Value required "
                             "immediately after %s method %s "
                             "keyword %s.", _type, _id, list[0] ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }
            _outflowCO = atof ( list[1] ) * ffactor ;
            _owner->_outflow = _outflowCO ;
            continue ;
        }
        else if ( !strcasecmp ( "INITIALSTORAGE", list[0] ) )
        {
            limitInitStorage = 1;
            if ( nlist < 2 )
            {
                PrintError ( routine, "Value required "
                             "immediately after %s method %s "
                             "keyword %s.", _type, _id, list[0] ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }
            _storageCO = atof ( list[1] ) * ffactor ;
            continue ;
        }
        else if ( !strcasecmp ( "INITIALLAGGEDINFLOW", list[0] ) )
        {
            limitInitLagged = 1;
            if ( nlist < 2 )
            {
                PrintError ( routine, "Value required "
                             "immediately after %s method %s "
                             "keyword %s.", _type, _id, list[0] ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }
            _laggedInflow = atof ( list[1] ) * ffactor ;
            continue ;
        }
        else if ( !strcasecmp ( "KFTWLOSS", list[0] ) )
        {
            limitFortWorth = 1;
            if ( nlist < 3 )
            {
                PrintError ( routine, "2 values required "
                             "immediately after %s method %s "
                             "keyword %s.", _type, _id, list[0] ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }
            _transLossCoef = atof ( list[1] ) * ffactor ;
            _transLossLevel = atof ( list[2] ) * ffactor ;
            continue ;
        }
        else if ( !strcasecmp ( "COINFLOW", list[0] ) )
        {
            limitCOInflow = 1;
            requireValues = 1;
        }
        else if ( !strcasecmp ( "VALUES", list[0] ) && requireValues )
        {
            requireValues = 0;
            requireEndValues = 1;
        }
        else if ( !strcasecmp ( "ENDVALUES", list[0] ) && requireEndValues )
        {
            requireEndValues = 0;
            requireEndCOInflow = 1;
        }
        else if ( requireEndValues )
        {
            // Check for too many arguments
            if( nlist > 1 )
            {
                PrintError ( routine, "Inside 'VALUES'..'ENDVALUES' section, "
                                      "Extra values found after '%s', "
                                      "on %s %s.", list[0], _owner->getID (), _id ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }
            // Check for inappropriate value inside Values/Endvalues
            if( !IsDouble( list[0] ) )
            {
                PrintError ( routine, "Inside 'VALUES'..'ENDVALUES' section, "
                                      "Value '%s' must be a floating point number, "
                                      "on %s %s.", list[0], _owner->getID (), _id ) ;
                list = FreeStringList ( list ) ;
                return ( STATUS_FAILURE ) ;
            }
        }
        else if ( !strcasecmp ( "ENDCOINFLOW", list[0] ) && requireEndCOInflow)
        {
            requireEndCOInflow = 0;
        }
        else if ( !strcasecmp ( "ENDTABLE", list[0] ))
        {
        }
        else if ( (strcasecmp("A", list[0]) < 0) &&
                  (strcasecmp("ZZZZZZZZZ", list[0]) > 0) )
        {
            PrintError ( routine, " Unknown keyword found inside LAGK section: "
                                  " '%s' "
                                  "(%s %s).", list[0], _type, _id ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }
    }

    if ( list )
    {
        list = FreeStringList ( list ) ;
    }

    // Detect error conditions
    if( requireOutflowK )
    {
        PrintError ( routine, "Expected an OUTFLOW_K table.  "
                              "The 'K' keyword was present, but no constant "
                              "K or variable K table was defined, on "
                              "(%s %s).", _type, _id ) ;
        return ( STATUS_FAILURE ) ;
    }
    if( requireInflowLag )
    {
        PrintError ( routine, "Expected an INFLOW_LAG table.  "
                              "The 'LAG' keyword was present, but no constant "
                              "lag or variable lag table was defined, on "
                              "(%s %s).", _type, _id ) ;
        return ( STATUS_FAILURE ) ;
    }
    if ( requireValues )
    {
        PrintError ( routine, "The 'COINFLOW' keyword requires a "
                              "by a 'VALUES' keyword "
                              "(%s %s).", _type, _id ) ;
        return ( STATUS_FAILURE ) ;
    }
    if ( requireEndValues )
    {
        PrintError ( routine, "The 'ENDVALUES' keyword must appear on "
                              "a separate line, following the "
                              "data in the 'VALUES' section "
                              "(%s %s).", _type, _id ) ;
        return ( STATUS_FAILURE ) ;
    }
    if ( requireEndCOInflow )
    {
        PrintError ( routine, "The 'ENDCOINFLOW' keyword is missing. "
                              "All COINFLOW..ENDCOINFLOW sections must "
                              "be matched and must surround the "
                              "'VALUES'..'ENDVALUES' section, on "
                              "(%s %s).", _type, _id ) ;
        return ( STATUS_FAILURE ) ;
    }

    // Calculate _sizeInflowCO
    if ( _lag == 0 )
    {
        _sizeInflowCO = 1 ;
    }
    else if ( _lag < _t_mult )
    {
        _sizeInflowCO = 2 ;
    }
    else if ( _lag % _t_mult != 0 )
    {
        _sizeInflowCO = _lag / _t_mult + 2 ;
    }
    else
    {
        _sizeInflowCO = _lag / _t_mult + 1 ;
    }

    // Determine the k mode
    if ( _n_kval == 1 )
    {
        _k_mode = CONSTANT_K ;
    }
    else
    {
        _k_mode = VARIABLE_K ;
    }

        int requiredLags = 0 ;
    // Determine the k mode
    if ( _n_lagval == 1 )
    {
        _lag_mode = CONSTANT_LAG ;
    }
    else
    {
        _lag_mode = VARIABLE_LAG ;
        if ( _n_lagval > _sizeInflowCO )
        {
            _sizeInflowCO = _n_lagval ;
        }
    }

    _co_inflow = new double[_sizeInflowCO] ;
    if ( _co_inflow == NULL )
    {
        PrintError ( routine, "Could not allocate %d doubles.",
                     _sizeInflowCO ) ;
        numErrs++ ;
    }
    else
    {
        // Initialize the carry over values to 0.0
        for ( i = 0 ; i < _sizeInflowCO ; i++ )
        {
            _co_inflow[i] = 0.0 ;
        }
    }

    // Look for carry over values in the parameter file
    value_list = GetSubStringList ( lagk_list, n_items, "COINFLOW",
                                   &n_value, RETURN_NO_KEYWORDS ) ;
    if ( value_list != NULL && n_value != 0 )
    {
        list = GetSubStringList (value_list, n_value, "VALUES",
            &n_value, RETURN_NO_KEYWORDS ) ;
        if ( list == NULL || n_value == 0 )
        {
            PrintError ( routine, "Troubles getting COINFLOW data "
                        "from control file.") ;
            value_list = FreeStringList ( value_list ) ;
            if ( list )
            {
                list = FreeStringList ( list ) ;
            }
            return ( STATUS_FAILURE ) ;
        }
        if ( _sizeInflowCO == 1 && n_value != 1 && _lag_mode == CONSTANT_LAG )
        {
            PrintError ( routine, "Must have 1 carryover inflow "
                        "value when lag is 0.") ;
            value_list = FreeStringList ( value_list ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }
        if ( _sizeInflowCO == 2 && n_value != 2 && _lag_mode == CONSTANT_LAG )
        {
            PrintError ( routine, "Must have 2 carryover inflow "
                        "values when non-zero lag is less than time step." ) ;
            value_list = FreeStringList ( value_list ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }
        else if ( n_value != _sizeInflowCO )
        {
            PrintError ( routine, "%s %s must specify %d carryover "
                        "inflow values.", _type, _id, _sizeInflowCO ) ;
            value_list = FreeStringList ( value_list ) ;
            list = FreeStringList ( list ) ;
            return ( STATUS_FAILURE ) ;
        }

        // Fill in _co_inflow array
        for ( i = 0 ; i < _sizeInflowCO ; i++ )
        {
            _co_inflow[i] = atof ( list[i] ) * ffactor ;
        }
        value_list = FreeStringList ( value_list ) ;
        list = FreeStringList ( list ) ;
    }
    else
    {
        // Might be good for warning to test for value and warn if it
        //     is found.
    }

    // Need to create this table after all of the necessary info
    // is in memory
    if ( _k_mode == VARIABLE_K )
    {
        if ( makeStorageVSOutflowTable ( ) )
        {
            PrintError ( routine, "Troubles making Outflow-Storage table." ) ;
            numErrs++ ;
        }
        if ( makeStorageVSOutflowTableQuarter () )
        {
            PrintError ( routine, "Troubles making Outflow-Storage table at"
                                  " 1/4 timestep." ) ;
            numErrs++ ;
        }
    }

    if ( numErrs > 0 )
    {
        return ( STATUS_FAILURE ) ;
    }
    _is_constructed = 1 ;

    // Check to see if a single K value of 0 is set.  If so, ensure that 
    // INITIALOUTFLOW is consistent with the first value (and second, 
    // if applicable) in the COINFLOW section.
    if ( _n_kval == 1 && valueOfK == 0 )
    {
        if ( _interp )
        {
            double tmpP = _co_inflow[0] ;
            double tmpN = _co_inflow[1] ;
            double diff = _t_mult * ( (int)_lag / _t_mult + 1 ) - _lag ;
            // Interpolate
            double LagdQin1 = tmpP + ( tmpN - tmpP ) *
                              ( diff / (double)_t_mult ) ;
            // Test equality using inequalities
            double tolerance = 0.0001*_outflowCO ;
            if ( LagdQin1 > _outflowCO+tolerance ||
                LagdQin1 < _outflowCO-tolerance )
            {
                // Print Warning and revise _outflowCO
                PrintWarning ( 1, routine, "INITIALOUTFLOW "
                              "value \"%f\" not consistent with "
                              "COINFLOW values \"%f and %f\" when "
                              "lagged resulting in \"%f\".  "
                              "Instability may result.  Revising "
                              "INITIALOUTFLOW value to \"%f\".",
                              _outflowCO/ffactor, tmpP/ffactor,
                              tmpN/ffactor, LagdQin1/ffactor,
                              LagdQin1/ffactor) ;
                _outflowCO = LagdQin1 ;
                _owner->_outflow = _outflowCO ;
            }
        }
        else
        {
            double LagdQin1 = _co_inflow[0] ;
            // Test equality using inequalities
            double tolerance = 0.0001*_outflowCO ;
            if ( LagdQin1 > _outflowCO+tolerance ||
                LagdQin1 < _outflowCO-tolerance )
                {
                // Print Warning and revise _outflowCO
                PrintWarning ( 1, routine, "INITIALOUTFLOW "
                              "value \"%f\" not consistent with "
                              "COINFLOW value \"%f\".  Instability "
                              "may result.  Revising INITIALOUTFLOW "
                              "value to \"%f\".", _outflowCO/ffactor,
                              LagdQin1/ffactor, LagdQin1/ffactor) ;
                _outflowCO = LagdQin1 ;
                _owner->_outflow = _outflowCO ;
            }
        }
    }

    // write the carryover inflow to the CO array at the System level...
    setCOstring ( ) ;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

    return( STATUS_SUCCESS ) ;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_construct.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_construct.cxx,v 1.15 2006/10/26 15:22:26 hsu Exp $";}
/*  ===================================================  */

}
