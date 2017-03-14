//------------------------------------------------------------------------------
// LagK - Object containing all LagK scheme information.
//------------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Mar 1998  Daniel K. Weiler, Riverside Technology, inc
//                                      Created initial version.
// 09 Apr 2001  James R. VanShaar, RTi  Added transferCO() function
// 12 Nov 2001  James R. VanShaar, RTi  Added setInactiveState
// 12 Nov 2001  JRV, RTi      Added function setCOstring
// 27 Nov 2001  JRV, RTi      Added _sizeInflowCO
// 12 Jan 2003  JRV, RTi      Added _n_OutStorVal
// 27 Mar 2006  Marc L. Baldo, RTi      Added support for new Lag/K functions
// 27 Mar 2006  MLB, RTi      Modified formatting, alphabetized member functions
//------------------------------------------------------------------------------

#ifndef LagK_INCLUDED
#define LagK_INCLUDED

#include "ReachMethod.h"
#include "resj/Table.h"

#define VARIABLE_K 0 
#define CONSTANT_K 1
#define VARIABLE_LAG 0 
#define CONSTANT_LAG 1

#define FLOWCOLUMN GETCOLUMN_1
#define KCOLUMN GETCOLUMN_2
#define OUTFLOWCOLUMN GETCOLUMN_1
#define STOR_OUTFLOWCOLUMN GETCOLUMN_2
#define STOR_SDTCOLUMN GETCOLUMN_1
#define TIMECOLUMN GETCOLUMN_2

#define MAXISEGS 20

class LagK : public ReachMethod 
{
public:
        
    LagK ( Reach* ) ;                   // Default constructor
    LagK ( const LagK&, Reach* ) ;      // Copy constructor

    virtual ~LagK ( ) ;                 // Destructor


    int construct ( char**, int ) ;
                                        // Called from the System
                                        // parse routine to build the 
                                        // release TSs

    LagK* copy ( Component* ) ;     // Calls copy constructor.

    int freeDataSpace ( ) ;

    // Get the carryover values into a passed table
    void getCarryOverValues ( TSDate& cur_date , Table &carryOverTable , 
                              int offset = 0 ) ;

    double getK();                      // returns _kconst

    // Get the carryover values into a passed char array
    void getCarryOverValuesStr ( TSDate& cur_date , char *coString ) ;

    int getLag ( ) ;                   // returns _lag

    int getSizeInflowCO ( ) ;          // _sizeInflowCO

    int print ( FILE* );             // Prints LagK info.

    // Solve Lag and/or K based on input deck for this reach
    int solveMethod ( TSDate&, int, double** = NULL );               

    void setInactiveState();        // Resets any variables which represent
                                    // a continuation of method activity 
                                    // from the previous time step

    int transferCO ( Method * methOLD, char * cOLD, char * cNEW, int * ipr ) ;
                                    // Transfers LagK carryover given
                                    // the new LagK method and the old and
                                    // new related portions of the carryover
                                    // strings

private:
    // Compute routing for an individual segment
    void ComputeSegmentRouting ( double x1, double x2, double y0, double y1, 
                                 double &y2, double xta, int &qdt ) ;

    // Interpolate value inside passed 2-value table
    double Interpolate ( double value, Table &sTable ) ;

    // When necessary, call this routine to calculate setment routing at 
    // 1/4 DT.
    void CalculateQuarterDT ( double &x1, double &x2, double &y0, double &y1, 
                              double &y2, double &xta, int &qdt ) ;

    // Calculate Fort Worth Channel Loss Recession effects
    double CalculateFortWorthLoss ( double qli, double qprev, double qk ) ;

    // Initialize data members
    int initialize ( );

    // Make storage versus outflow table: 2S/(dt/4)+O2 vs O2
    int makeStorageVSOutflowTable ( ) ;         

    // Make storage versus outflow table: 2S/(dt/divisor)+O2 vs O2
    int makeStorageVSOutflowTableBase ( Table &outTable, double divisor ) ;

    // Make quartered storage versus outflow table: 2S/(dt/4)+O2 vs O2
    int makeStorageVSOutflowTableQuarter ( ) ;  

    // Prepares carryover string for original / parameter input and CO 
    // array sizing
    int setCOstring( ); 

    // Prepares adds carryover string to the ResJSys carryover array
    int setCOstring(TSDate&);       

    // Perform Atlanta K routing
    double solveAtlantaK ( double LagdQin1, double LagdQin2, 
                           double** group_val ) ;
    // Get lagged value for current time step
    double solveLag ( TSDate& cur_date, double** group_val );

    // Perform mcp2 K routing
    double solveMCP2K ( double LagdQin1, double LagdQin2, double** group_val ) ;

    Table   _out_k_tbl;             // K-Outflow table. Outflow in col 0
                                    // and K in col 1

    Table   _in_lag_tbl;            // inflow-lag table. Inflow is in col 0
                                    // and lag in col 1.

    Table  _stor_out_tbl;           // O2 vs. 2S2/dt+O2 table. S term in
                                    // col 0 and O2 term in col 1

    Table  _stor_out_tbl4;          // O2 vs. 2S2/(1/4*dt)+O2 table. S term
                                    // in col 0 and O2 term in col 1

    double  _kconst;                // Constant K value in units of time.
    int     _lag;                   // Lag value in units of time.
        
    int     _n_kval,                // Number of k values in the table.
            _n_lagval,              // Number of lag values in the table.
            _k_mode,                // Constant or variable K.
            _lag_mode,              // Constant or variable LAG.
            _interp,                // Will get set to 1 if we need to
                                    // interpolate for the lagged inflow
                                    // values.
            _n_OutStorVals;         // Number of value pairs in the 
                                    // _stor_out_tbl.

    double* _co_inflow;             // Carryover inflow value array

    double _outflowCO;              // Last outflow
    double _laggedInflow;           // Lagged inflow

    double _storageCO;              // Last storage

    int    _sizeInflowCO;           // Number of inflow values required for
                                    // continuous execution of LagK--
                                    // number of carry over inflow values

    double _transLossCoef;          // Fort Worth transmission loss Coefficient
    double _transLossLevel;         // Minimum Level for Fort Worth Loss Coeff

};
#endif
