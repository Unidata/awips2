//------------------------------------------------------------------------------
// Lookup3 - Object containing all Lookup3 method information.
//------------------------------------------------------------------------------
// Copyright:  See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 2006 Feb 02  James R. VanShaar, Riverside Technology, inc
//                                      Created initial version.
//------------------------------------------------------------------------------
// Variables:   I/O     Description
//
//
//------------------------------------------------------------------------------

#ifndef Lookup3_INCLUDED
#define Lookup3_INCLUDED

#include "Component.h"
//#include "NodeMethod.h"
#include "Expression.h"
#include "ComponentMethod.h"
#include "Node.h"
#include "ReferencedMatrix.h"
#include "Reservoir.h"

class Lookup3 : public ComponentMethod
{
public:
    Lookup3( Component*, char* type );      // Default constructor

    virtual ~Lookup3();         // Destructor

    int construct( char**, int );
                                // Time series construction.

//REMAINING
    Lookup3* copy( Component* );// Calls copy constructor.

    Component * getReceivingComp();
//REMAINING
    int freeDataSpace();        // Deletes dynamically allocated data.

    void getMyValueAsOut( double *in, double *out );

    int solveMethod( TSDate &, int, double** = NULL );
                                // Solving algorithm.

//REMAINING
    int print( FILE* );         // Prints all the info about the
                                // Lookup3 object.

    void setInactiveState();    // Resets any variables which represent
                                // a continuation of method activity
                                // from the previous time step

    int setCOstring();  // Prepares carryover string for original
                        // parameter input and CO array sizing

    int setCOstring(TSDate&);   // Prepares adds carryover string to the
                                // ResJSys carryover array

//REMAINING
    int transferCO ( Method * methOLD, char * cOLD, char * cNEW,
        int * ipr );
                                // Transfers Lookup3 carryover given
                                // the new Lookup3 method and the
                                // old and new related portions of the
                                // carryover strings

private:
    // ############### Start Methods ###############{

//REMAINING
    int initialize();           // Initialize data members

    void  sendToTS( TSDate date );
    void  sendToTS( TSDate date, double scalar );

    // Method construction sub-methods.
    int buildTable( char**, int );
    int key_BLENDTBL_BLENDTS( char **list, int nlist, int type );
    int key_COLUMNVAR( char **list, int nlist, char *re_list );
    int key_ROWVAR( char **list, int nlist, char *re_list );
    int key_INITIALTRANSFER( char **list, int nlist );
    int key_INTERPOLATE( char **list, int nlist );
    int key_TABLEVAR( char **list, int nlist, char *re_list );
    int key_TOCOMP( char **list, int nlist );
    int key_TSINDEX( char **list, int nlist, char *re_list );
    int key_TSINPUT( char **list, int nlist, char *re_list );
    int key_VALUES( char **list, int nlist, char **re_list, int *rowCount );
    int key_WEEKLYVARIATION( char **list, int nlist );

    // Method solution sub-methods
    int restartBlend( int rowI, int colI );

    // ############### End Methods ###############}

    // ############### Start Members ###############{

    // Observed Time series
    HourTS*             _release_obs;  // Input observed release TS.
    HourTS*             _withdraw_obs; // Input observed withdraw TS.
    HourTS*             _diversion_obs;// Input observed diversion TS.
    HourTS*             _augment_obs;  // Input observed augmentation TS.

    // Indexing time series
    HourTS*             _rowIndex;     // TS used for indexing rows.
    HourTS*             _colIndex;     // TS used for indexing columns.

    // Table stuff
    enum
    {
        INDEX_UNDEFINED = 0,
        INDEX_DATE      = 1,
        INDEX_TS        = 2,
        INDEX_COMPSTATE = 3
    };

    // Column indexing, etc.
    int         _colVar;               // Integer flag defining the type of
                                       // column indexing variable.
                                       //   0    = initialized, not defined.
                                       //   1    = Date
                                       //   2    = Input Time series
                                       //   3    = Some component state

    char        _colOrigUnits[24];     // User input units for column indexes.

    float       _colConv;              // Coversion for column indexes.

    Expression *_colExpr;              // Expression pointer for storing
                                       // column component state expression.

    // Row indexing, etc.
    int         _rowVar;               // Integer flag defining the type of
                                       // row indexing variable.
                                       //   0    = Date
                                       //   1    = Input Time series
                                       //   2    = Some component state
                                       //   -999 = initialized, not defined.

    char        _rowOrigUnits[24];     // User input units for row indexes.

    float       _rowConv;              // Coversion for column indexes.

    Expression *_rowExpr;              // Expression pointer for storing
                                       // column component state expression.

    // Data, etc.
    enum
    {
        TVAR_UNDEFINED    = 0,
        TVAR_RELEASE      = 1,
        TVAR_WITHDRAWAL   = 2,
        TVAR_DIVERSION    = 3,
        TVAR_AUGMENTATION = 4
    };

    int         _tableVar;             // Integer flag defining the type of
                                       // output from the table and method.
                                       //   0    = initialized, not defined.
                                       //   1    = Release
                                       //   2    = Withdrawal
                                       //   3    = Diversion
                                       //   4    = Augmentation
                                       //          (negative Diversion)
    double      _tableConv;            // Coversion for column indexes.


    // Blending
    int         _n_blend_ts,    // Number of time series blending time steps.
                _ts_step,       // Number of time series blending time steps
                                // since last update.
                _n_blend_tbl,   // Number of table blending time steps.
                _tbl_step,      // Number of table blending time steps since
                                // last update.
                _iValKnown,     // Trigger informing whether the last table
                                // indexes were known:
                                //   0 = Not known
                                //   1 = both are known
                _lastValKnown;  // Trigger informing whether the method solved
                                // successfully last time step.
                                //   0 = Did not solve (inactive)
                                //   1 = Solved successfully

    int         _colI,          // Last index to the column index array
                _rowI;          // Last index to the row index array

    // WeeklyVariation
    double*     _inWeekScalars;        // list of scaling values used to
                                       // provide within-week variation.

    // To Component pointer
    Component* _receivingComp;  // Component for toComp
    // Transfer to another component
    int _toCompMode;    // # of timesteps to lag transfer of withdrawal
                        // to the Component.
    int _fromComp;      // Counter of negative values that are incompatible
                        // with TOCOMP mode.

    // ############### End Members ###############}

protected:
    ReferencedMatrix _table; // The Lookup3 table.

    HourTS      _Comp_ts;       // Component inflow TS at which the
                                // withdrawal will be applied.
};

#endif
