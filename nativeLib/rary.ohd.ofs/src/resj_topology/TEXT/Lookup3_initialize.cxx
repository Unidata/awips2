//------------------------------------------------------------------------------
// Lookup3 :: initialize - initialized instance data members.
//------------------------------------------------------------------------------
// History:
// 
// 01 Feb 2006  James R. VanShaar, RTi    Created initial version
//------------------------------------------------------------------------------
#include "Lookup3.h"

//------------------------------------------------------------------------------
// Lookup :: initialize - Initialize data members.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
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
Initialize data members.
@return int value signifying success
*/
int Lookup3 :: initialize()
{
     char routine[]="Lookup3::initialize";

    strcpy( _type, "LOOKUP3" );
    _group_id = -999;

    // Set the observed time series pointers to NULL
    _release_obs = NULL;
    _withdraw_obs = NULL;
    _diversion_obs = NULL;
    _augment_obs = NULL;

    // **********##### Start: Table ###############{
    // Set the table indexing time series pointers to NULL.
    // They may not be needed.
    _rowIndex = NULL;
    _colIndex = NULL;

    // Set the table indexing units (applicable when the user specifies one or
    // more table indexing time series to empty string.
    // They may not be needed.
    strcpy( _colOrigUnits, "" );
    strcpy( _rowOrigUnits, "" );

    // Column variable type and conversion of table column indexes.
    _colVar = 0;
    _colConv = 1.000000000;

    // Row variable type and conversion of table row indexes.
    _rowVar = 0;
    _rowConv = 1.000000000;

    // Set the Component State indexing expression pointers to NULL.
    // They may not be needed.
    _colExpr = NULL;
    _rowExpr = NULL;

    // Set the table data flags
    _tableVar = TVAR_UNDEFINED; // probably 0, see Lookup3.h
    _tableConv = 1.000000000;

    // Blending
    // Less than one, not yet constructed.
    _n_blend_tbl = -1;
    _n_blend_ts = -1;
    // An otherwise, non-parameterized blend step would start with 1.
    _tbl_step = 1;
    _ts_step = 1;
    // indexing array indexes not known.
    _iValKnown = 0;
    // At last time step the method did not solve.
    _lastValKnown = 0;
    // Less than one, not yet constructed.
    _rowI = -1;
    _colI = -1;
    // **********##### End: Table ###############}

    // Pointer to the array of weekly variation scalars not defined.
    _inWeekScalars = NULL;

    // Initialize this to provide way for error trapping in 
    // Lookup3::construct
    _myValue = MISSING;

    // Allocation for toComp _outflow TS will occur in construction of the
    // method, if necessary.

    _receivingComp = NULL;
    _toCompMode = 1;          // Next step
    _fromComp = 0;

    _hasStates = 1;

    return( STATUS_SUCCESS );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Lookup3_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Lookup3_initialize.cxx,v 1.1 2006/10/26 15:24:45 hsu Exp $";}
/*  ===================================================  */

}
