//------------------------------------------------------------------------------
// Lookup3 :: freeDataSpace - delete dynamically allocated memory.
//------------------------------------------------------------------------------
// History:
// 
// 01 Feb 2006  James R. VanShaar, RTi  Created initial version
//------------------------------------------------------------------------------
#include "Lookup3.h"

//------------------------------------------------------------------------------
// Lookup :: freeDataSpace - Frees dynamically allocated memory
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
Frees dynamically allocated memory.
@return int value signifying success
*/
int Lookup3 :: freeDataSpace()
{
    int i;
    
    if( _colExpr )
    {
        delete _colExpr;
    }
    if( _rowExpr )
    {
        delete _rowExpr;
    }
    if( _inWeekScalars != NULL ) {
        delete [] _inWeekScalars;
    }

    return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Lookup3_freeDataSpace.cxx,v $";
 static char rcs_id2[] = "$Id: Lookup3_freeDataSpace.cxx,v 1.1 2006/10/26 15:24:35 hsu Exp $";}
/*  ===================================================  */

}
