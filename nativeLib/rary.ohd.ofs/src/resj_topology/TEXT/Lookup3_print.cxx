//------------------------------------------------------------------------------
// Lookup3 :: print - print instance data members.
//------------------------------------------------------------------------------
// History:
// 
// 01 Feb 2006  James R. VanShaar, RTi  Created initial version
//------------------------------------------------------------------------------
#include "Lookup3.h"
#include "TSDateIterator.h"

//------------------------------------------------------------------------------
// Lookup :: print - print instance data members
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintWarning()
//     Coimponent::getID()
// Errors:
//     None
// Warnings:
//     File pointer is NULL.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Print instance data members
@return int value signifying success
@param file FILE pointer for output
*/
int Lookup3 :: print( FILE* fp )
{
    char routine[] = "Lookup3 :: print";

    if( fp == NULL ) {
        PrintWarning( 1, routine, "Cannot print Lookup3 info - null FILE*.");
        return( STATUS_FAILURE );
    }

    // Print self id fnd owner first
    fprintf( fp, "Lookup3 method \"%s\" owned by Component \"%s\".\n", _id,
        _owner->getID() );

    // End identifier
    fprintf( fp, "End of \"%s %s %s\" information.\n\n", _type, _owner->_id, 
        _id );

    return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Lookup3_print.cxx,v $";
 static char rcs_id2[] = "$Id: Lookup3_print.cxx,v 1.1 2006/10/26 15:25:20 hsu Exp $";}
/*  ===================================================  */

}
