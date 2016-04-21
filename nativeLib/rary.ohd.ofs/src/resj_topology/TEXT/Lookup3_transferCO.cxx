//------------------------------------------------------------------------------
// Lookup3::transferCO - Transfers carry over parameters for a Lookup3 
// 				method
//
//------------------------------------------------------------------------------
// History:
// 
// 01 Feb 2006  James R. VanShaar, RTi	Created initial version
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// @returns ierr integer representing errors occuring or significant warnings:
//	   0 = no problems
// @params methOLD Method object from old resj system definition (not used here)
// @params cOLD character string pointing to the portion of the old C array 
//	   containing carryover for the active Lookup3 method.
// @params cNEW character string pointing to the portion of the new C array 
//	   containing carryover for the active Lookup3 method.
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "Lookup3.h"
#include <string.h>

//------------------------------------------------------------------------------
// Lookup :: transferCO - Transfer carryover values.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     ResJ_fwrite( &length, message, ipr );
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
@returns ierr integer representing errors occuring or significant warnings (0 =
 no problems).
@params methOLD Method object from old resj system definition (not used here)
@params cOLD character string pointing to the portion of the old C array 
 containing carryover for the active Lookup3 method.
@params cNEW character string pointing to the portion of the new C array 
 containing carryover for the active Lookup3 method.
*/
int Lookup3 :: transferCO ( Method * methOLD, char * cOLD, char * cNEW,
	int * ipr )

    // Currently (02/2006) Lookup3 carryover data consists of:
    //   1)  The time step counter for the time series blend at the next 
    //       time step for which blending is required.
    //   2)  The time step counter for the table blend at the next time
    //       step for which blending is required
    //   3)  The column index integer referencing what part of the table was
    //       used.
    //   4)  The row index integer referencing what part of the table was
    //       used.
    //   5)  The initial transfer to a component, which is the value 
    //       assigned to _myValue
    //
    //   Of the five, only the initial transfer value is transferred.
    //      This value is transferred directly, with no extra consideration.
    //      It has no meaning if there is no ToComp parameterization on the
    //      new method, and still exists if the old method had no ToComp 
    //      parameterization.

{
    // NOTE: The following variable definitions are based entirely on
    //    only transferring initialtransfer.

    int ierr = 0, length;
    char routine[]="Lookup3::transferCO", message[256], partA[256], 
        partB[256], *temp;
    int numFloats = 3+3+1+3+3+1+1+1+1;
        // number of float positions in the C array used to store 'METHOD',
        // method_ID, the index to the next carryover the method type "LOOKUP3"
        // the owning Component id, the time series blend, the table blend, the
        // column index and the row index.
    int index = numFloats * sizeof(float);
        // Defines the position after all the labels and the next index
    
    // Simply copy the INITIALTRANSFER value from the old to the new 
    // carryover array.
    strncpy( &cNEW[index], &cOLD[index], 8 );

    // Handle any errors
    if ( ierr > 0 ) {
        sprintf (message, "Lookup3 string copy error for %s.",_id);
        length = strlen (message);
        ResJ_fwrite( &length, message, ipr );
    }
    
    return ierr;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Lookup3_transferCO.cxx,v $";
 static char rcs_id2[] = "$Id: Lookup3_transferCO.cxx,v 1.1 2006/10/26 15:25:29 hsu Exp $";}
/*  ===================================================  */

}
