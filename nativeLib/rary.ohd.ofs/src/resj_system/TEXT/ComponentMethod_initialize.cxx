//------------------------------------------------------------------------------
// ComponentMethod :: initialize - initializes data members 
//------------------------------------------------------------------------------
// History:
//
// 16 Feb 2006  James R. VanShaar,  Riverside Technology, inc
//                        Created initial version.
//------------------------------------------------------------------------------

#include "ComponentMethod.h"

//------------------------------------------------------------------------------
// ComponentMethod :: initialize - Initializes required members.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     Integer specifying the success of the initialization.
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
Initializes required members.
@return int specification of initialization success.
*/
int ComponentMethod :: initialize()
{
    //char routine[] = "ComponentMethod :: initialize";
    _owner = NULL;
    return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ComponentMethod_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: ComponentMethod_initialize.cxx,v 1.1 2006/10/26 15:17:37 hsu Exp $";}
/*  ===================================================  */

}
