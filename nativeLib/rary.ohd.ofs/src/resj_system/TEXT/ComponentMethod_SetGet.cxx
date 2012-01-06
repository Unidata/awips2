//------------------------------------------------------------------------------
// ComponentMethod_SetGet - base SetGet for common scheme data.
//------------------------------------------------------------------------------
// History:
//
// 16 Feb 2006  James R. VanShaar,  Riverside Technology, inc
//                        Created initial version.
//------------------------------------------------------------------------------

#include "ComponentMethod.h"

//------------------------------------------------------------------------------
// ComponentMethod :: getOwner - Returns a Component pointer to _owner
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     None
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
Returns a Component pointer to _owner.
@return Component* to _owner.
*/
Component* ComponentMethod :: getOwner()
{
    return( _owner );
}

//------------------------------------------------------------------------------
// ComponentMethod :: getOwnerType - Returns an integer specifying the type of
//                                   the owning component.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     None
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
Returns the integer value of _ownerType.
@return int _ownerType
*/
int ComponentMethod :: getOwnerType()
{
    return( _ownerType );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ComponentMethod_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: ComponentMethod_SetGet.cxx,v 1.1 2006/10/26 15:17:00 hsu Exp $";}
/*  ===================================================  */

}

