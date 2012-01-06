//------------------------------------------------------------------------------
// ComponentMethod :: ComponentMethod - Constructors.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// History:
//
// 16 Feb 2006  James R. VanShaar,  Riverside Technology, inc
//                        Created initial version.
//------------------------------------------------------------------------------
#include "ComponentMethod.h"

//------------------------------------------------------------------------------
// ComponentMethod :: ComponentMethod - Constructor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     initialize()
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Constructs a ComponentMethod
@return ComponentMethod
@param owner Component that will own the method
@param type Character string defining the type of component owner.
*/
ComponentMethod :: ComponentMethod( Component* owner, char* type ) : Method() 
{
    if( !strcmp( "RESERVOIR", type ) )
    {
        // If constructed in this way, method is owned by a reservoir.
        _ownerType = CM_RESERVOIR;
    }
    else if( !strcmp( "NODE", type ) )
    {
        // If constructed in this way, method is owned by a node.
        _ownerType = CM_NODE;
    }
    else
    {
        // If constructed in this way, method is owned by a reach.
        _ownerType = CM_REACH;
    }

    initialize();
    _owner = owner;
}

//------------------------------------------------------------------------------
// ComponentMethod :: ComponentMethod - Constructor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     initialize()
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Constructs a ComponentMethod
@return ComponentMethod
@param meth Pointer to a ComponentMethod upon which this new ComponentMethod
 will be based.
@param owner Component that will own the method
*/
ComponentMethod :: ComponentMethod( const ComponentMethod& meth, 
    Component* owner ) : Method( meth )
{
    //char    routine[]="ComponentMethod :: ComponentMethod";

    initialize();

    _ownerType = meth._ownerType;
    _owner = meth._owner;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ComponentMethod_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: ComponentMethod_Constructors.cxx,v 1.1 2006/10/26 15:12:48 hsu Exp $";}
/*  ===================================================  */

}
