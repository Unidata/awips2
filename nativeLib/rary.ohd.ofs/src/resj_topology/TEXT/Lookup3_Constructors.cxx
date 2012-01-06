//------------------------------------------------------------------------------
// Lookup3 :: Lookup3 - Constructors.
//------------------------------------------------------------------------------
// History:
// 
// 01 Feb 2006  James R. VanShaar, RTi    Created initial version
//------------------------------------------------------------------------------
#include "Lookup3.h"
#include "Reservoir.h"
#include "Node.h"

//------------------------------------------------------------------------------
// Lookup :: Lookup - Constructor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     
// Calls:
//     Method::getUnitType()
//     PrintWarning( int, char*, char* )
//     initialize()
// Errors:
//     None
// Warnings:
//     Inability to set owner due to virtual ComponentMethod( Component, char* )
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Constructs a Lookup3 method using virtual ComponentMethod( Component, char* ).
@return 
@param owner pointer to owning Component
@param type character array defining the type of the owning Component
*/
Lookup3 :: Lookup3( Component* owner, char* type ) : 
           ComponentMethod( owner, type )
{
    char    routine[]="Lookup3 :: Lookup3";

    if( owner == NULL ) {
        PrintWarning( 1, routine, "Lookup3 :: constructor cannot set owner to "
            "NULL." );
    }

    initialize();

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Lookup3_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Lookup3_Constructors.cxx,v 1.1 2006/10/26 15:23:57 hsu Exp $";}
/*  ===================================================  */

}
