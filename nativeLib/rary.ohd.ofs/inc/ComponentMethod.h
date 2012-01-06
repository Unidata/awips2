//------------------------------------------------------------------------------
// ComponentMethod - Derived class for methods that only act on reservoir
//             components.
//------------------------------------------------------------------------------
// Copyright:    See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 16 Feb 2006  James R. VanShaar,  Riverside Technology, inc
//                        Created initial version.
//------------------------------------------------------------------------------
// Variables:    I/O    Description        
//
//
//------------------------------------------------------------------------------

#ifndef ComponentMethod_INCLUDED
#define ComponentMethod_INCLUDED

#include "Component.h"
#include "Method.h"

#define    NORMAL 0
#define INTERPOLATE 1
#define INTERPOLATE_TIME 1 
#define INTERPOLATE_ELEV 2 
#define INTERPOLATE_ALL 3 
#define INTERPOLATE_ROWS 1 
#define INTERPOLATE_COLS 2 


class ComponentMethod : public Method
{
public:
    ComponentMethod( Component*, char* CompType );  // Default constructor.
    //ComponentMethod( Component* );  // Default constructor.

    ComponentMethod( const ComponentMethod&, Component* );  // Copy constructor.

    virtual ~ComponentMethod();  // Default destructor.

    Component*      getOwner();  // returns _owner

    int         getOwnerType();  // returns _ownerType

    ComponentMethod* copy( Component* ) = 0;

    virtual int solveMethod( TSDate&, int, double** = NULL ) = 0;

    virtual int print( FILE* ) = 0;

    virtual int freeDataSpace() = 0;

    enum
    {
        CM_RESERVOIR = 1,
        CM_NODE      = 2,
        CM_REACH     = 3
    };

private:
    int initialize();

protected:
    Component*     _owner;
    int _ownerType;          // The component type owning this method.

};

#endif
