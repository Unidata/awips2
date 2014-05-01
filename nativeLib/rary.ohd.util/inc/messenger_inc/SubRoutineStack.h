
#ifndef SubRoutineStack_INCLUDED
#define SubRoutineStack_INCLUDED

#include <stdio.h>
#include "messenger_inc/SubRoutineInfo.h"

/////////////////////////////////////////////////////////////////////////////////////////////////
// The SubRoutineStack Class
/////////////////////////////////////////////////////////////////////////////////////////////////
// 
// DESCRIPTION:
// 
// This class maintains a stack of SubRoutineInfo objects.  It makes use of the
// linked list option within the objects, and records the bottom and top 
// elements of the stack.  
//
// Usage (functions are prototyped inside of cwrappers.h):
// ------
//
// Simple... just create a stack using the constructor.  Whenever you need to
// add a SubRoutineInfo to the top of the stack, call pushRoutine, passing in
// the arguments you would normally pass into the SubRoutineInfo constructor,
// namely the routine name and version.  
//
// When you leave the routine, or just want to remove from the top of the stack
// call popRoutine.  It will return false if there is nothing left to pop.
//
// If you want to read from the element at the top of the stack, call
// getTopSubRoutine().  For example, getTopSubRoutine()->getName() will return
// the name of the routine on the top of the stack.
//
/////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Date		Person		Comment
//      ----------      ------------    ----------------------------------------------
//      2002-05-20      Hank Herr       Prototype complete.
//      2002-05-21      Hank Herr       Made GLOBAL_STACK static so that it will link.
//      2002-05-23      Hank Herr       Added time stamp.
//
/////////////////////////////////////////////////////////////////////////////////////////////////

#define THRESHOLD_STACK_SIZE 1000

//ALL GLOBAL FUNCTIONS ARE PROTOTYPED INSIDE OF cwrappers.h !!!

class SubRoutineStack
{
    //Attributes
    
    SubRoutineInfo* _bottom;    //Points to the first subroutine in stack.
    SubRoutineInfo* _top;       //Points to the last subroutine in stack.
    int             _count;     //The size of the stack.
    
    
    public:

        //Constructor:  Initialize all pointers above to NULL.
        SubRoutineStack();
        
        //Push and Pop Stuff.
        int pushRoutine(const char* name, const char* version);
        int popRoutine();
        int popRoutine(const char* name);
        
        //Sets and gets
        SubRoutineInfo* getTopSubRoutine();

};

static SubRoutineStack* GLOBAL_STACK = NULL;

#endif

