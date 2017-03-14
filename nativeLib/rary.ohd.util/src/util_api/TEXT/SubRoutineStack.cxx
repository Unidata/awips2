#include "messenger_inc/SubRoutineStack.h"
#include "messenger_inc/msgwrappers.h"
#include <stdlib.h>
#include <string.h>


/////////////////////////////////////////////////////////////////////////////
//GLOBAL METHODS AND C WRAPPERS FOR SubRoutineStack OBJECT
/////////////////////////////////////////////////////////////////////////////


//Create the GLOBAL_STACK.  Call this before using either of the two above
//functions.  It is best to call this within main(...) as soon as program
//starts.
//
//NEVER DELETE THE GLOBAL STACK.  Just let it lie there and when the program
//exits, it will be destroyed automatically.  However, if you try to create
//more than one GLOBAL_STACK, this will delete the old one before making the
//new, but you should have no reason to do that.
extern "C" void createglobalstack()
{
    if (GLOBAL_STACK != NULL)
        delete GLOBAL_STACK;
    GLOBAL_STACK = new SubRoutineStack();
}

//Call this function upon entering a new subroutine.  This version assumes
//a NULL ('\0') terminated string.
extern "C" void enterrtn(const char* name, const char* version)
{
    //If GLOBAL_STACK is NULL, then you need to initialize it.
    if (GLOBAL_STACK == NULL)
        createglobalstack();
        
    //Push the passed in name and versin onto the GLOBAL_STACK.
    //Via the check above, the if should always be true, but just in case,
    //I don't want a core dump.
    if (GLOBAL_STACK != NULL)
        GLOBAL_STACK->pushRoutine(name, version);
}

//Call this function upon leaving a subroutine.
extern "C" void leavertn(const char* name)
{
    //Pop the top off of the GLOBAL_STACK.
    if (GLOBAL_STACK != NULL)
        GLOBAL_STACK->popRoutine(name);
}


/////////////////////////////////////////////////////////////////////////////
//CONSTRUCTORS AND DESTRUCTORS
/////////////////////////////////////////////////////////////////////////////

SubRoutineStack::SubRoutineStack()
{
    //Set the two pointers to NULL.
    _bottom = NULL;
    _top    = NULL;
    _count  = 0;
}


/////////////////////////////////////////////////////////////////////////////
//TOOLS
/////////////////////////////////////////////////////////////////////////////

//Push a routine with the passed in name and version onto the top of the stack.
//
//Return false if either string is NULL or if the name is a zero-length string.
//I will, however, allow a zero-length version string (implying we just ignore
//the version).
int SubRoutineStack::pushRoutine(const char* name, const char* version)
{
    //Check for null strings.  Return false if bad.
    if ( (name == NULL) || (version == NULL) )
        return false;
    
    //Check for 0-length name (I don't care about version being 0.
    if (strlen(name) == 0)
        return false;
    
    //Create a SubRoutineInfo object with name and version.
    SubRoutineInfo* info = new SubRoutineInfo(name, version);
    
    //If _bottom is NULL, then this is the first element of the stack.
    //Just set _top and _bottom to point to info.
    if (_bottom == NULL)
    {
        _bottom = info;
        _top    = info;
    }
    
    //Otherwise, Add it after _top and set _top to it.
    else
    {
        //Add it to the list after of _top.
        info->addToListAfter(_top);
        _top = info;
    }
    writemsg(EVT_START_RTN + SEV_INFO + TIER_1, "Entering routine %s (%s) at %s.\n", 
        name, version, _top->getTimeStamp());
    
    //Increase the count.
    _count ++;
    if (_count > THRESHOLD_STACK_SIZE)
    {
        writemsg(EVT_START_RTN + SEV_WARNING + TIER_1,
            "The stack size has become larger than %d.\n", THRESHOLD_STACK_SIZE);
    }
    
    return true;
}

//Pop the routine off of the top and discard it.  THIS WILL NOT RETURN TOP
//but will simply destroy it.  To read from the top, call the getTopSubRoutine
//below.
//
//Return false if there is nothing to pop.
int SubRoutineStack::popRoutine()
{
    //If there is nothing to pop, return false.
    if (_bottom == NULL)
        return false;
    
    //If _bottom is _top, then we just delete what bottom and top point to
    //and set both to null.
    if (_bottom == _top)
    {
        delete _bottom;
        _bottom = NULL;
        _top = NULL;
        return true;
    }
    
    //Top is not bottom, so remove top.  To do so, do the following:
    //  (1) get the link before _top and store it in info.
    //  (2) call removeFromList for _top, which will set links around _top
    //      accordingly.
    //  (3) delete the old _top.
    //  (4) set the new _top to be info, or the old previous.
    //  (5) decrease the count.
    SubRoutineInfo* info = _top->getPrev();
    _top->removeFromList();
    delete _top;
    _top = info;
    _count --;
    
    return true;
}

//Pop all the routines above and including the routine specified by name
//from the stack.
//
//Return false if there is nothing to pop.
int SubRoutineStack::popRoutine(const char* name)
{
    //If there is nothing to pop, return false.
    if (_bottom == NULL)
        return false;

    //Search the linked list from the _top down until I find the first routine
    //with a name of name.
    SubRoutineInfo* current = _top;
    while ( strcmp(current->getName(), name) != 0 ) 
    {
        current = current->getPrev();
        
        //If I reach the point where current == NULL, then I've searched the
        //full list and there was no routine by that name.
        if (current == NULL)
            return false;
    }
    
    writemsg(EVT_FINISH_RTN + SEV_INFO + TIER_1, "Leaving routine %s (%s) at %s.\n", 
        name, current->getVersion(), gettimestamp());
    
    //Pop all of the routines until we reach this one.
    while (_top != current)
        popRoutine();
    
    //Finally, pop this one.
    return popRoutine(); 
}


/////////////////////////////////////////////////////////////////////////////
//SETS AND GETS
/////////////////////////////////////////////////////////////////////////////

//Grab a pointer to the current top of the stack.  This will return the pointer
//to the actual top of the stack.  Hence, ONLY USE THE GET COMMANDS!!!  Do not
//call the add or remove methods from the returned pointer.
//
//If NULL is returned, then the stack is empty.
SubRoutineInfo* SubRoutineStack::getTopSubRoutine() 
{
    return _top;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/SubRoutineStack.cxx,v $";
 static char rcs_id2[] = "$Id: SubRoutineStack.cxx,v 1.1 2002/10/10 20:15:30 dws Exp $";}
/*  ===================================================  */

}
