#include "messenger_inc/SubRoutineInfo.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>


//Get a string containing the current time stamp.  It allocates space for the string as
//necessary.
extern "C" char* gettimestamp()
{
    //Get the time stamp...
    struct timeval fulltime;         //Get the timval structure
    gettimeofday(&fulltime, NULL);
    struct tm *local;
    time_t t = fulltime.tv_sec;      //The value of t comes from the timeval structure
    local = localtime(&t);           //local is used to get the year, month, etc. info.
    
    //Build the string.  The value of tv_usec is the number of microseconds.
    //I divide it by 1000 to get the milliseconds.
    static char strtime[256];
    sprintf(strtime, "%04d-%02d-%02d %02d:%02d:%02d.%d", 
        1900 + local->tm_year, local->tm_mon, local->tm_mday,
        local->tm_hour, local->tm_min, local->tm_sec, (int)(fulltime.tv_usec/1000));
    
    return strtime;
}

/////////////////////////////////////////////////////////////////////////////
//CONSTRUCTORS AND DESTRUCTORS
/////////////////////////////////////////////////////////////////////////////

//A basic constructor, accepting the subroutine name and version string.
SubRoutineInfo::SubRoutineInfo(const char* name, const char* version)
{
    
    //Initialize the pointers.
    _name = NULL;
    _version = NULL;
    _timestamp = NULL;
        
    //Try to set the two parameters of this SubRoutineInfo class based on the passed
    //in variables.
    setName(name);
    setVersion(version);
    setTimeStamp(gettimestamp());

    //Set pointers to null.
    _next = NULL;
    _prev = NULL;
}

SubRoutineInfo::~SubRoutineInfo()
{
    //Delete the name and version strings.
    delete [] _name;
    delete [] _version;
    delete [] _timestamp;
}



/////////////////////////////////////////////////////////////////////////////
//TOOLS
/////////////////////////////////////////////////////////////////////////////

//Remove this SubRoutineInfo from the linked list.  It will connect up the two links around
//itself.
//
//THIS ROUTINE SHOULD ALWAYS BE FOLLOWED BY A delete CALL!!!
//
//It will NOT remove itself if it is the only item in the list.  Why?  Because there is
//nothing to reconnect, so there is no reason for it to do anything.  So, FALSE is not a
//return that implies the routine failed.  It only implies that this is the last element
//of the linked list!
int SubRoutineInfo::removeFromList()
{
    //First, if _next and _prev are null, then return false.
    if ( (_next == NULL) && (_prev == NULL) )
        return false;
        
    //For _prev... If it is NOT NULL, then sets its _next to be this _next.
    if (_prev != NULL)
        _prev->setNext(_next);
    
    //Same for _next with respect to _prev.
    if (_next != NULL)
        _next->setPrev(_prev);
    
    //Now, there should be no other links in the chain connected to this one.
    _next = NULL;
    _prev = NULL;
    
    return true;
}

//Add this SubRoutineInfo object to the list before the passed in link.  Return false
//if the passed in link is NULL or if the passed in link is this.  This will avoid cyclical
//chains. 
int SubRoutineInfo::addToListBefore(SubRoutineInfo* link)
{
    if ( (link == NULL) || (link == this) )
       return false;
    
    //Three things to do... (1) set this previous to be link's previous, (2) set this next
    //to point to link, and (3) set link's previous to point to this.
    setPrev(link->getPrev());
    setNext(link);
    link->setPrev(this);
    
    return true; 
}

//Add this SubRoutineInfo object to the list after the passed in link.  Return false
//if the passed in link is NULL or if the passed in link is this.  This will avoid cyclical
//chains. 
int SubRoutineInfo::addToListAfter(SubRoutineInfo* link)
{
    if ( (link == NULL) || (link == this) )
       return false;
    
    //Three things to do... (1) set this next to be link's next, (2) set this previous
    //to point to link, and (3) set link's next to point to this.
    setNext(link->getNext());
    setPrev(link);
    link->setNext(this);
    
    return true; 
}


/////////////////////////////////////////////////////////////////////////////
//SETS AND GETS
/////////////////////////////////////////////////////////////////////////////

//Set the Name, returning false if the passed in pointer is NULL.
int SubRoutineInfo::setName(const char* name)
{
    //Return false if name is NULL.
    if (name == NULL)
        return false;

    //Delete the current name.
    if (_name != NULL)
        delete [] _name;
        
    //Allocate space.
    int len = strlen(name);
    _name = new char [len + 1];
    
    //Set the string.
    strcpy(_name, name);
    _name[len] = '\0';
    
    return true;
}

//Set the Version, returning false if the passed in pointer is NULL.
int SubRoutineInfo::setVersion(const char* version)
{
    //Return false if name is NULL.
    if (version == NULL)
        return false;

    //Delete the current name.
    if (_version != NULL)
        delete [] _version;
        
    //Allocate space.
    int len = strlen(version);
    _version = new char [len + 1];
    
    //Set the string.
    strcpy(_version, version);
    _version[len] = '\0';
    
    return true;
}

//Set the time stamp, blindly, to the passed in string.
int SubRoutineInfo::setTimeStamp(const char* timestamp)
{
    //Return false if timestamp is NULL.
    if (timestamp == NULL)
        return false;

    //Delete the current time stamp.
    if (_timestamp != NULL)
        delete [] _timestamp;
    
    //Allocate space.
    int len = strlen(timestamp);
    _timestamp = new char [len + 1];
    
    //Set the string.
    strcpy(_timestamp, timestamp);
    _timestamp[len] = '\0';
    
    return true;
}
        

//Set the next SubRoutineInfo item in the linked list.
void SubRoutineInfo::setNext(SubRoutineInfo* next)
{
    _next = next;
}

//Set the previous SubRoutineInfo item in the linked list.
void SubRoutineInfo::setPrev(SubRoutineInfo* prev)
{
    _prev = prev;
}

//Return the name of the subroutine.  This returns a pointer to the name.  You should copy
//it before manipulating it!
//
char* SubRoutineInfo::getName()
{
    return _name;
}

//Return the version of the subroutine.  This returns a pointer to the version.  You should copy
//it before manipulating it!
//
char* SubRoutineInfo::getVersion()
{
    return _version;
}

//Return the timestamp of the creation.  This returns a pointer to the timestamp.  You should copy
//it before manipulating it!
//
char* SubRoutineInfo::getTimeStamp()
{
    return _timestamp;
}
    
//Return the pointer to the next item in the linked list.
SubRoutineInfo* SubRoutineInfo::getNext()
{
    return _next;
}
    
//Return the pointer to the previous item in the linked list.
SubRoutineInfo* SubRoutineInfo::getPrev()
{
    return _prev;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/SubRoutineInfo.cxx,v $";
 static char rcs_id2[] = "$Id: SubRoutineInfo.cxx,v 1.1 2002/10/10 20:14:34 dws Exp $";}
/*  ===================================================  */

}


