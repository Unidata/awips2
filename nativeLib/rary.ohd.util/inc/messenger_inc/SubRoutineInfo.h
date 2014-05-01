
#ifndef SubRoutineInfo_INCLUDED
#define SubRoutineInfo_INCLUDED

#include <stdio.h>

/////////////////////////////////////////////////////////////////////////////////////////////////
// The SubRoutineInfo Class
/////////////////////////////////////////////////////////////////////////////////////////////////
//
// DESCRIPTION:
//   
// This class stores the name and version of a subroutine for use with SubRoutineStack.  It also
// provides tools necessary to manage a linked list of SubRoutineInfos.  Finally, whenever a new
// object is created, its creation time is stored in a time stamp, which can be used in
// conjunction with the SubRoutineStack to determine when a subroutine begins in time. 
//
// Usage:
//
// Call constructor passing in the name and version.  Then use the addToList* routines to put
// it in the correct location.
//
// Once the piece is no longer needed, just call the following routines assuming info is defined
// as a SubRoutineInfo*...
//
//     info->removeFromList();
//     delete info;
//
// NOTE: the setName and setVersion routines will COPY the passed in string.  It does not use the
//       pointers directly.
// NOTE: the setNext and setPrev routines used the pointer passed in directly.  So, do not destroy
//       what you pass in after passing it in.  Leave it intact and allow SubRoutineInfo to manage
//       it.
// 
/////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Date		Person		Comment
//      ----------      ------------    ----------------------------------------------
//      2002-05-16      Hank Herr       Prototype complete.
//
/////////////////////////////////////////////////////////////////////////////////////////////////

//This function is used to get the time stamp.
extern "C" char* gettimestamp();


class SubRoutineInfo
{
    //Name and version are dynamically allocated character arrays.  
    
    char* _name;            //Stores the name.
    char* _version;         //Stores the version.
    char* _timestamp;       //The time at which this object was created.
    SubRoutineInfo* _next;  //Points to next item in stack.
    SubRoutineInfo* _prev;  //Points to previous item in stack.


    public:
    
        //Constructor:  Pass in the name and version of routine.  Sets the 
        //    _next and _prev to be NULL.
        SubRoutineInfo(const char* name, const char* version);
        
        //Destructor:  Destroy the _name and _version arrays.  
        ~SubRoutineInfo();
        
        //Tools
        int removeFromList();                      //Remove this from list, by manipulating links.
        int addToListBefore(SubRoutineInfo* link); //Add this to list before passed in link.
        int addToListAfter(SubRoutineInfo* link);  //Add this to list after passed in link.
        
        //Sets and Gets
        int setName(const char* name);               
        int setVersion(const char* version);  
        int setTimeStamp(const char* timestamp);     
        void setNext(SubRoutineInfo* next);     
        void setPrev(SubRoutineInfo* prev);      
        char* getName();
        char* getVersion();
        char* getTimeStamp(); 
        SubRoutineInfo* getNext();
        SubRoutineInfo* getPrev();

};  
 
#endif



