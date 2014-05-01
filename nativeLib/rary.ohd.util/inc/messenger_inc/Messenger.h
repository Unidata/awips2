
#ifndef Messenger_INCLUDED
#define Messenger_INCLUDED

#include <stdio.h>

/////////////////////////////////////////////////////////////////////////////////////////////////
// The Prototype Messenger
/////////////////////////////////////////////////////////////////////////////////////////////////
//
// DESCRIPTION:
// 
// Messenger is a class that provides means by which a program can produce diagnostic output.  It
// is recommended that it be used via the createglobalmsg# and writemsg* functions.  
// Each works with the global Messenger object, GLOBAL_MSG.  The createglobalmsg# functions
// should be called once within each program (usually when the main routine starts up) in order
// to create the global messenger.  The global messenger should never be destroyed until the
// program quits.
//
// Event (evt) Levels:
// -------------------
//
// Messages are generated relative to events.  For example, and File I/O event may generate the 
// error message "File not found" or the status message "File successfully opened".  Both derive
// from the same event.  A program can define its own set of events or go with the default events
// provided in the global constants and variables given in msgwrappers.h.  To define your own, 
// call initmsg passing in the number of events you wish to define and an array of strings
// defining the title of the events.  The title will be printed out with the messages produced.
//
// Severity (sev) Levels:
// ----------------------
// 
// The severity level defines the type of message: debug, info, warning, or error.  For example,
// a warning message would include the SEV_WARNING constant in its bitcode value (see examples 
// below).  Constants to be used for the severity level are available in msgwrappers.h.
//
// Data Quality (dq) Levels:
// -------------------------
// 
// This number represents the quality of the data that should be expected after a message is
// sent.  For example, if an error occurs, and the data is completely bad, you should send a 
// message with dq of DQ_BAD.  However, if data is missing so that you have to estimate the data
// from surrounding points, then you would print a warning message with a dq of DQ_QUESTIONABLE.
// Constants to be used for the data quality level are available in msgwrappers.h.
// 
// Problem Status (ps) Levels:
// ---------------------------
//
// This number represents if an error that occurs is something an end user can correct, of if it
// should be left to the software programmer.  For example, if a file name is invalid, then the 
// error message would be sent with a probstat of PS_CORRECT (correctable).  However, if the 
// program fails to print data to an output file because fprintf failed, then an error message
// should be printed with a probstat of PS_NON_CORRECT.  Constants to be used for the problem
// status level are available in msgwrappers.h.   
//
// Trace Level (level):
// --------------------
//
// This number is a measure of how often the message should be printed.  If it is LESS THAN OR
// EQUAL TO the reqdlevel for the event of the message, then the message will be printed/logged
// accordingly.  If not, it will be ignored.  The value may vary from 0-127.
// 
/////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Date		Person		Comment
//      ----------      ------------    ----------------------------------------------
//      2002-05-20      Hank Herr       Prototype complete.
//      2002-05-21      Hank Herr       Made it use static constants so it would link.
//      2002-05-23      Hank Herr       Added data quality and problem status stuff.
//      2002-06-04      Hank Herr       Make writemsg use bitcode and simplified initmsg.
//
/////////////////////////////////////////////////////////////////////////////////////////////////

//ALL FUNCTIONS DESCRIBED ABOVE ARE PROTOTYPED INSIDE OF cwrappers.h!!!

//===================================================================================
//    Internal Routines
//===================================================================================
//No one should ever need to use anything below this point, except for the programmer
//maintaining the Messenger stuff.

//Modulus amounts used to acquire the values of the parameters.
#define EVT_MOD_LEVEL 128     // 2^7
#define EVT_MOD_SEV   512     // 2^9
#define EVT_MOD_PS    2048    // 2^11
#define EVT_MOD_DQ    8192    // 2^13
#define EVT_MOD_EVT   2097152 // 2^21

/*Maximum message length. */
#define MSG_MAX_LENGTH 1024


//The actual internal class that maintains this stuff.
class Messenger
{
    //Attributes
    
    int     _numevents;        //Stores the number of events.  If MISSING then messenger
                               //will not function.
    int*    _reqdlevels;       //The severity level required of an event to print message.
    char*   _filename;         //Output file name.
    char**  _eventnames;       //The name of the event.
    FILE*   _fileptr;          //Stores the current active pointer to the open file.
    
    public:
    
        //Constructors
        Messenger(int numevents, char** eventnames, char* filename, int* reqdlevels);
        ~Messenger();
    
        //Write the message, based on the required levels.  Each passed in int should
        //be a value starting from 0 and counting up by 1's.  In other words, DO NOT PASS IN
        //ONE OF THE CONSTANTS ABOVE... instead, pass in the constant modulus the 
        //corresponding EVT_MOD_* value above.
        void writeMessage(int msgevent, int severity, int level, int problemstatus, 
                          int dataqual, char* message);
    
        //Sets and gets.
        void setRequiredLevel(int msgevent, int reqdlevels);
    
    private:
    
        //Open the files.  Files that are used for more than one event are opened only once
        //and then those events'_fileptr elements will point to the same opened FILE 
        //structure.
        void openFile();
        
};

//The Global Variable that stores an instance of the Messenger class.
//This should be defined in Main with the desired parameters, or by calling
//createMSG(...) above.
static Messenger* GLOBAL_MSG = NULL;

#endif


