
#ifndef msgwrappers_INCLUDED
#define msgwrappers_INCLUDED

#include <stdio.h>

/*
**========================================================================================
** THIS IS A COLLECTION OF C WRAPPERS ON THE STUFF IN Messenger.h AND SubRoutineStack.h
** ALL FUNCTIONS ARE DECLARED IN Messenger.cxx AND SubRoutineStack.cxx
**========================================================================================
*/

/*
**========================================================================================
** GLOBAL MESSENGER (source code is in Messenger.cxx) 
**========================================================================================
**
** Usage:
** ------
** 
** It is recommended that you use the GLOBAL_MSG object to produce messages.  GLOBAL_MSG
** is defined within Messenger.h (at the bottom).  To use it...
** 
** (1) Include "messenger_inc/msgwrappers.h" inside of the source file directly or via another
**     include file.
** (2) call one of the initmsg* functions as early as possible within main() in order to
**     initialize the global messenger.  C/C++ should call initmsg, and Fortran should call 
**     initmsgc.
**
**     NOTE: If a program defines its own event list, then it is responsible for defining 
**           constants for use in writemsg as appropriate (i.e. an event constant equals the
**           index of the event within the event list MULTIPLIED BY 2^13).
**
** (3) call one of the writemsg* methods to send out the messages.  C should call writemsg
**     and Fortran should call writemsgc.  The arguments passed in are the message, of course,
**     and an integer that defines the characteristics of the message.  To construct the
**     integer, simply ADD the constants corresponding to the event, severity, data quality,
**     problem status, and trace level you wish to use.    See examples below.  
**
**     NOTE: If a initmsg* has not been called when a writemsg* is called, then 
**           initmsg(...) will be called automatically, using all the defaults,
**           and sending the message to stdout.
**     NOTE: Use at most ONE value from each set of constants when performing the addition.
**
** (4) call closemsg() or exit the program in order to close the GLOBAL_MSG output file.
**
** Examples:
** ---------
**
** - Example initialization, sending stuff to a file and using defaults for other params:
**       int defevt    = -1;
**       initmsg(&defevt, NULL, argv[1], NULL); 
**
** - The message below is a message that will always get printed:
**     writemsg( EVT_SOURCE_IO + ALWAYS_PRINT + SEV_INFO, " THIS IS A START!!!\n"); 
**
** - This message will print only if the reqd_level is above TIER_1, or 20:
**     writemsg( EVT_PARAMETERS + TIER_1 + SEV_DEBUG,      "This is a TIER_1 message!!!\n");
** 
** - This message uses the variable length argument list version, with a %s in it:
**     char test[4] = "ME!";    
**     writemsg( EVT_LOGGING + ALWAYS_PRINT + SEV_WARNING, "The string is: %s\n", test);
**     
** - This message uses the full argument list to print an error:
**     writemsg(EVT_FINISH_RTN + ALWAYS_PRINT + SEV_ERROR + PS_NON_CORRECT + DQ_BAD, 
**         "FULL ERROR MESSAGE!!!!!\n");
**   
** Conventions:
** ------------
**
** - Try to use the values defined within the constants below whenever possible.
** 
**========================================================================================
*/

#define   USE_DEFAULT_PARAM -999

/*
**Key values of the trace level.  If a message is printed with a trace level
**of ALWAYS_PRINT, the message will always be printed, unless the user specifically
**overrides the required priority and sets it to be 0.  The trace level varies from
**0 through 127.  
**BITS 0-7
*/
#define   ALWAYS_PRINT      1
#define   TIER_1            20
#define   TIER_2            40
#define   TIER_3            60
#define   TIER_4            80
#define   TIER_5            100

/*
**Available Severity Levels
**BITS 8, 9
*/ 
#define   MSG_NUMBER_SEV    4  
#define   SEV_DEBUG         0      /*0*/
#define   SEV_INFO          128    /*1*/
#define   SEV_WARNING       256    /*2*/
#define   SEV_ERROR         384    /*3*/
static char* MSG_DEF_SEV [] = { "DEBUG  ", "INFO   ", "WARNING", "ERROR  " };

/*
**Available Problem Statuses
**BITS 10, 11
*/
#define   MSG_NUMBER_PS     4
#define   PS_UNKNOWN        0      /*0*/
#define   PS_NONE           512    /*1*/
#define   PS_CORRECT        1024   /*2*/
#define   PS_NON_CORRECT    1536   /*3*/

/*
**Available Data Quality Levels
**BITS 12, 13
*/
#define   MSG_NUMBER_DQ     4
#define   DQ_UNKNOWN        0      /*0*/ 
#define   DQ_GOOD           2048   /*1*/
#define   DQ_QUESTIONABLE   4096   /*2*/
#define   DQ_BAD            6144   /*3*/

/*
**Default events settings.
**BITS 14-21
*/
#define   MSG_NUMBER_EVENTS 11     /*The default number of events*/
#define   EVT_UNKNOWN       0      /*0*/
#define   EVT_LOGGING       8192   /*1*/
#define   EVT_START_RTN     16384  /*2*/
#define   EVT_FINISH_RTN    24576  /*3*/
#define   EVT_PROGRESS      32768  /*4*/
#define   EVT_PARAMETERS    40960  /*5*/
#define   EVT_SOURCE_IO     49152  /*6*/
#define   EVT_DATA          57344  /*7*/
#define   EVT_MISC          65536  /*8*/
#define   EVT_REGRESSION    73728  /*9*/
#define   EVT_GUI           81920  /*10*/
static char* MSG_DEF_EVENTS []= { "", "LOGGING   ", "START RTN ", "FINISH RTN", "PROGRESS  ", 
                                      "PARAMETERS", "SOURCE I/O", "DATA      ", "MISC      ", 
                                      "REGRESSION", "GUI       "};
static int   MSG_DEF_LEVELS []= { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};


/* 
** ############### This is the section that will be used by C++ ######################
*/
#ifdef __cplusplus

/*
**====================================================================================
** INITIALIZER/CLOSER FUNCTIONS (Source code is in Messenger.cxx) 
**====================================================================================
**
**All can be called from C or Fortran.  Here are the parameters:
**
**    numevents  -- an integer value representing the number of events to use, 
**                  if not using default.
**    eventnames -- an array of strings representing the titles of those events,
**                  if not using default.
**    filename   -- the name of the output file.
**    reqdlevels -- an array of required severity levels for each event.  If a
**                  message as a severity less than the required severity, it 
**                  will be printed.
**
**Each parameter has a default setting.  The following lists the value that results
**in using the default and what that default is.
**
**    Parameters    Value For Default        Default
**    ----------    -----------------        -------
**    numevents  -- negative integer         MSG_NUMBER_EVENTS
**    eventnames -- numevents is defaulted   MSG_DEF_EVENTS
**    filename   -- NULL or ""               "stdout" for standard output
**    reqdlevels -- negative for a level     corresponding element within MSG_DEF_LEVELS
**                  or NULL pointer          MSG_DEF_LEVELS
**
**Specify all parameters manually (baseline):
*/

extern "C" void initmsg(int* numevents, char** eventnames, char* filename, int* reqdlevels);

/*
**Special fortran version necessary because of the 2-d array of characters.
*/
extern "C" void initmsgc(int* numevents, char* eventnames, char* filename, int* reqdlevels,
                         int* namelen);

/*
**Fortran and C wrapper to destroy the GLOBAL_MSG object.
*/
extern "C" void closemsg();

/*
**===================================================================================
** MESSAGE WRITING FUNCTIONS (Source code is in Messenger.cxx) 
**===================================================================================
**Wrappers on the write message routine.  The "..." is usable by C and C++, and is
**illustrated in ESPADP code.
**
**BOTH REQUIRE NULL TERMINATED STRINGS!!! (i.e. last character is a '\0')
*/

extern "C" void writemsg(       int  bitcode, const char* format, ... );  /*C/C++  */
extern "C" void writemsgc(const int* bitcode, const char* message);       /*Fortra */

/* 
**An internal routine to breakdown one string into multiple strings 
*/
extern "C" char** breakstr(int numstrings, int stringlen, const char* string);


/*
**===================================================================================
** SUBROUTINE STACK FUNCTIONS (Source code is in SubRoutineStack.cxx)
**===================================================================================
**
** Usage:
** ------
** Provided in SubRoutineStack.h is a GLOBAL_STACK variable, which is a global 
** variable visible to all routines that include the SubRoutineStack.h file. 
** To use it... 
**
** - When you enter a routine, call enterrtn, passing in the routine 
**   name and version.  The GLOBAL_STACK will be initialized the first
**   time enterrtn is called.
** - When you leave a routine, call leavertn. 
**
** These are all routines that should be usable by any C or Fortran function.   
** 
** ALL char* MUST BE NULL TERMINATED STRINGS!!! (i.e. last character is a '\0')
*/
extern "C" void enterrtn(const char* name, const char* version);  /*Call from C or Fortran Wrapper */ 
extern "C" void leavertn(const char* name);                       /*Call from C or Fortran */
extern "C" void createglobalstack();


#else

/* 
** ################ This is the section that will be used by C ######################
**
** This is a restatement of the above externs without the "C" so that a C compiler
** can understand it.  See the above comments within C++ section for a description.
*/
extern void initmsg(int* numevents, char** eventnames, char* filename, int* reqdlevels);
extern void initmsgc(int* numevents, char* eventnames, char* filename, int* reqdlevels, int* namelen);
extern void closemsg();
extern void writemsg( int  bitcode, const char* format, ... ); 
extern void writemsgc(const int* bitcode, const char* message); 
extern char** breakstr(int numstrings, int stringlen, const char* string);     
extern void enterrtn(const char* name, const char* version);  
extern void leavertn(const char* name);              
extern void createglobalstack();

#endif /* End of the if clause for _cplusplus */

#endif /* End of the if clause for cwrappers_INCLUDED */

