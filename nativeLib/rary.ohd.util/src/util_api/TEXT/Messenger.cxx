#include "messenger_inc/Messenger.h"
#include "messenger_inc/msgwrappers.h"
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

   
/////////////////////////////////////////////////////////////////////////////
//GLOBAL METHODS AND C WRAPPERS FOR Messenger OBJECT
/////////////////////////////////////////////////////////////////////////////

//The complete global call.  The value of numevents may change upon return
//if -999 is passed in.
extern "C" void initmsg(int* numevents, char** eventnames, char* filename, 
                        int* reqdlevels)
{
    //Check events for default.
    if (*numevents < 0)
    {
        *numevents = MSG_NUMBER_EVENTS;
        eventnames = MSG_DEF_EVENTS;
    }
    //Theoretically, this should never happen unless numevents is USE_DEFUALT_PARAM.
    //But, just in case, here it is.
    if (eventnames == NULL)
        eventnames = MSG_DEF_EVENTS;
        
    //Check filename for default.  Use default if it is NULL or 0-length.
    if (filename == NULL)
    {
        filename = new char[7];
        strcpy(filename, "stdout");
    }
    else if (filename[0] == '\0')
    {
        filename = new char[7];
        strcpy(filename, "stdout");
    }
    
    //Check reqdlevels for default.  If the required levels is NULL, then 
    //point it to MSG_DEF_LEVELS.
    if (reqdlevels == NULL)
    {
        reqdlevels = MSG_DEF_LEVELS;
    }
    //Otherwise, loop through the passed in levels.  If an individual element
    //is -999, then try to pull the default level from MSG_DEF_LEVELS.
    else
    {
        int i;
        for (i = 0; i < *numevents; i ++)
        {
            //If the passed in level is -999, then...
            if (reqdlevels[i] < 0)
            {
                //Use the MSG_DEF_LEVELS for that element if we are not off of
                //the end of the array.
                if (i < MSG_NUMBER_EVENTS)
                    reqdlevels[i] = MSG_DEF_LEVELS[i];
            }
        }
    }
    
    //Initialize the messenger.
    GLOBAL_MSG = new Messenger(*numevents, eventnames, filename, reqdlevels);
}


//The complete global call.  The value of numevents may change upon return
//if -999 is passed in.
extern "C" void initmsgc(int* numevents, char* eventnames, char* filename, 
                         int* reqdlevels, int* namelen)
{
    if (*namelen < 0)
        *numevents = USE_DEFAULT_PARAM;
    initmsg(numevents, breakstr(*numevents, *namelen, eventnames),
            filename, reqdlevels);
}


//This shuts down the GLOBAL_MSG.  It should always be done when finished using
//the API.  Normally, exiting the system will have the same effect as closemsg
//(i.e. the file write buffer will be dumped to the file), but if you are still
//executing the program and no longer want to use the GLOBAL_MSG object, then
//this function will allow you to shut it off manually.
extern "C" void closemsg()
{
    delete GLOBAL_MSG;
}

//The write messages.
//This one is used primarily by C/C++, since it uses a variable length list.
extern "C" void writemsg(int bitcode, const char* format, ... )
{
    if (GLOBAL_MSG == NULL)
    {
        int defevt = USE_DEFAULT_PARAM;
        initmsg(&defevt, NULL, NULL, NULL);
    }
        
    char message[MSG_MAX_LENGTH];
    
    //Construct the message.
    va_list ap;
    va_start ( ap, format );
    vsprintf ( message, format, ap );
    va_end (ap );

    //Acquire the parameters of the writeMessage call.
    int level = bitcode % EVT_MOD_LEVEL;
    bitcode  -= level;
    int sev   = (int) ((bitcode % EVT_MOD_SEV)/EVT_MOD_LEVEL);
    bitcode  -= sev;
    int ps    = (int) ((bitcode % EVT_MOD_PS) /EVT_MOD_SEV);
    bitcode  -= ps;
    int dq    = (int) ((bitcode % EVT_MOD_DQ) /EVT_MOD_PS);
    bitcode  -= dq;
    int evt   = (int) ((bitcode % EVT_MOD_EVT)/EVT_MOD_DQ);
    
    //Send to global.
    GLOBAL_MSG->writeMessage(evt, sev, level, ps, dq, message);
}

extern "C" void writemsgc(const int* bitcode, const char* message)
{
    //Send to global.
    writemsg(*bitcode,  message);
} 

//A tool to break down a string containing many fixed length substrings into an array
//of strings.  Parameters are the number of substrings within the string, the length
//of each substring, and the string, itself. 
extern "C" char** breakstr(int numstrings, int stringlen, const char* string)
{
    //Check for problems.
    if ( (numstrings < 0) || (stringlen < 0) || (string == NULL) )
        return NULL;

    //Declare the returned doulbe array and allocate space for the first dimension.
    char** returnarray = new char* [numstrings];
    
    int i, j;
    //For each string, do...
    for (i = 0; i < numstrings; i ++)
    {
        //Allocate the string having length stringlen
        returnarray[i] = new char [stringlen + 1];
        
        //Construct the string character by character.  j is the position within
        //the passed in string.  There might be a way to do this without working
        //character by character, but why bother.
        for (j = i * stringlen; j < (i + 1)*stringlen; j ++)
        {
            returnarray[i][j - i*stringlen] = string[j];
        }
        
        //Null terminate it.
        returnarray[i][stringlen] = '\0';
    }
    
    //Return the doulbe array.
    return returnarray;
}


/////////////////////////////////////////////////////////////////////////////
//CONSTRUCTORS AND DESTRUCTORS
/////////////////////////////////////////////////////////////////////////////

//This constructor takes an integer equal to the number of events, an array of strings
//for the file names where the output is to be sent, and an array of required severity
//levels.  Both arrays should be numevents long.  Both arrays are used directly, so
//DO NOT DELETE THE ARRAYS EXCEPT THROUGH THE DESTRUCTOR AFTER PASSING THEM IN!!! 
Messenger::Messenger(int numevents, char** eventnames, char* filename, int* reqdlevels)
{
    int TmpLen;

    //If numevents is invalid, then turn off the messenger by setting numevents
    //to MISSING.
    if (numevents <= 0)
    {
        _numevents = -1;
        return;
    }
    
    //Now, I want to allocate the space for the various storage devices and set pointers
    //to the two passed in array.
    _numevents    = numevents;
/*    _reqdlevels   = reqdlevels;
    _filename     = filename;
    _eventnames   = eventnames;
*//*Change so eventnames, reqdlevels, and filename do not have to be global
    or static variables.---kwz.  12-1-03*/
    _reqdlevels=new int[_numevents];
    _filename=new char[strlen(filename)+1];
    _eventnames=new char*[_numevents];

    strcpy(_filename,filename);
    TmpLen=strlen(eventnames[0])+1;/*added 1 for \n*/
    for (int i=0;i<_numevents;i++)
    {
      _eventnames[i]=new char[TmpLen];
	  strcpy(_eventnames[i],eventnames[i]);
	  _reqdlevels[i]=reqdlevels[i];
    }
    
    //Call the file opening routine.
    openFile();
}

//Destructor
//This routine does NOT delete any of the _reqdlevels, _filename, or _eventnames
//arrays.  These are merely pointers pointint to arrays passed into the 
//constructor.  As such, it is up to the routine calling the constructor to
//delete those arrays.
Messenger::~Messenger()
{
    //I will close the file pointer, however, to ensure that the file buffer is
    //cleared.
    fclose(_fileptr);

    //dispose whatever created in constructor.---kwz,12-1-03
    for (int i=0;i<_numevents;i++)
    {
      delete _eventnames[i];
    }
    delete _eventnames;
    delete _reqdlevels;
    delete _filename;	
}


/////////////////////////////////////////////////////////////////////////////
//TOOLS
/////////////////////////////////////////////////////////////////////////////

//Open the one file.
void Messenger::openFile()
{
    int i, j;
    FILE* file;
    
    //If the filename is NULL, then set the pointer to stdout.
    if (_filename == NULL)
        _fileptr = stdout;
    
    //If the file to open is "stdout", then just set the pointer to stdout.
    else if (strcmp(_filename, "stdout") == 0)
        _fileptr = stdout;
        
    //Same for stderr.
    else if ( strcmp(_filename, "stderr") == 0 )
        _fileptr = stderr;
        
    //Otherwise, just open the file,
    else
    {
        _fileptr = fopen(_filename, "a");
            
        //Check for an error.
        if (_fileptr == NULL)
        {
            fprintf(stderr, "MESSENGER WARNING: Unable to open output file, \"%s\".\n", _filename);
            fprintf(stderr, "MESSENGER WARNING: Defaulting to standard output.\n");
            _fileptr = stdout;
        }
    }
}

//Write a message to the appropriate location based on the passed in event.
void Messenger::writeMessage(int msgevent, int severity, int level, int problemstatus, 
                             int dataqual, char* message)
{
    //Check for valid messenger settings.
    if ( (_numevents > 0) &&                                  //If numevents is usable
         ((msgevent >= 0) && (msgevent < _numevents)) &&      //and msgevent is valid
         ((severity >= 0) && (severity < MSG_NUMBER_SEV)) &&  //and severity is valid
         (level <= _reqdlevels[msgevent] ) )                   //and it is severe enough...
    {
        //and then print to the file...
        //fflush(_fileptr);       //This forces the buffer to flush.
        //fseek(_fileptr, 0, 2);  //2 specifies to start at the end of the file.
        fprintf(_fileptr, "%s %s >> %s", _eventnames[msgevent], 
            MSG_DEF_SEV[severity], message);
    }
}
  
 
/////////////////////////////////////////////////////////////////////////////
//Sets and Gets
/////////////////////////////////////////////////////////////////////////////

//Set the required severity level of an event for it to be printed.  The higher it is,
//the more messages you will get.
void Messenger::setRequiredLevel(int msgevent, int reqdlevels)
{
    //Check for validity of the event.
    if ( (msgevent < 0) || (msgevent >= _numevents) )
        return;
    
    //Set the required severity.
    _reqdlevels[msgevent] = reqdlevels;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/Messenger.cxx,v $";
 static char rcs_id2[] = "$Id: Messenger.cxx,v 1.2 2003/12/29 15:03:50 wkwock Exp $";}
/*  ===================================================  */

}
   
