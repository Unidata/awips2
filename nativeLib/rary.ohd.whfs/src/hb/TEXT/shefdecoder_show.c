/*******************************************************************************
* FILENAME:            shefdecoder_show.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h> /* For pid_t definition. */
#include <sys/wait.h> /* For "wait()" declaration. */
#include <unistd.h> /* For "execlp(), fork()" declarations. */
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/Xm.h>

#include "GeneralUtil.h"
#include "shefdecoder.h"
#include "shefdecoder_show.h"
#include "Xtools.h"

#define SHEFDECODE_HOSTNAME_LEN 100

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static char shefdecode_hostname [ SHEFDECODE_HOSTNAME_LEN ] ;

static int fork_process ( char * process_path_and_name ,
                          char * o_option , char * host_name ,
                          char * program ) 
{
   int child_status = 99 ;
   int stat_loc ;
   pid_t process_id ;

   if ( process_path_and_name == NULL ||
        o_option == NULL || host_name == NULL || program == NULL )
   {
      return child_status ;
   }

   /* Create a new child process. */
   errno = 0 ;

   fprintf ( stdout , "\nIn routine 'fork_process':\n"
                      "Forking to create a child process\n" ) ;
   process_id = fork ( ) ;

   if ( process_id == -1 )
   {
      switch ( errno )
      {
         case EAGAIN :

            fprintf ( stderr , "\nIn routine 'fork_process':\n"
                               "The system-imposed limit on the number of\n"
                               "processes has been exceeded. Cannot start\n"
                               "the Shefdecoder.\n" ) ;
            break ;

         case ENOMEM :

            fprintf ( stderr , "\nIn routine 'fork_process':\n"
                               "There is not enough memory to create\n"
                               "the start_shefdecode process.\n" ) ;
            break ;

         default :

            fprintf ( stderr , "\nIn routine 'fork_process':\n"
                               "The attempt to create a new process\n"
                               "for the start_shefdecode script has\n"
                               "failed.\n" ) ; 
            break ;
      }
   }
   else
   {
      /* Determine if this is the parent or child process.  If it is the
         child, then run the start_shefdecode script in it. */
      if ( process_id == 0 )
      {
         fprintf ( stdout , "\nIn routine 'fork_process':\n"
                            "Performing an execlp of command %s with\n"
                            "arguments '%s %s %s' in child process %d.\n" , 
                            process_path_and_name , o_option , host_name , 
                            program , process_id ) ;
         execlp ( process_path_and_name , process_path_and_name , o_option , 
                  host_name , program , ( char * ) 0 ) ;
         fprintf ( stderr , "\nIn routine 'fork_process':\n"
                            "The attempt to execlp the process '%s' with\n"
                            "options '%s %s %s' has failed.\n" ,
                            process_path_and_name , o_option , host_name , 
                            program ) ;
         exit ( 99 ) ;
      }
      else
      {
         /* This is the parent. Wait for the child to complete. */
        waitpid ( process_id , & stat_loc , 0 ) ;

        if ( stat_loc == 0 )
        {
           /* The attempt to start the Shefdecoder was successful. */
           child_status = 0 ;
        }
        else if ( WIFEXITED ( stat_loc ) != 0 )
        {
           /* A status code was returned from the child process. */
           child_status = WEXITSTATUS ( stat_loc ) ;
        }

      }
   }

   return child_status ;
}

static void start_shefdecode ( Widget w, XtPointer clientdata ,
                               XtPointer calldata )
{
   static char * o_option = "-oBatchMode=yes" ;

   static char options [ BUFSIZ ] ;
   char reply [ BUFSIZ ]  ;
   static char * shefdecode_bin_token = "SHEFDECODE_BIN" ;
   static char * ssh_command = "ssh" ;
   int child_status ;
   int reply_len ;
   int request_len ;

   memset ( options , '\0' , BUFSIZ ) ;
   memset ( reply , '\0' , BUFSIZ ) ;

   request_len = strlen ( shefdecode_bin_token ) ;
   child_status = get_apps_defaults ( shefdecode_bin_token , 
                                      & request_len ,
                                      reply ,
                                      & reply_len ) ; 

   if ( child_status != 0 )
   {
      XmTextSetString ( shefdecodeTX ,
                      "The shefdecode_bin token is undefined.\n"
                      "Cannot find the start_shefdecode script.\n" ) ;
      fprintf ( stderr , "In routine 'start_shefdecode':\n"
                         "The shefdecode_bin token is undefined.\n"
                         "Cannot find the start_shefdecode script.\n" ) ;
      return ;
   }

   strcat ( reply , "/start_shefdecode -i" ) ; 
   
   sprintf ( options , "-oBatchMode=yes %s %s" , 
             shefdecode_hostname , reply ) ;

   /* Create a new child process. */
   child_status = fork_process ( ssh_command , o_option ,
                                shefdecode_hostname , reply ) ;

   switch ( child_status )
   {
      case 0 :
             
             /* The attempt to start the shefdecoder was successful. */
             XmTextSetString ( shefdecodeTX ,
                             "The Shefdecoder has been started.\n" ) ; 
             break ;

      case 1 :

             /* An invalid argument was passed to this script. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to start the Shefdecoder failed.\n"
                             "An invalid argument was passed to the\n"
                             "start_shefdecode script.\n" ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "An invalid argument '%s' was passed into\n"
                                "the start_shefdecode script. Cannot start\n"
                                "The Shefdecoder.\n" , reply ) ;  
             break ;

      case 2 :

             /* The shefdecode_userid token is not defined. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to start the Shefdecoder failed.\n"
                             "The shefdecode_userid token has not been\n"
                             "set.\n" ) ;  
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "The shefdecode_userid token has not been\n"
                                "defined.  Cannot start the shefdecoder.\n" ) ;
                       
             break ;

      case 3 :

             /* The user does not have permission to start Shefdecoder. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to start the Shefdecoder failed.\n"
                             "The user does not have permission to start\n"
                             "Shefdecoder.\n" ) ; 
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "User does not have permission to\n"
                                "start Shefdecoder.\n" ) ;
             break ;


      case 4 :
           
             /* The directory specified for the data files does not exist. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to start the Shefdecoder failed.\n"
                             "The directory specified for the Shefdecoder\n"
                             "data files does not exist.  Check the value\n"
                             "of token shef_data_dir.\n" ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "The directory specified for the Shefdecoder\n"
                                "data files does not exist. Check the value\n"
                                "of token shef_data_dir.\n" ) ;
             break ;

      case 5 :

             /* The directory specified for the log files does not exist. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to start the Shefdecoder failed.\n"
                             "The directory specified for the Shefdecoder\n"
                             "log files does not exist.  Check the value\n"
                             "of token shefdecode_log.\n" ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "The directory specified for the Shefdecoder\n"
                                "log files does not exist.  Check the value\n"
                                "of token shefdecode_log.\n" ) ;
             break ;

      case 6 :

             /* The directory specified for the error files does not exist. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to start the Shefdecoder failed.\n"
                             "The directory specified for the Shefdecoder\n"
                             "error files does not exist. Check the value\n"
                             "of token shef_error_dir.\n" ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "The directory specified for the Shefdecoder\n"
                                "error files does not exist. Check the value\n"
                                "of token shef_error_dir.\n" ) ;
             break ;

      case 7 :

             /* The directory specified for the executables does not exist. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to start the Shefdecoder failed.\n"
                             "The directory specified for the Shefdecoder\n"
                             "executable does not exist. Check the value\n"
                             "of token shefdecode_bin.\n" ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "The directory specified for the Shefdecoder\n"
                                "executable does not exist. Check the value\n"
                                "of token shefdecode_bin.\n" ) ;
             break ;

      case 8 :

             /* The Shefdecoder is already running. */
             XmTextSetString ( shefdecodeTX , 
                               "Shefdecoder is already running. Cannot start\n"
                               "another instance of it.\n" ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "An instance of Shefdecoder is already\n"
                                "running.  Cannot start another one.\n" ) ;
             break ;

      case 9 :
    
             /* The SHEFdecoder cannot be started from this computer.
                The value of the shefdecode_host token determines which
                system SHEFdecoder may be run on. */
             sprintf ( reply , "Cannot start SHEFdecoder on this system.\n"
                               "SHEFdecoder may only be started from the\n"
                               "%s system.\n" , shefdecode_hostname ) ;
             XmTextSetString ( shefdecodeTX , reply ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "Cannot start SHEFdecoder from this system.\n"
                                "It can only only be started from the %s\n"
                                "system.\n" , shefdecode_hostname ) ;
             break ;

      case 99 :

             XmTextSetString ( shefdecodeTX ,
                             "An error occurred while trying to fork/execlp\n"
                             "the start_shefdecode script.\n" ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "Could not fork/execlp the start_shefdecode\n"
                                "script.\n" ) ;
             break ;

      case 255 :

             XmTextSetString ( shefdecodeTX ,
                            "The call to ssh failed.  Check with your system\n"
                            "administrator to make sure that you have\n"
                            "created a public key for ssh to use.\n" ) ;
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "The %s command with options '%s' failed. A\n"
                                "possible cause is that the user has not\n"
                                "created a public RSA key and placed it in\n"
                                "~/.ssh/authorized_keys.\n" , ssh_command ,
                                options ) ;
             break ;


      default :         

             /* Unrecognized error code. */
             XmTextSetString ( shefdecodeTX ,
                             "An unrecognized error code has been returned\n"
                             "from 'start_shefdecode'.\n" ) ; 
             fprintf ( stderr , "In routine 'start_shefdecode':\n"
                                "An unrecognized error code %d has been\n"
                                "returned from 'start_shefdecode'.\n" ,
                                child_status ) ;
   }
}

static void stop_shefdecode ( Widget w, XtPointer clientdata ,
                              XtPointer calldata )
{
   static char * o_option = "-oBatchMode=yes" ;

   static char options [ BUFSIZ ] ;
   char reply [ BUFSIZ ]  ;
   static char * shefdecode_bin_token = "SHEFDECODE_BIN" ;
   static char * ssh_command = "ssh" ;
   int child_status ;
   int reply_len ;
   int request_len ;

   memset ( options , '\0' , BUFSIZ ) ;
   memset ( reply , '\0' , BUFSIZ ) ;

   request_len = strlen ( shefdecode_bin_token ) ;
   child_status = get_apps_defaults ( shefdecode_bin_token , 
                                      & request_len ,
                                      reply ,
                                      & reply_len ) ; 

   if ( child_status != 0 )
   {
      XmTextSetString ( shefdecodeTX ,
                      "The shefdecode_bin token is undefined.\n"
                      "Cannot find the stop_shefdecode script.\n" ) ;
      fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                         "The shefdecode_bin token is undefined.\n"
                         "Cannot find the stop_shefdecode script.\n" ) ;
      return ;
   }

   strcat ( reply , "/stop_shefdecode -i" ) ; 

   /* Create a new child process. */
   sprintf ( options , "-o BatchMode=yes %s \"%s -i\"" , 
             shefdecode_hostname , reply ) ;
   child_status = fork_process ( ssh_command , o_option ,
                                 shefdecode_hostname , reply ) ;

   switch ( child_status ) 
   {
      case 0 :
             
             /* The attempt to start the shefdecoder was successful. */
             XmTextSetString ( shefdecodeTX ,
                             "The Shefdecoder is being stopped.\n" 
                             "It may take several seconds before the\n"
                             "Shefdecoder completely shuts down.\n"
                             "Please press the 'Check' button to make sure\n"
                             "the Shefdecoder has completely shut down\n"
                             "before trying to restart it.\n" ) ; 
             break ;

      case 1 :

             /* An invalid argument was passed to this script. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to start the Shefdecoder failed.\n"
                             "An invalid argument was passed to the\n"
                             "stop_shefdecode script.\n" ) ;
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "An invalid argument '%s' was passed into\n"
                                "the stop_shefdecode script. Cannot stop\n"
                                "The Shefdecoder.\n" , reply ) ;
             break ;

      case 2 :

             /* The shefdecode_userid token is not defined. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to stop the Shefdecoder failed.\n"
                             "The shefdecode_userid token has not been\n"
                             "set.\n" ) ;  
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "The shefdecode_userid token has not been\n"
                                "defined.  Cannot stop the shefdecoder.\n" ) ;
             break ;

      case 3 :

             /* The user does not have permission to stop Shefdecoder. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to stop the Shefdecoder failed.\n"
                             "The user does not have permission to stop\n"
                             "Shefdecoder.\n" ) ; 
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "User does not have permission to\n"
                                "stop Shefdecoder.\n" ) ;
             break ;


      case 4 :
           
             /* The directory specified for the data files does not exist. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to stop the Shefdecoder failed.\n"
                             "The directory specified for the Shefdecoder\n"
                             "data files does not exist.  Check the value\n"
                             "of token shef_data_dir.\n" ) ;
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "The directory specified for the Shefdecoder\n"
                                "data files does not exist. Check the value\n"
                                "of token shef_data_dir.\n" ) ;
             break ;

      case 5 :
     
             /* The SHEFdecoder is not running.  There is no process to
                stop. */
             XmTextSetString ( shefdecodeTX ,
                             "Shefdecoder is not running.\n" ) ;
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "Cannot stop Shefdecoder - it is not\n"
                                "running\n" ) ;
             break ;
             
      case 6 :

             /* The directory specified for the log files does not exist. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to stop the SHEFdecoder failed.\n"
                             "The directory specified for the SHEFdecoder\n"
                             "log files does not exist.  Check the value\n"
                             "of token shef_log_dir.\n" ) ;
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "The directory specified for the SHEFdecoder\n"
                                "log files does not exist. Check the value\n"
                                "of token shef_log_dir.\n" ) ;
             break ;

      case 7 :

             /* The SHEFdecoder cannot be stopped from this computer.
                The value of the shefdecode_host token determines which
                system SHEFdecoder may be run on. */
             sprintf ( reply , "Cannot stop SHEFdecoder from this system.\n"
                               "SHEFdecoder may only be stopped from the\n"
                               "%s system.\n" , shefdecode_hostname ) ;
             XmTextSetString ( shefdecodeTX , reply ) ;
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "Cannot stop SHEFdecoder from this system.\n"
                                "It can only only be stopped from the %s\n"
                                "system.\n" , shefdecode_hostname ) ;
             break ;

      case 99 :

            XmTextSetString( shefdecodeTX ,
                             "An error occurred while trying to fork/execlp\n"
                             "the stop_shefdecode script.\n" ) ;
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "Could not fork/execlp the stop_shefdecode\n"
                                "script.\n" ) ;
             break ;


      case 255 :

             XmTextSetString ( shefdecodeTX ,
                            "The call to ssh failed.  Check with your system\n"
                            "administrator to make sure that you have\n"
                            "created a public key for ssh to use.\n" ) ;
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "The %s command with options '%s' failed. A\n"
                                "possible cause is that the user has not\n"
                                "created a public RSA key and placed it in\n"
                                "~/.ssh/authorized_keys.\n" , ssh_command ,
                                options ) ;
             break ;

      default :         

             /* Unrecognized error code. */
             XmTextSetString ( shefdecodeTX ,
                             "An unrecognized error code has been returned\n"
                             "from 'stop_shefdecode'.\n" ) ; 
             fprintf ( stderr , "In routine 'stop_shefdecode':\n"
                                "An unrecognized error code %d has been\n"
                                "returned from 'stop_shefdecode'.\n" ,
                                child_status ) ;
   }
}

static void check_shefdecode ( Widget w , XtPointer clientdata ,
                               XtPointer calldata )
{
   static char * o_option = "-oBatchMode=yes" ;

   char reply [ BUFSIZ ]  ;
   char message [ BUFSIZ ] ;
   char options [ BUFSIZ ] ;
   static char * shefdecode_bin_token = "SHEFDECODE_BIN" ;
   static char * shefdecode_log_token = "SHEFDECODE_LOG" ;
   char * shef_process_text = NULL ;
   static char * ssh_command = "ssh" ;
   FILE * pFile = NULL ;
   int child_status ;
   int reply_len ;
   int request_len ;
   struct stat file_stat ;

   request_len = strlen ( shefdecode_bin_token ) ;
   child_status = get_apps_defaults ( shefdecode_bin_token , 
                                      & request_len ,
                                      reply ,
                                      & reply_len ) ; 

   if ( child_status != 0 )
   {
      XmTextSetString ( shefdecodeTX ,
                      "The shefdecode_bin token is undefined.\n"
                      "Cannot find the check_shefdecode script.\n" ) ;
      fprintf ( stderr , "In routine 'check_shefdecode':\n"
                         "The shefdecode_bin token is undefined.\n"
                         "Cannot find the check_shefdecode script.\n" ) ;
      return ;
   }

   strcat ( reply , "/check_shefdecode" ) ; 

   /* Create a new child process. */
   sprintf ( options , "-o BatchMode=yes %s %s" , 
             shefdecode_hostname , reply ) ;
   child_status = fork_process ( ssh_command , o_option ,
                                 shefdecode_hostname , reply ) ;

   switch ( child_status ) 
   {
      case 0 :
      case 1 :

             request_len = strlen ( shefdecode_log_token ) ;              
             child_status = get_apps_defaults ( shefdecode_log_token ,
                                                & request_len ,
                                                reply ,
                                                & reply_len ) ; 
   
             if ( child_status != 0 )
             {
                XmTextSetString ( shefdecodeTX ,
                             "The directory specified for the Shefdecoder\n"
                             "log files does not exist.  Check the value\n"
                             "of token shefdecode_log.\n" ) ;
                fprintf ( stderr , "\nIn routine 'check_shefdecode':\n"
                                "The directory specified for the Shefdecoder\n"
                                "log files does not exist.  Check the value\n"
                                "of token shefdecode_log.\n" ) ;
             }
             else
             {
                strcat ( reply , "/shef_process_file" ) ;

                /* Retrieve the size of the file. */
                child_status = stat ( reply , & file_stat ) ; 

                if ( child_status != 0 )
                {
                   sprintf ( message , "Could not read the shef process file\n"
                                       "%s attributes.\n" , reply ) ;
                   XmTextSetString ( shefdecodeTX , message ) ; 
                   fprintf ( stderr , "\nIn routine 'check_shefdecode':\n"
                                      "%s" , message ) ;
                }
                else
                {
                
                   shef_process_text = ( char * ) malloc ( sizeof ( char ) *
                                                      file_stat.st_size + 1 ) ;
                   if ( shef_process_text == NULL )
                   {
                      fprintf ( stderr , "In routine 'check_shefdecode':\n"
                                         "Could not allocate %ld bytes of\n"
                                         "memory.\n" , 
                                         ( long ) file_stat.st_size + 1 ) ;
                      return ;
                   
                   }
    
                   pFile = fopen ( reply , "r" ) ;
          
                   if ( pFile == NULL )
                   {
                      sprintf ( message , "Could not open file %s." ,
                                reply ) ;
                      XmTextSetString ( shefdecodeTX , message ) ; 
                      fprintf ( stderr , "In routine 'check_shefdecode':\n"
                                         "%s\n" , message ) ;
                   }
                   else
                   { 
                      fread ( shef_process_text , sizeof( char ) ,
                              file_stat.st_size , pFile ) ;
                      fclose ( pFile ) ;
                      pFile = NULL ;
                      shef_process_text [ file_stat.st_size ] = '\0' ;

                      XmTextSetString ( shefdecodeTX ,
                                        shef_process_text ) ; 
                      free ( shef_process_text ) ;
                      shef_process_text = NULL ;

                   }
                }
             }

             break ;

      case 2 :

             /* The directory specified for the log files does not exist. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to check the Shefdecoder failed.\n"
                             "The directory specified for the Shefdecoder\n"
                             "log files does not exist.  Check the value\n"
                             "of token shefdecode_log.\n" ) ;
             fprintf ( stderr , "In routine 'check_shefdecode':\n"
                                "The directory specified for the Shefdecoder\n"
                                "log files does not exist.  Check the value\n"
                                "of token shefdecode_log.\n" ) ;
             break ;

      case 3 :
           
             /* The directory specified for the data files does not exist. */
             XmTextSetString ( shefdecodeTX ,
                             "The attempt to check the Shefdecoder failed.\n"
                             "The directory specified for the Shefdecoder\n"
                             "data files does not exist.  Check the value\n"
                             "of token shef_data_dir.\n" ) ;
             fprintf ( stderr , "In routine 'check_shefdecode':\n"
                                "The directory specified for the Shefdecoder\n"
                                "data files does not exist. Check the value\n"
                                "of token shef_data_dir.\n" ) ;
             break ;

      case 4 :

             /* The directory specified for the log files does not exist. */
             XmTextSetString ( shefdecodeTX ,
                               "The current state of the Shefdecoder\n"
                               "cannot be determined.\n" ) ;
             fprintf ( stderr , "In routine 'check_shefdecode':\n"
                                "Cannot determine the current state\n"
                                "of the Shefdecoder.\n" ) ;
             break ;

      case 99 :

             XmTextSetString ( shefdecodeTX ,
                               "An error occurred while trying to fork/execlp\n"
                               "the check_shefdecode script.\n" ) ;
             fprintf ( stderr , "In routine 'check_shefdecode':\n"
                                "Could not fork/execlp the check_shefdecode\n"
                                "script.\n" ) ;
             break ;

      case 255 :

             XmTextSetString ( shefdecodeTX ,
                            "The call to ssh failed.  Check with your system\n"
                            "administrator to make sure that you have\n"
                            "created a public key for ssh to use.\n" ) ;
             fprintf ( stderr , "In routine 'check_shefdecode':\n"
                                "The %s command with options '%s' failed. A\n"
                                "possible cause is that the user has not\n"
                                "created a public RSA key and placed it in\n"
                                "~/.ssh/authorized_keys.\n" , ssh_command ,
                                options ) ;
             break ;


      default :         

             /* Unrecognized error code. */
             XmTextSetString ( shefdecodeTX ,
                             "An unrecognized error code has been returned\n"
                             "from 'check_shefdecode'.\n" ) ;
             fprintf ( stderr , "In routine 'check_shefdecode':\n"
                                "An unrecognized error code %d has been\n"
                                "returned from 'check_shefdecode'.\n" ,
                                child_status ) ;
   }
}

static void close_shefdecode ( Widget w , XtPointer clientdata ,
                               XtPointer calldata )
{
   if ( XtIsManaged ( shefdecodeDS ) )
   {
      XtDestroyWidget ( shefdecodeDS ) ;
      shefdecodeDS = NULL ;
   }
}

void ShowShefdecoderDS ( Widget w )
{
   Atom wmAtom ;
   static Boolean first = True ;
   char reply [ SHEFDECODE_HOSTNAME_LEN ] ; 
   static char * shefdecode_host_token_name = "shefdecode_host" ;
   int reply_len ;
   int request_len ;
   int status ;

   if ( first == True )
   {
      /* Get the value of the shefdecode_host token. */ 
      request_len = strlen ( shefdecode_host_token_name ) ;
      status = get_apps_defaults ( shefdecode_host_token_name , & request_len ,
                                   reply , & reply_len ) ;

      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine ShowShefdecodeDS:\n"
                   "Could not retrieve the value of the shefdecode_host\n"
                   "token.  A default value of 'ds' is being used.\n" ) ;
         strcpy ( shefdecode_hostname , "dx1f" ) ;
      }
      else
      {
         strcpy ( shefdecode_hostname , reply ) ;
      }
                     
      first = False ;
   }
 
   if ( shefdecodeDS == NULL )
   {
      create_shefdecodeDS ( GetTopShell ( w ) ) ;
      wmAtom = XmInternAtom ( XtDisplay ( shefdecodeDS ) , 
                              "WM_DELETE_WINDOW" , False ) ; 
      XmAddWMProtocolCallback ( shefdecodeDS , wmAtom ,
                                close_shefdecode , NULL ) ;
      XtAddCallback ( startshefPB , XmNactivateCallback ,
                      start_shefdecode , NULL ) ;  
      XtAddCallback ( stopshefPB , XmNactivateCallback ,
                      stop_shefdecode , NULL ) ;
      XtAddCallback ( checkshefPB , XmNactivateCallback ,
                      check_shefdecode , NULL ) ;
      XtAddCallback ( closeshefPB , XmNactivateCallback , 
                      close_shefdecode , NULL ) ;
      XmTextSetString ( shefhostTX , shefdecode_hostname ) ;
   }

   if ( ! XtIsManaged ( shefdecodeDS ) )
   {
      XtManageChild ( shefdecodeFO ) ;
      XtManageChild ( shefdecodeDS ) ;
   }

   return ;
}
