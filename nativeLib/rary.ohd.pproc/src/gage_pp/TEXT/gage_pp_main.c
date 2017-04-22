/*******************************************************************************
* FILENAME:             main_gage_pp.c
* NUMBER OF MODULES:         1
* GENERAL INFORMATION:
*   MODULE 1:           main
* DESCRIPTION:          This is the main routine for the Gage Precip Processor
*                       application.
*
* ORIGINAL AUTHOR:      Moria Shebsovich
* CREATION DATE:        July 6, 2004
* ORGANIZATION:         HSEB / OHD
* MACHINE:              HP9000 / Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE           PROGRAMMER        DESCRIPTION/REASON
*          1        7/6/2004       Moria S.          Original Coding
*          1        July/Aug/2004  Bryon Lawrence    Recoded and Fixed. Updated
*                                                    documentation.
*          1        9/17/2004      Bryon Lawrence    Added the include of the
*                                                    gage_pp_write_rec.h
*                                                    header file for
*                                                    definition of
*                                                    GagePPoptions structure.
*********************************************************************************/
#include <stdio.h>
#include <string.h>
#include "DbmsAccess.h"
#include "GeneralUtil.h"
#include "gage_pp_check_stop.h"
#include "gage_pp_log.h"
#include "gage_pp_main.h"
#include "gage_pp_write_pid.h"
#include "gage_pp_write_rec.h"
#include "get_precip_settings.h"
#include "process_gage_pp.h"
#include "sqlca.h"
#include "time_convert.h"

#define LEN_REPLY 1024

/********************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   main_gage_pp
*
* PURPOSE:       This is the main routine for the Gage Precip Processor
*                application.
*
* THE FOLLOWING FILES ARE USED:
*   THE FOLLOWING APPS_DEFAULTS USED:
*   gage_pp_userid   - controls UNIX user
*   gage_pp_host     - controls UNIX system
*   gage_pp_data     - data input files location
*   gage_pp_log      - daily log files location
*   gage_pp_sleep    - duration in seconds to wait between queries for input
*                      data file
*   shef_duplicate   - flag for defining how to handle shef revision
*                      flags: ALWAYS_OVERWRITE, USE_REVCODE, IF_DIFFERENT,
*                      IF_DIFFERENT_OR_REVCODE
*
*********************************************************************************/

/* Macro for deallocating memory when GagePP shutsdown. */
#define FREE_GPP_MEMORY    if ( data_dir != NULL )   \
                           {                         \
                              free ( data_dir ) ;    \
                              data_dir = NULL ;      \
                           }                         \
                                                     \
                           if ( log_out != NULL )    \
                           {                         \
                              free ( log_out ) ;     \
                              log_out = NULL ;       \
                           }                         \
                                                     \
                           if ( stopfile != NULL )   \
                           {                         \
                              free ( stopfile ) ;    \
                              stopfile = NULL ;      \
                           }                         \

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   main
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME         DESCRIPTION/UNITS
*   input  int         argc         The number of command line arguments
*                                   passed into this routine.
*   input  char *      argv [ ]     The command line arguments.
*
*   Note that in OB5 GagePP does not expect any command line arguments.
*
* RETURNS:
*   DATA TYPE           DESCRIPTION
*   int                 O means that GagePP executed with out detectable
*                         exception
*                       1 means that GagePP shut down due to an error.
*
* APIs UTILIZED:
*   NAME                HEADER FILE           DESCRIPTION
*   CloseDbms           DbmsAccess.h          Closes connection to database.
*   closelog            gage_pp_log.h         Closes gage_pp log file.
*   gage_pp_check_stop  gage_pp_check_stop.h  Checks for existence of stop
*                                             file.
*   get_apps_defaults   GeneralUtil.h         Retrieves the value of a
*                                             apps defaults token.
*   get_precip_window   get_precip_settings.h Returns the values of the
*                                             intpc, intlppp and intuppp
*                                             tokens.
*   get_6hour_precip_window get_precip_settings.h Returns the value of the
*                                                 intppq token.
*   OpenDbms            DbmsAccess.h          Opens the database.
*   openlog             gage_pp_log.h         Opens the gage_pp log.
*   process_gage_pp     process_gage_pp.h     Processes top of hour PC and PP
*                                             files produced by ShefDecoder.
*   writelog            gage_pp_log.h         Writes to the gage_pp log.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                    DESCRIPTION
*   char *     db_name_token           The name of the token containing the
*                                      database name.
*   char *     gage_pp_data_token      The name of the token containing the
*                                      gage_pp data directory.
*   char *     gage_pp_logsuffix       Contains the
*   char *     gage_pp_log_token       The name of the token containing
*                                      the gage_pp log directory.
*   char *     gage_pp_sleep_token     The name of the token containing the
*                                      duration, in seconds, the gage_pp
*                                      process should sleep between checking
*                                      the data directory for PP and PC data
*                                      files.
*   char *     stopfilename            The name of the file (in the data
*                                      directory) which gage_pp should look
*                                      for in order to determine if the
*                                      user wants it to shut down.
*   char *     data_dir                The directory containing the PP/PC
*                                      data files which gage_pp processes.
*   char *     log_out                 The path of the log file.
*   char *     stopfile                The path and name of the stopfile.
*   char       always_overwrite_reply [ ]  The retrieved value of the
*                                          vl_always_overwrite_flag token.
*   char       data_reply [ ]          The retrieved value for the
*                                      gage_pp_data token.
*   char       db_reply [ ]            The retrieved value for the
*                                      db_name token.
*   char       log_reply[ ]            The retrieved value for the
*                                      gage_pp_log token.
*   char       message [ ]             Log messages are written to this
*                                      array and then passed to the
*                                      writelog ( ) routine.
*   char       sleep_command [ ]       The system command to sleep for the
*                                      duration specified by gage_pp_sleep
*                                      token is constructed in this array.
*   char       sleep_reply [ ]         The retrieved value for the
*                                      gage_pp_sleep token.
*   GagePPoptions gage_pp_options      Contains the options (based on
*                                      token settings) which control the
*                                      operation of GagePP.
*   int        data_request_len        The length of the gage_pp_data token
*                                      name.
*   int        data_reply_len          The length of the value of the
*                                      gage_pp_data token.
*   int        db_request_len          The length of the db_name token name.
*   int        db_reply_len            The length of the value of the
*                                      db_name token.
*   int        log_request_len         The length of the gage_pp_log token
*                                      name.
*   int        log_reply_len           The length of the value of the
*                                      gage_pp_log token value.
*   int        lensleep                The length of the gage_pp_sleep token
*                                      name.
*   int        malloc_length           The number of bytes to allocate
*                                      for a given call to malloc.
*   int        sleep_reply_len         The length of the value of the
*                                      gage_pp_sleep token.
*   int        status                  Contains return codes from
*                                      function calls.
*   int        idam [ ]                Contains date and time information.
*                                      idam [ 0 ] = Year, idam [ 1 ] = Month,
*                                      idam [ 2 ] = Day.
*   int        logday                  Corresponds to idam [ 3 ] .
*   int        sleepdur                The number of seconds to sleep
*                                      between checks for PC/PP data files.
*                                      Base on gage_pp_sleep token value.
*                                      Minimum value of 10 seconds.
*
* DATA FILES AND/OR DATABASE:
*
*   gage_pp_pid.dat - contains the process id of the currently
*                     executing gage_pp
*   log file    - general log file, one per day
*   data file   - contains top of the hour PC and PP precipitation
*                 reports from ShefDecoder Poster, deleted after processing
*                 posting to the HourlyPP and HourlyPC tables.
*   files.list  - contains list of files currently in ../data/gpp_input
*                 directory.  This is used to distinguish the data files from
*                 from the pid and stop files.
*   stop file   - The file whose very existence forces the gage_pp  program
*                 to shut down.  The stop file is created by the stop_gage_pp
*                 script.
*
* ERROR HANDLING:
*    ERROR CODE           DESCRIPTION
*             0           GagePP ran ok.
*             1           GagePP shutdown due to an error.
********************************************************************************
*/
int gage_pp_main ( int argc , const char ** argv)
{
    static char * db_name_token = "db_name" ;
    static char * gage_pp_data_token = "gage_pp_data" ;
    static char * gage_pp_logsuffix = "gage_pp_log_" ;
    static char * gage_pp_log_token = "gage_pp_log" ;
    static char * gage_pp_sleep_token = "gage_pp_sleep" ;
    static char * stopfilename = "stop_gage_pp" ;

    char * data_dir = NULL ;
    char * log_out = NULL ;
    char * stopfile = NULL ;

    char data_reply [ LEN_REPLY + 1 ] ;
    char db_reply [ LEN_REPLY + 1 ] ;
    char log_reply [ LEN_REPLY + 1 ] ;
    char message [ MAX_LOG_LENGTH ] ;
    char sleep_command [ 20 ] ; ;
    char sleep_reply [ LEN_REPLY + 1 ] ;

    GagePPoptions gage_pp_options ;

    int data_request_len ;
    int data_reply_len ;
    int db_request_len ;
    int db_reply_len ;
    int log_request_len ;
    int log_reply_len ;
    int lensleep ;
    int malloc_length ;
    int sleep_reply_len ;
    int status ;
    int idam [7] ;
    int logday = 0 ;
    int sleepdur ;

    /* Get log files directory token */
    log_request_len = strlen ( gage_pp_log_token ) ;
    status = get_apps_defaults ( gage_pp_log_token , & log_request_len ,
                                log_reply , & log_reply_len ) ;
    if ( status != 0 )
    {
        fprintf ( stderr , "\nIn routine 'main':\n"
                        "Could not retrieve the path of the gage_pp data\n"
                        "log directory from the environment\n"
                        "or apps defaults token file.\n" ) ;
        return 0 ;
    }

    /* Build part of the logfile path and name. */
    malloc_length = ( log_reply_len + 1 + strlen ( gage_pp_logsuffix ) + 1 )
	            * sizeof ( char ) ;
    log_out = ( char * ) malloc ( malloc_length ) ;

    if ( log_out == NULL )
    {
       fprintf ( stderr , "\nIn routine 'main':\n"
                          "Could not allocate %d bytes of memory for\n"
                          "the log path.\n" , malloc_length ) ;
       FREE_GPP_MEMORY ;
       return 0 ;
    }

    memset ( log_out , '\0' , malloc_length ) ;
    strcpy ( log_out , log_reply ) ;
    strcat ( log_out , "/" ) ;
    strcat ( log_out , gage_pp_logsuffix ) ;

    status = openlog ( idam , log_out , & logday ) ;

    sprintf( message, "\n%s %s - %s\n", gagepp_name, gagepp_ver, gagepp_date );
    writelog( message );

    if ( status != GPP_OK )
    {
       fprintf ( stderr , "\nIn routine 'main':\n"
                          "Could not open the log file. Shutting down.\n" ) ;
       FREE_GPP_MEMORY ;
       return 1 ;
    }

    /* Get all the application default values.  Log the values of these
       tokens into the log file. */

    /* Get the name of the database to open. */
    db_request_len = strlen ( db_name_token ) ;
    status = get_apps_defaults ( db_name_token , & db_request_len ,
                                 db_reply , & db_reply_len ) ;

    if ( status != 0 )
    {
       sprintf ( message , "Could not retrieve database name from "
                           "token %s. Shutting down." , db_name_token ) ;
       writelog ( message ) ;
       FREE_GPP_MEMORY ;
       closelog ( ) ;
       return 1 ;

    }

    sprintf ( message , "db_name: %s" , db_reply ) ;
    writelog ( message ) ;

    /* Get gage_pp local data directory token */
    data_request_len = strlen ( gage_pp_data_token ) ;
    status = get_apps_defaults ( gage_pp_data_token , & data_request_len ,
                                data_reply , & data_reply_len ) ;

    if ( status != 0 )
    {
       sprintf ( message , "Could not retrieve gage_pp data path "
                           "from token %s. Shutting down." ,
                           gage_pp_data_token ) ;
       writelog ( message ) ;
       FREE_GPP_MEMORY ;
       closelog ( ) ;
       return 1 ;
    }

    lensleep = strlen ( gage_pp_sleep_token ) ;
    status = get_apps_defaults ( gage_pp_sleep_token , & lensleep ,
                                sleep_reply , & sleep_reply_len ) ;

    if ( status != 0 )
    {
        sprintf ( message , "Could not retrieve gage_pp_sleep token value. "
                            "Shutting down." ) ;
        writelog ( message ) ;
        FREE_GPP_MEMORY ;
        closelog ( ) ;
        return 1 ;
    }

    sprintf ( message , "gage_pp_sleep: %s secs" , sleep_reply ) ;
    writelog ( message ) ;

    /* Check for the sleep duration.   This is the number of seconds
       the gauge precipitation processor will sleep between checks for
       precipitation files to process. */
    if ( sleep_reply_len != 0 )
    {
         sleepdur = atoi ( sleep_reply ) ;

         if ( sleepdur < MINIMUM_GPP_SLEEP_DURATION )
         {
            sleepdur = MINIMUM_GPP_SLEEP_DURATION ;
         }
    }
    else
    {
        sleepdur = MINIMUM_GPP_SLEEP_DURATION ;
    }

    /* Determine how to handle duplicate reports. Should a duplicate
       report always overwrite data in HourlyPC and HourlyPP tables or
       should it only overwrite if it is a revision?. */
    get_and_translate_duplicate_token ( & gage_pp_options.shef_duplicate );

    sprintf ( message , "shef_duplicate : " );

    switch ( gage_pp_options.shef_duplicate )
    {
       case ALWAYS_OVERWRITE :

          strcat ( message , "ALWAYS_OVERWRITE (always process duplicate "
                             "reports)" ) ;
          break;

       case USE_REVCODE :

          strcat ( message , "USE_REVCODE (only process duplicate reports "
                             "if they are revisions)" ) ;
          break;

       case IF_DIFFERENT :
          strcat ( message , "IF_DIFFERENT (only process duplicate reports "
                             "if their values are different)" ) ;
          break;

       case IF_DIFFERENT_OR_REVCODE :
          strcat ( message , "IF_DIFFERENT_OR_REVCODE (only process duplicate "
                             "reports if they are revisions or their values\n"
                             "are different)" ) ;
          break;

       default :
          break;
    }

    writelog ( message ) ;

    /* Retrieve the tokens which define the precipitation windows for PCI,
     * PPH and PPQ data. */
    get_precip_window ( & gage_pp_options.intpc,
                        & gage_pp_options.intlppp,
                        & gage_pp_options.intuppp );

    get_6hour_precip_window ( & gage_pp_options.intppq );

    /* Output this information to the log. */
    sprintf ( message, "intpc:   %d  minutes", gage_pp_options.intpc );
    writelog ( message );

    sprintf ( message, "intlppp: %d  minutes", gage_pp_options.intlppp );
    writelog ( message );

    sprintf ( message, "intuppp: %d  minutes", gage_pp_options.intuppp );
    writelog ( message );

    sprintf ( message, "intppq:  %3.1f hours", gage_pp_options.intppq );
    writelog ( message );

    /* Create the sleep command which will be passed to the system. */
    sprintf ( sleep_command , "sleep %d" , sleepdur ) ;

    /* Build the data directory path. */
    malloc_length = ( strlen ( data_reply ) + 1 ) * sizeof ( char ) ;

    data_dir = ( char * ) malloc ( malloc_length ) ;

    if ( data_dir == NULL )
    {
       sprintf ( message , "Could not allocate %d bytes of memory for\n"
                           "the datadir path. Shutting down." ,
                           malloc_length ) ;
       writelog ( message ) ;
       FREE_GPP_MEMORY ;
       closelog ( ) ;
       return 1 ;
    }

    memset ( data_dir , '\0' , malloc_length ) ;
    strcpy ( data_dir , data_reply ) ;

    sprintf ( message , "data dir: %s" , data_dir ) ;
    writelog ( message ) ;

    /* Create the path and name of the stopfile. */
    malloc_length =  ( strlen ( data_dir ) + 1 + strlen ( stopfilename ) + 1 )
                     * sizeof ( char ) ;
    stopfile = ( char * ) malloc ( malloc_length ) ;

    if ( stopfile == NULL )
    {
       sprintf ( message , "Could not allocate %d bytes of memory for\n"
                           "the stopfile path and name. Shutting down." ,
                           malloc_length ) ;
       writelog ( message ) ;
       FREE_GPP_MEMORY ;
       closelog ( ) ;
       return 1 ;
    }

    memset ( stopfile , '\0' , malloc_length ) ;
    sprintf ( stopfile , "%s/%s" , data_dir , stopfilename ) ;

    sprintf ( message , "stopfile name: %s" , stopfile ) ;
    writelog ( message ) ;

    status = gage_pp_check_stop ( stopfile ) ;

    if ( status == GPP_STOP )
    {
        FREE_GPP_MEMORY ;
        closelog ( ) ;
        return 0 ;
    }

    /* Open the database. */
    status = OpenDbms ( db_reply ) ;

    if ( status != Ok )
    {
       sprintf ( message , "Error opening database %s.\n" , db_reply ) ;
       writelog ( message ) ;
       closelog ( ) ;
       return 1 ;
    }

    /* Write out the process id of this execution for
       possible use by the stop script */
    status = gage_pp_write_pid ( data_dir ) ;

    if ( status != GPP_OK )
    {
       closelog ( ) ;
       CloseDbms ( ) ;
       FREE_GPP_MEMORY ;
       return 1 ;
    }

    /*** begin main loop ********************************************/

    for ( ; ; )  /* To infinite and beyond.*/
    {
       /* Process any files that are in the input data directory */
       status = process_gage_pp ( data_dir ,
                                  stopfile ,
		                          & gage_pp_options ) ;

       if ( status != GPP_OK )
       {
           CloseDbms ( ) ;
           closelog ( ) ;
           FREE_GPP_MEMORY ;
           return 1 ;
       }

       /* Check for a stop file after each of files are processed */
       status = gage_pp_check_stop ( stopfile ) ;

       if ( status == GPP_STOP )
       {
           CloseDbms ( ) ;
           closelog ( ) ;
           FREE_GPP_MEMORY ;
           return 0 ;
       }

       /* Wait a little bit */
       system ( sleep_command ) ;

       /* If date has changed, then open a new log file.
       The check for a new date is performed after each file is read */
       datimls ( idam ) ;
       if ( idam [ 3 ] != logday )
       {
          closelog ( ) ;
          openlog ( idam , log_out , & logday ) ;
       }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
