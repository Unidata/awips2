/********************************************************************************
* FILENAME:             gage_pp_log.c
* NUMBER OF MODULES:         3
* GENERAL INFORMATION:
*   MODULE 1:           openlog 
* DESCRIPTION:          This is subroutine to close old log file and open new 
*                       log file Gage Precip Processor.
*   MODULE 2:           closelog
* DESCRIPTION:          Closes the log file opened by the call to the 
*                       openlog routine.
*   MODULE 3:           writelog 
* DESCRIPTION:          Appends the message to the log file opened by calling
*                       the openlog routine.
*
*                       Calling subroutine: gage_pp_main program.
*
* ORIGINAL AUTHOR:      Moria Shebsovich
* CREATION DATE:        July 8, 2004
* ORGANIZATION:         HSEB / OHD
* MACHINE:              HP9000 / Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        7/22/2004    Moria Shebsovich  Original Coding
*          1        7/29/2004    Bryon Lawrence    Modified to reduce passing
*                                                  of logfile FILE pointer.
*          1        9/17/2004    Bryon Lawrence    Added include of
*                                                  gage_pp_write_rec.h 
*                                                  header file.
*          2        7/29/2004    Bryon Lawrence    Original Coding
*          3        7/29/2004    Bryon Lawrence    Original Coding
*********************************************************************************/

#include <stdio.h>
#include <string.h>
#include "time_convert.h"
#include "gage_pp_main.h"
#include "gage_pp_log.h"
#include "gage_pp_write_rec.h"

/*******************************************************************************
* MODULE NUMBER: 1
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
#define LENGTH_OF_DATE_STRING 5

static FILE * logFilePtr = NULL ;
static char * log_filename = NULL ;

int openlog ( int * idat , const char * log_out , int * logday )
{
    char datestr [ LENGTH_OF_DATE_STRING ] ;
    int length ;
    int status ;

    if ( idat == NULL || log_out == NULL || logday == NULL )
    {
       return GPP_ERROR ;
    }

    length = strlen ( log_out ) ; 

    /* Make sure the user did not supply empty string for log_out. */
    if ( length == 0 )
    {
       return GPP_ERROR ;
    }

    /* Check if the log file is open.  If it is open then close it. */
    status = closelog ( ) ;

    if ( status == GPP_ERROR )
    {
        return GPP_ERROR ;
    }

    /* Define a new file name */
    datimls ( idat ) ;

    * logday  = idat [ 3 ] ;
    sprintf ( datestr , "%02d%02d" , idat [2] , idat [3] ) ; 

    log_filename = ( char * ) malloc ( ( length + LENGTH_OF_DATE_STRING + 1 )
                                       * sizeof ( char ) ) ; 
    
    if ( log_filename == NULL )
    {
       return GPP_ERROR ;
    }

    strcpy ( log_filename , log_out ) ;
    strcat ( log_filename , datestr ) ;
    
    /* Open the log file so that new log information is appended to it. */
    logFilePtr = fopen ( log_filename , "a" ) ;

    if ( logFilePtr == NULL )
    {
        fprintf(stderr , "\nIn routine 'openlog':\n"
                         "Error opening log file: %s\n", log_filename ) ;
	return GPP_ERROR ;
    }
    
    return GPP_OK  ;
}

/*******************************************************************************
* MODULE NUMBER:  2
* MODULE NAME:    closelog ( )
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
int closelog ( )
{
   int status ;

   if ( logFilePtr != NULL )
   {
       /* Close the log file. */
       status = fclose ( logFilePtr ) ;

       if ( status != 0 )
       {
          fprintf ( stderr , 
              "\nIn routine \"openlog\": The log file cannot be closed.\n" ) ;
          return GPP_ERROR ;
       }

       logFilePtr = NULL ;

       if ( log_filename != NULL )
       {
          free ( log_filename ) ;
          log_filename = NULL ;
       }
        
    }

    return GPP_OK ;
}

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
int writelog ( const char * log_message )
{
    if ( logFilePtr == NULL )
    {
       fprintf ( stderr , "\nIn routine 'write_log':\n"
                          "The log file is not open.\n" ) ;
       return GPP_ERROR ;
    } 

    if ( ( log_message != NULL ) && ( * log_message != '\0' ) )
    {
       fprintf ( logFilePtr , "%s\n" , log_message ) ;
       fflush ( logFilePtr ) ;
    }

    return GPP_OK ;
}
