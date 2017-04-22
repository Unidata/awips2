/****************************************************************************
* FILENAME:       gage_pp_check_stop.c
* MODULE 1:       gage_pp_check_stop 
* DESCRIPTION:    This routine checks whether a stop file exists,
*                 if it does the program is aborted.
*                 
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        9/17/2004    Bryon Lawrence    Added the include of the
*                                                  gage_pp_write_rec.h 
*                                                  header file for the
*                                                  GPP_STOP and GPP_OK
*                                                  definitions.
******************************************************************************/

#include <stdio.h>
#include <string.h>

#include "gage_pp_check_stop.h"
#include "gage_pp_log.h"
#include "gage_pp_main.h"
#include "gage_pp_write_rec.h"
	
int gage_pp_check_stop ( const char * stopfile )
{
    static char message [ MAX_LOG_LENGTH ] ;
    FILE * pFile = NULL ;

    /* Check if stop file exists */
    pFile = fopen ( stopfile , "r" ) ;
 
    /* If file exists, write log messages and close database 
       before stopping */
    if ( pFile != NULL )
    {
        sprintf ( message , "\nStopfile %s found\n...shutting down\n" , 
			     stopfile ) ; 
	writelog ( message ) ;
        return GPP_STOP ;
    }

    return GPP_OK ; 
}
