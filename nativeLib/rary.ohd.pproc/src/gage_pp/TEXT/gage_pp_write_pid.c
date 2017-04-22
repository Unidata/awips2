/******************************************************************************* 
* FILENAME:            gage_pp_write_pid.c
* NUMBER OF MODULES:   1 
* GENERAL INFORMATION:
*   MODULE 1:          gage_pp_write_pid 
* DESCRIPTION:         Writes the process id of the Gage Precip Processor 
*                      application to a file. 
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        9/17/2004    Bryon Lawrence    Added the include
*                                                  file "gage_pp_write_rec.h". 
*                                                  This is for the GPP_OK and
*                                                  GPP_ERROR constants.
********************************************************************************
*/

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gage_pp_log.h"
#include "gage_pp_main.h"
#include "gage_pp_write_pid.h"
#include "gage_pp_write_rec.h"

int gage_pp_write_pid ( const char * data_dir )
{
   char   *     full_pidname = NULL ;
   char         msgstr[MAX_LOG_LENGTH] ;
   const static char * pidfilename = "gage_pp_pid.dat" ;
   FILE         * pidfilePtr = NULL ;
   int          malloc_length ;
   long int     gage_pp_pid ;

   /* get the process id */
   gage_pp_pid = getpid ( ) ;

   /* write it to the gage_pp logs */
   sprintf (msgstr, "Process id of this gage pp run = %ld\n",
             gage_pp_pid) ;
   writelog ( msgstr ) ;

   /* write it to a special file in the data directory */
   malloc_length = ( strlen ( data_dir ) + 1 + strlen ( pidfilename ) + 1 )
	           * sizeof ( char ) ;
   full_pidname = ( char * ) malloc ( malloc_length ) ; 

   if ( full_pidname == NULL )
   {
      sprintf ( msgstr , "\nIn routine 'gage_pp_write_pid':\n" 
                          "Could not allocate %d bytes of memory for\n"
                          "full_pidname\n" , malloc_length ) ;  
      writelog ( msgstr ) ;
      return GPP_ERROR ;
   }

   memset ( full_pidname , '\0' , malloc_length ) ;
   sprintf(full_pidname, "%s/%s", data_dir, pidfilename);
   pidfilePtr = fopen(full_pidname, "w");

   if ( pidfilePtr == NULL )
   {
      sprintf ( msgstr , "Error opening pid file: %s" , full_pidname ) ;
      writelog ( msgstr ) ;
      return GPP_ERROR ;
   }

   else
   {
      fprintf ( pidfilePtr , "%ld" , gage_pp_pid ) ;
      fclose ( pidfilePtr ) ;
      sprintf ( msgstr , "PID written to file: %s" , full_pidname ) ;
   } 

   if ( full_pidname != NULL )
   {
      free ( full_pidname ) ;
      full_pidname = NULL ;
   }

   return GPP_OK;
}
