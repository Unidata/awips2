/********************************************************************************
* FILENAME:             gage_pp_check_input.c
* NUMBER OF MODULES:         1
* GENERAL INFORMATION:
*   MODULE 1:           gage_pp_check_input 
* DESCRIPTION:          This routine searches a specified directory for all 
*                       regular files and loads a list of filenames, up to
*                       a specified maximum, that are in the directory.
*
*                       The number of files read and a list of the filenames 
*                       are returned by this function.
* 
*                       Calling subroutine: process_gage_pp
* 
* ORIGINAL AUTHOR:      Moria Shebsovich
* CREATION DATE:        July 20, 2004
* ORGANIZATION:         HSEB / OHD
* MACHINE:              HP9000 / Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        9/17/2004    Bryon Lawrence    Added the include
*                                                  of gage_pp_write_rec.h
*                                                  for the definitions of
*                                                  constants GPP_ERROR
*                                                  and GPP_OK.
*          1        6/6/2005     Bryon Lawrence    Modified to check for
*                                                  workfile extension
*                                                  specified in 
*                                                  GPP_WORKFILE_SUFFIX.
*********************************************************************************/
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "gage_pp_check_input.h"
#include "gage_pp_log.h"
#include "gage_pp_main.h"
#include "gage_pp_write_rec.h"
#include "get_precip_settings.h"

extern int errno ;

int gage_pp_check_input ( const char * data_dir , 
                          const char * stopfilename ,
                          char ** infiles ,
                          int MAXFILES ,
                          int * numfound )
{
   char * command = NULL;
   char filename [ BUFSIZ ];
   char * full_filename = NULL;
   const static char * listfile = "files.list";
   char msgstr [ MAX_LOG_LENGTH ];
   char * pExtension = NULL;
   const static char * pidfile = "gage_pp_pid.dat";
   char * pRead = NULL;
   const static char * workfile_extension = GPP_WORKFILE_SUFFIX;
   FILE *infilePtr = NULL;
   int cnt , ret , slen;
   int load_flag;
   int malloc_length;
   int status;
   int is_workfile;
   struct stat filestat;

   cnt = 0 ;
   * numfound = 0 ;
 
   /* Create a list of files in the current directory and
      write the output to a text file */
   malloc_length = ( strlen ( data_dir ) + 1 + strlen ( listfile ) + 1 ) *
	           sizeof ( char ) ;

   full_filename = ( char * ) malloc ( malloc_length ) ;

   if ( full_filename == NULL )
   {
      sprintf ( msgstr , "In routine 'gage_pp_check_input':\n"
                         "Could not allocate %d bytes for full_filename." ,
                          malloc_length ) ; 
      writelog ( msgstr ) ;
      return GPP_ERROR ;
   }

   memset ( full_filename , '\0' , malloc_length ) ;
   sprintf ( full_filename , "%s/%s" , data_dir , listfile ) ; 

   malloc_length = ( strlen ( "ls -rt1  2>> /dev/null >  " ) +
                     strlen ( data_dir ) + strlen ( full_filename ) + 1 ) 
                    * sizeof ( char ) ;

   command = ( char * ) malloc ( malloc_length ) ;

   if ( command == NULL )
   {
      sprintf ( msgstr , "In routine 'gage_pp_check_input':\n"
                         "Could not allocate %d bytes for command." ,
                         malloc_length ) ;
      writelog ( msgstr ) ;
      free ( full_filename ) ;
      return GPP_ERROR ;
   }

   sprintf ( command , "ls -rt1 %s 2>> /dev/null > %s " ,
           data_dir , full_filename ) ;
   system ( command ) ;
   free ( command ) ;
   command = NULL ;

   /* Open the listfile for the list of filenames */
   infilePtr = fopen ( full_filename , "r" ) ;

   if ( infilePtr == NULL )
   {
      sprintf ( msgstr , "Error opening list file: %s" , full_filename ) ;
      writelog ( msgstr ) ;
      free ( full_filename ) ;
      full_filename = NULL ;
      return GPP_ERROR ;
   }

   free ( full_filename ) ;
   full_filename = NULL ;

   /* Read the filenames from the list and load the names into
      the list; limit each loading of list to MAXFILES */

   pRead = fgets ( filename , BUFSIZ , infilePtr ) ;
   while ( ( ! feof ( infilePtr ) ) && ( cnt < MAXFILES ) )
   {

      /* Null out the newline character that is stored
         at the end of the read string */
      slen = strlen ( filename ) ;
      filename [ slen - 1 ] = '\0' ; 

      /* Don't include certain files */
      /* Test for a work extension.  Files with a work extension
         should not be processed by GagePP. */
      is_workfile = 0;
      pExtension = strrchr ( filename, '.' );

      if ( pExtension != NULL )
      {
         pExtension ++;
         status = strcmp ( pExtension, workfile_extension );

         if ( status == 0 )
         {
            is_workfile = 1;
         }
      }

      if ( strcmp ( filename , stopfilename ) == 0 ||
           strcmp ( filename , pidfile )      == 0 ||
           strcmp ( filename , listfile ) == 0 ||
           ( is_workfile == 1 ) )
      {
         load_flag = 0 ;
      }
      else
      {
         load_flag = 1 ;
      }

      /* Check if the current file being read is a regular
         text file; i.e. not a directory or special file */
      if ( load_flag == 1 )
      {
         malloc_length = ( strlen ( data_dir ) + 1 + strlen ( pidfile ) + 1 )
                         * sizeof ( char )  ; 
         full_filename = ( char * ) malloc ( malloc_length ) ; 

         if ( full_filename == NULL )
         {
            sprintf ( msgstr , "In routine 'gage_pp_check_input':\n"
                               "Could not allocate %d bytes for "
                               "full_filename.\n" ,  malloc_length ) ; 
            writelog ( msgstr ) ;
            return GPP_ERROR ;
         }

         memset ( full_filename , '\0' , malloc_length ) ;
         sprintf ( full_filename , "%s/%s" , data_dir , pidfile ) ;

         if ( ( ret = stat ( full_filename, &filestat ) ) >= 0 )
         {
            if ( !( filestat.st_mode & 0100000 ) )
            {
               load_flag = 0 ;
            }

	    free ( full_filename ) ;
	    full_filename = NULL ;
         }
         else
         {
            sprintf ( msgstr , "Error checking file status on: %s ; errno= %d" ,
                    full_filename , errno ) ;
            writelog ( msgstr ) ;
            free ( full_filename ) ;
            return GPP_ERROR ;
         }
      }

      /* Check that the file name length is valid */

      if ( load_flag == 1 )
      {
         if ( strlen ( filename ) <= 0 || strlen (filename ) > 80 )
         {
            sprintf ( msgstr, "Error - invalid length for filename: %s" ,
                    filename ) ;
            writelog ( msgstr ) ;
            load_flag = 0 ;
         }
      }

      /* If passed all tests, then add file to list */
      if ( load_flag == 1 )
      {
         malloc_length = strlen ( filename ) + 1 ;
         infiles [ cnt ] = ( char * ) malloc ( malloc_length ) ;

         if ( infiles [ cnt ] == NULL )
         {
            sprintf ( msgstr , "In routine 'gage_pp_check_input':\n"
                               "Error allocating %d bytes for input file list" ,                               malloc_length ) ;
            writelog ( msgstr ) ;
            break ;
         }
         else
         {
            memset ( infiles [ cnt ] , '\0' , malloc_length ) ;
            strcpy ( infiles [ cnt ] , filename ) ;
            cnt++ ;
         }
      }

      pRead = fgets ( filename , BUFSIZ , infilePtr ) ;
   }
   /* Issue message if in catchup mode, as determined by number of files
      logged */

   if ( cnt == MAXFILES )
   {
      sprintf ( msgstr ,
              "In full catchup mode; processing maximum number of files (%d)" ,
              MAXFILES ) ;
      writelog ( msgstr ) ;
   }

   /* Close the input list file */
   fclose (infilePtr ) ;

   *numfound = cnt ;

   return GPP_OK ;
}
