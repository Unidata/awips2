/******************************************************************************

 Function:  convert_ascii_to_latlon_main.c
 Purpose:   Takes the output from an NWSRFS run (@DUMP PUNCH BASIN)
            processes one basin at a time and writes information to an ascii
            file.  Later call to function convert_ascii_to_latlon reads
            the ascii file and creates the binary file in a standard binary
            format for RFC applications.

             Allows command one line option:
                  ascii:  that assumes existance of ascii file (in proper
                          format) then only calls routine to create binary
                          file from the ascii one.
                    OR
                  filename:  filename for the raw punch from NWSRFS
                             (can be full path name)

   Written by:  Donna Page - HRL - 9 Feb. 1994
   Revision by: Moria Shebsovich - OHD - July 2003
   ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "convert_ascii_to_latlon.h"
#include "GeneralUtil.h"

/******************************************************************************

 Function:  main()

   ****************************************************************************/

int convert_ascii_to_latlon_main ( int argc , const char ** argv)
{
   int      len ;
   int      len2 ;
   char     ascii_fname [ BUFSIZ ] ;
   char     bin_fname [ BUFSIZ ] ;
   char     path_name [ BUFSIZ ] ;
   char     temp_str [ BUFSIZ ] ;

   memset ( ascii_fname , '\0' , BUFSIZ ) ;
   memset ( bin_fname , '\0' , BUFSIZ ) ;
   memset ( path_name , '\0' , BUFSIZ ) ;
   memset ( temp_str , '\0' , BUFSIZ ) ;

   /* check number of arguments */

   if (argc != 3)
   {
      printf("Only 2 command line arguments allowed - Try again");
      exit(1);
   }
   else
   {
      /* get geo data token value */
      len = strlen("whfs_geodata_dir");
      get_apps_defaults("whfs_geodata_dir", &len, path_name, &len2);
      strcat(path_name, "/");

      /*  Test to determine whether or not the user has supplied the
          full file path and name for the input argument.  If the full
          path and filename have been supplied then use the file as is.
          Otherwise, use the path build from the "whfs_geodata_dir"
          token to locate the filename. */
      if ( strchr ( argv [ 1 ] , '/' ) != NULL )
      {
         strcpy ( ascii_fname , argv [ 1 ] ) ;
      }
      else
      {
         sprintf ( ascii_fname , "%s%s" , path_name , argv [ 1 ] ) ;
      }

      printf ("ascii_file: %s\n", ascii_fname);
      sprintf ( bin_fname , "%s%s" , path_name , argv [ 2 ] ) ;
      printf ("bin_file: %s\n", bin_fname);
      convert_ascii_to_latlon(ascii_fname, bin_fname);
   }
   exit (0);
}
