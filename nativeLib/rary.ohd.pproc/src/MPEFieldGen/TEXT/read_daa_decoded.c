#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "GeneralUtil.h"
#include "GetOS.h"
#include "read_stage1_decoded.h"
#include "Swap4Bytes.h"
/*-------------------------------------------------*/
/*  this routine opens, reads and closes the file  */
/*   containing the decoded DAA product            */
/*  calling routine: readRDMosaicRadars            */
/*-------------------------------------------------*/

void read_daa_decoded ( const char * filename ,
                            const int * fname_length ,
                            float radar [ ] [ NUM_DPA_COLS ] ,
                            int * ierr )
{
   static int firstdaa = 1;
   static char * daa_grid_dir_token = "daa_grid_dir" ;
   char * filepath = NULL ;
   char * fname = NULL ;
   static char reply [ 100 ] = {'\0'};
   FILE * pFile = NULL ;
   float * pValue = NULL;
   static double radar_zero;
   register int j ;
   int length ;
   static int reply_len ;
   int status ;

   radar_zero = pow ( 10.0, -98.0/10.0 );

   * ierr = 0 ;

   pValue = &radar[0][0]; 

   /* Create the array to contain the filename, allowing for space
      for the '\0' character at the end of the string. */
   fname = ( char * ) malloc ( sizeof ( char ) * ( * fname_length ) + 1 ) ; 

   if ( fname == NULL ) 
   {
      fprintf ( stderr , "In \"read_daa_data\":\n"
                         "Could not allocate memory for \"fname\". Aborting\n"
                         "the attempt to read the daa decoded file\n"
                         "\"%s\".\n" , filename ) ;
      * ierr = 1 ;
      return  ;
   }

   strncpy ( fname , filename , * fname_length ) ;
   fname [ * fname_length ] = '\0' ;
  
   if ( firstdaa == 1 )
   {
      firstdaa = 0;
      length = strlen ( daa_grid_dir_token ) ;
      status = get_apps_defaults ( daa_grid_dir_token , & length ,
                                   reply , & reply_len ) ;

      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine \"read_daa_data\":\n"
                            "Cannot retrieve token \"%s\". Aborting\n"
                            "attempt to read daa file \"%s\".\n" ,
                            daa_grid_dir_token , filename ) ; 
         * ierr = 2 ; 
      }
   }

   length = strlen ( fname ) + reply_len + 2 ;
   filepath = ( char * ) malloc ( length * sizeof ( char ) ) ;

   if ( filepath == NULL )
   {
      fprintf ( stderr , "\nIn routine \"read_daa_data\":\n"
                         "Could not allocate %d bytes of memory for\n"
                         "the \"filepath\" variable.  Aborting the\n"
                         "attempt to read daa file \"%s\".\n" ,
                         length , filename ) ; 
      * ierr = 3 ;
   }
   else
   {
      sprintf ( filepath , "%s/%s" , reply , fname ) ;
      pFile = fopen ( filepath , "r" ) ;
         
      if ( pFile == NULL )
      {
         fprintf ( stderr , "\nIn routine \"read_daa_data\":\n"
                            "Could not open daa file \"%s\".\n" ,
                             filepath ) ;
         * ierr = 4 ;
      }
      else /* ( pFile != NULL ) */
      {
         status = fread ( pValue, sizeof (float), NUM_DPA_ELEMENTS, 
                          pFile );

         fclose ( pFile ) ;
         pFile = NULL ;

         if ( status != NUM_DPA_ELEMENTS )
         {
            * ierr = 5 ;
            free (fname );
            fname = NULL;
            return;
         }

         /* Process the values.
            A value of -99. signifies out-of-range of the radar and
            is considered missing.  */
         pValue = &radar[0][0]; 

         for ( j = 0 ; j < NUM_DPA_ELEMENTS ; ++ j )
         {
            if ( *pValue > -99.)
            {
               *pValue = ( float ) pow ( 10 , *pValue / 10. ) ;
            }

            ++pValue;
         } /* for j */

      } /* ( pFile != NULL ) */
   }

   if ( fname != NULL )
   {
      free ( fname ) ;
      fname = NULL ;
   }

   if ( filepath != NULL )
   {
      free ( filepath ) ;
      filepath = NULL ;
   }

} /* end read_daa_decoded */
