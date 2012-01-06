/*******************************************************************************
* FILENAME:             read_stage1_decoded.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*   MODULE 1:           read_stage1_decoded_
* DESCRIPTION:          This routine reads in a decoded DPA radar product.
*                       If this routine is being run on the Linux operating
*                       system, then it will automatically swap the bytes
*                       in each word of data so that the information can be 
*                       read and processed correctly in a little Endian
*                       memory architecture.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        May 31, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP Unix / Dell Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        5/31/2002    Bryon Lawrence    Original Coding.
*                   9/20/2002    P Tilles          added test for out-of-range
*                                                  values
*                   8/13/2003    P Tilles          (1) Removed read of extra 4 bytes
*                                                      at beginning and end of product
*                                                      (product now written using C fwrite -
*                                                      previously used FORTRAN write)
*                                                  (2) changed test on OS so that
*                                                      if OS = UNIX, then call Swap4Bytes
********************************************************************************
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "GeneralUtil.h"
#include "GetOS.h"
#include "read_stage1_decoded.h"
#include "Swap4Bytes.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    read_stage1_decoded_
* PURPOSE:        This routine reads the contents of a file containing
*                 decoded DPA data and stores in a 131 by 131 array of
*                 floating values.  This routine assumes that the 
*                 file has a Little Endian memory architecture.  When this
*                 utility is used on a Unix operating system, it will
*                 automatically flip the bytes in each word to mimic
*                 the Big Endian architecture of Unix. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME            DESCRIPTION/UNITS
*   input  char *        filename        The name of the file containing
*                                        the decoded dpa data. 
*   i/o    float [ ] [ ] radar           The dpa data read from the file is
*                                        returned in this array.
*                                        rows and columns.
*   output int *         ierr            An error flag.  A value of "0" means
*                                        that the routine ran ok.  A value
*                                        of "1" means that it did not. 
*
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                   HEADER FILE    DESCRIPTION
*   GetOS                  GetOS.h        Returns the type of operating system
*                                         this routine is running under.
*   get_apps_defaults      GeneralUtil.h  Returns the value of the
*   Swap4Bytes_            Swap4Bytes.h   This routine swaps 4 bytes.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*   This routine uses the filename passed for locating the decoded DPA
*   product.  It used the "dpa_grid_dir" token to build a path to this file.
*
* ERROR HANDLING:
*    ERROR CODE          DESCRIPTION
*    0                   The routine ran without detectable errors.
*    1                   Could not allocate memory for the internal filename.
*    2                   Could not retrieve the dpa_grid_dir token.
*    3                   Could not allocate memory for the internal path. 
*    4                   Could not open the stage1 file.
*    5                   An error was encountered reading the stage1 file.
********************************************************************************
*/
void read_stage1_decoded_ ( const char * filename ,
                            const int * fname_length ,
                            float radar [ ] [ NUM_DPA_COLS ] ,
                            int * ierr )
{
   static int first = 1;
   static char * dpa_grid_dir_token = "dpa_grid_dir" ;
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
      fprintf ( stderr , "In \"read_stage1_data\":\n"
                         "Could not allocate memory for \"fname\". Aborting\n"
                         "the attempt to read the stage1 decoded file\n"
                         "\"%s\".\n" , filename ) ;
      * ierr = 1 ;
      return  ;
   }

   strncpy ( fname , filename , * fname_length ) ;
   fname [ * fname_length ] = '\0' ;
  
   if ( first == 1 )
   {
      first = 0;
      length = strlen ( dpa_grid_dir_token ) ;
      status = get_apps_defaults ( dpa_grid_dir_token , & length ,
                                   reply , & reply_len ) ;

      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine \"read_stage1_data\":\n"
                            "Cannot retrieve token \"%s\". Aborting\n"
                            "attempt to read stage1 dpa file \"%s\".\n" ,
                            dpa_grid_dir_token , filename ) ; 
         * ierr = 2 ; 
      }
   }

   length = strlen ( fname ) + reply_len + 2 ;
   filepath = ( char * ) malloc ( length * sizeof ( char ) ) ;

   if ( filepath == NULL )
   {
      fprintf ( stderr , "\nIn routine \"read_stage1_data\":\n"
                         "Could not allocate %d bytes of memory for\n"
                         "the \"filepath\" variable.  Aborting the\n"
                         "attempt to read stage1 dpa file \"%s\".\n" ,
                         length , filename ) ; 
      * ierr = 3 ;
   }
   else
   {
      sprintf ( filepath , "%s/%s" , reply , fname ) ;
      pFile = fopen ( filepath , "r" ) ;
         
      if ( pFile == NULL )
      {
         fprintf ( stderr , "\nIn routine \"read_stage1_data\":\n"
                            "Could not open stage1 dpa file \"%s\".\n" ,
                             filepath ) ;
         * ierr = 4 ;
      }
      else
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

         /* Preprocess the values.
            A value of -99. signifies out-of-range of the radar and
            is considered missing.  */
         pValue = &radar[0][0]; 

         for ( j = 0 ; j < NUM_DPA_ELEMENTS ; ++ j )
         {
            if ( *pValue > -99.)
            {
               if ( *pValue == -98 )
               {
                  *pValue = ( float ) radar_zero;
               }
               else
               {
                  *pValue = ( float ) pow ( 10 , *pValue / 10. ) ;
               }
            }

            ++pValue;
         }
      }
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
}
