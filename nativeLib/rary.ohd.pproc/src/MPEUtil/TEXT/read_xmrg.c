#include <stdio.h>
#include "mpe_log_utils.h"

const int MPEUtil_num_bytes_to_discard = 94 ;

void read_xmrg ( const int * mx , const int * my , const int * irow ,
                 const char * fname , const int * length , int * ifile , 
                 short * mosaic )
{
   char garbage [ MPEUtil_num_bytes_to_discard ] ;
   static FILE * pFile = NULL ;

   * ifile = 0 ;
   
   if ( * irow == 0 )
   {
      pFile = fopen ( fname , "r" ) ;

      if ( pFile == NULL )
      {
         * ifile = -1 ;
         return ;
      }

      /* Read and discard the first two records.  This is equivalent to
         reading and discarding 94 bytes. */
      fread ( & garbage , MPEUtil_num_bytes_to_discard , 1 , pFile ) ;   

      if ( feof ( pFile ) )
      {
         * ifile = -1 ;
         fclose ( pFile ) ;
         pFile = NULL ;
         return ;
      }
   }

   /* Read a record in.  Since these files are created with fortran read/write
      routines, discard the last 4 bytes of the previous record and the
      first four bytes of this record. */
   fread  ( & garbage , 2 * sizeof ( int ) , 1 , pFile ) ; 

   if ( feof ( pFile ) )
   {
      * ifile = -1 ;
      fclose ( pFile ) ;
      pFile = NULL ;
      return ;
   } 

   /* Read in a record from the file. */
   fread ( mosaic , sizeof ( short ) * ( * mx ) , 1 , pFile ) ; 

   if ( feof ( pFile ) )
   {
      * ifile = -1 ;
      fclose ( pFile ) ;
      pFile = NULL ;
      return ;
   } 

   if ( * irow == ( * my - 1 ) )
   {
      fclose ( pFile ) ;
      pFile = NULL ;
   }

}
