#include <string.h>

#include "gen_areal_qpe.h"
#include "read_xmrg.h"

 /***********************************************************************
   
    read_saved_xmrg_grid
    
    Generates a mosaic of multiple RFC QPE products as needed and crops the 
    resulting mosaic into a local-sized grid defined by the MPE coord file.
    Reads the QPE products from an RFC that are placed in a temporary 
    directory to hold the just-decoded GRIB products received from the SBN.

 *************************************************************************/

int read_saved_xmrg_grid ( const gaq_options_struct * options,
                           const geo_data_struct * coord_info,
                           const qpe_info_struct * qpe_info_struct,
                           float ** pXmrgGrid)
{
   const char * xmrg_path = NULL;
   int file_name_length;
   int i;
   int ifile;
   int j;
   short temp [ coord_info->num_cols ];
   
   xmrg_path = build_saved_xmrg_filename ( options, qpe_info_struct ); 

   if ( xmrg_path == NULL )
   {
      return FAIL;
   }

   /* The operating system this file was created on can be assumed to be 
      Linux. No byte flipping is necessary. */
   file_name_length = strlen ( xmrg_path );

   for ( i = 0; i < coord_info->num_rows; ++i )
   {
      read_xmrg ( & coord_info->num_cols, & coord_info->num_rows, &i, xmrg_path,
                  & file_name_length, & ifile, temp );

      if ( ifile != 0 )
      {
         printf ( "Error reading %s.\n", xmrg_path );
         return FAIL;
      }

      for ( j = 0; j < coord_info->num_cols; ++j )
      {
         if ( ( temp[j] != XMRG_MISSING ) &&
              ( temp[j] >= 0 ) )
         {
            pXmrgGrid [i][j] = (float) temp [j] / ( float ) PRECIP_FACTOR;
         }
      }
   }

   return SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
