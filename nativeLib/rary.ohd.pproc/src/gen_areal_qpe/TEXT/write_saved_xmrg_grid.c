#include <stdlib.h>

#include "gen_areal_qpe.h"

/******************************************************************************
   PURPOSE
   This routine writes the WFO RFC QPE mosaic XMRG file to the temporary 
   directory.
******************************************************************************/
static size_t get_record_size ( num_cols )
{
   size_t record_length;
   record_length = ( num_cols * sizeof ( short ) ) + 2 * sizeof ( int ) ;
   return record_length;
}

int write_saved_xmrg_grid ( const gaq_options_struct *  options,
                            const geo_data_struct * coord_info,
                            const qpe_info_struct * qpe_info,
                            float ** pXmrgGrid)
{
   const char * xmrg_temp_filename = NULL;
   const char * xmrg_saved_filename = NULL;
   static const char * user = "LXSAN";
   int byte_count;
   int i;
   int j;
   int record_length;
   long status;
   short int * out_mosaic  = NULL;
   short int * pRecord = NULL;
   short int value;

   /* Create the output filename. */
   xmrg_temp_filename = build_temp_xmrg_filename ( options, qpe_info );

   if ( xmrg_temp_filename == NULL )
   {
      return FAIL;
   }

   xmrg_saved_filename = build_saved_xmrg_filename ( options, qpe_info );

   if ( xmrg_saved_filename == NULL )
   {
      return FAIL;
   }

   /* Compute the number of bytes per row in the XMRG file. */ 
   byte_count = coord_info->num_cols * sizeof ( short );

   /* Determine the length of a record in the XMRG file. */
   record_length = get_record_size ( coord_info->num_cols );

   /* Allocate memory for the output mosaic. */

   out_mosaic = ( short * ) malloc ( coord_info->num_rows * record_length );
   
   if ( out_mosaic == NULL )
   {
      printf ( "Could not allocate memory for out_mosaic to write %s.\n",
               xmrg_saved_filename );
      return FAIL;
   }
 
   pRecord = out_mosaic;

   /* Copy the QPE data to the outmosaic. */
   for ( i = 0; i < coord_info->num_rows; i++)
   {
      memcpy ( pRecord, & byte_count, sizeof ( int ) );
      pRecord += 2;
 
      for ( j = 0; j < coord_info->num_cols; j++)
      { 
         /* Need to convert from millimeters to hundredths of 
            millimeters. */
         if ( ( pXmrgGrid[i][j] != (float) XMRG_MISSING ) &&
              ( pXmrgGrid[i][j] >= 0.0 ) )
         {
            value = (short int) (pXmrgGrid[i][j] * PRECIP_FACTOR) ;
         }
         else
         {
            value = XMRG_MISSING;
         }

         * pRecord = value;

         ++ pRecord; 
      }

      memcpy ( pRecord, & byte_count, sizeof ( int ) );
      pRecord +=2;
   }

   /* Call the routine which writes the XMRG file. 
      Write the saved XMRG file. */
   writeXmrg (  coord_info,
                xmrg_saved_filename,
                user,
                qpe_info->endtime,
                qpe_info->proc_flag,
                out_mosaic,
                record_length,
                & status ); 

   if ( status != 0 )
   {
      printf ( "The write of saved XMRG file %s failed.\n", 
               xmrg_saved_filename );
      /* Free the memory used by the output mosaic. */
      free ( out_mosaic );
      out_mosaic = NULL;
      return FAIL;
   }
   else
   {
      printf ( "Saved XMRG file %s written.\n", xmrg_saved_filename );
   }

   /* Call the routine which writes the XMRG file. 
      Write the temporary xmrg file.*/
   writeXmrg (  coord_info,
                xmrg_temp_filename,
                user,
                qpe_info->endtime,
                qpe_info->proc_flag,
                out_mosaic,
                record_length,
                & status ); 

   /* Free the memory used by the output mosaic. */
   free ( out_mosaic );
   out_mosaic = NULL;

   if ( status != 0 )
   {
      printf ( "The write of temp XMRG file %s failed.\n", 
                 xmrg_temp_filename );
      return FAIL;
   }  
   else
   {
      printf ( "Temporary XMRG file %s written.\n", xmrg_temp_filename );
      return SUCCESS;
   }
               

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
