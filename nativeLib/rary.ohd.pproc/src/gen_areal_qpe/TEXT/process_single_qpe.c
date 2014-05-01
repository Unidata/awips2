#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <math.h>

#include "gen_areal_qpe.h"


/*********************************************************************
   process_single_qpe()
   
   PURPOSE
   Reads a netCDF RFC QPE product and stores the portion of it that 
   falls in the WFOs MPE forecast area on the XMRG grid
   
 ********************************************************************/

int process_single_qpe(const gaq_options_struct * options,
                       int rfc_mask [][RFC_MASK_COLS],
                       const geo_data_struct * coord_info,
		       const qpe_info_struct *	qpe_info,
                       float ** pXmrgGrid )
{   
  int duration_match = NOT_FOUND;
  int grid_exists = NOT_FOUND;
  int i;
  int j;
  int rfc_name_match = NOT_FOUND;
  int status;
   
   /* check if the rfc and duration match the user-specified filter */
   for ( i = 0; i < options->num_rfcs; ++i )
   {
      status = strcmp ( options->rfcid[i], qpe_info->rfc );

      if ( status == 0 )
      {
         rfc_name_match = FOUND;
         break;
      }
   }

   if ( rfc_name_match == NOT_FOUND )
   {
      printf ( "RFC %s is not in the user-specified list of RFCs to "
               "mosaic.\n", qpe_info->rfc );
      return FAIL;
   }

   /* Check the duration against the user-specified list of durations. */
   for ( i = 0; i < options->num_durations; ++i )
   {
      if ( qpe_info->dur == options->durations[i] )
      {
         duration_match = FOUND;
         break;
      }
   }

   if ( duration_match == NOT_FOUND )
   {
      printf ( "Duration %d is not in the user-specified list of RFC QPE "
               "product durations to mosaic.\n", qpe_info->dur );
      return FAIL;
   }

   /* Initialize the XMRG data grid to all missing. */
   for ( i = 0; i < coord_info->num_rows; ++i )
   {
      for ( j = 0; j < coord_info->num_cols; ++j )
      {
         pXmrgGrid[ i ] [ j ] = XMRG_MISSING;
      }
   } 
   
   /* check if an output XMRG file already exists for the given
      duration and ending time. */
   grid_exists = check_if_saved_mosaic( options, qpe_info ); 

   if ( grid_exists == FOUND )
   {
      /*  Read the XMRG mosaic into memory. */
      status = read_saved_xmrg_grid ( options, 
                                      coord_info, 
                                      qpe_info, 
                                      pXmrgGrid );

      if ( status == FAIL )
      {
         return FAIL;
      }
   }
      
   
   /* Read the netcdf file and overlay input grid data into 
      mosaicked output XMRG grid */
   status = process_rfc_qpe_grid ( options,
                                   rfc_mask,
                                   coord_info,
                                   qpe_info,
                                   pXmrgGrid);


   if ( status == FAIL )
   {
      return FAIL;
   }
   
   /* write the xmrg grid to temporary and saved xmrg directories. */
   status = write_saved_xmrg_grid ( options,
                                    coord_info,
                                    qpe_info,
                                    pXmrgGrid );

   return status; 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

} 

