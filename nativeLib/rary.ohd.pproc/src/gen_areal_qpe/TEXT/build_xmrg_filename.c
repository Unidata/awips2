#include <stdio.h>
#include <string.h>
#include <time.h>

#include "gen_areal_qpe.h"

/***********************************************************************

    build_saved_xmrg_filename

    Builds the name of the input XMRG file which contains the 
    RFC QPE Mosaic.  This is based upon the ending time and duration
    of the current RFC QPE file being processed.

 *************************************************************************/
const char * build_saved_xmrg_filename ( const gaq_options_struct * options,
                                         const qpe_info_struct * 
                                         qpe_info_struct )
{
   static char xmrg_path [ MAX_FILE_LEN + 1 ];
   char time_string [ YYYYMMDDHH_LEN + 1 ];
   struct tm * pStructTm = NULL;

   /* Clear the path array. */
   memset ( xmrg_path, '\0', MAX_FILE_LEN + 1 );

   /* Convert ticks to struct tm. */
   pStructTm = gmtime ( & qpe_info_struct->endtime );

   /* Append the datetime in a YYYYMMDDHHz format. */
   strftime ( time_string, YYYYMMDDHH_LEN + 1, "%Y%m%d%H", pStructTm );

   /* Build the xmrg path and filename. */
   switch ( qpe_info_struct->dur )
   {
      case 1:
         sprintf ( xmrg_path, "%s/%s%02d%sz", options->input_xmrg_path_1hr,
                              RFC_QPE_BASE_STRING, 
                              qpe_info_struct->dur, time_string );
         break;

      case 6:
         sprintf ( xmrg_path, "%s/%s%02d%sz", options->input_xmrg_path_6hr,
                              RFC_QPE_BASE_STRING, 
                              qpe_info_struct->dur, time_string );
         break;

      case 24:
         sprintf ( xmrg_path, "%s/%s%02d%sz", options->input_xmrg_path_24hr,
                              RFC_QPE_BASE_STRING, 
                              qpe_info_struct->dur, time_string );
         break;

      default:
         printf ( "Unrecognized duration %d.\n", qpe_info_struct->dur );
         return NULL;
   }

   return xmrg_path;
     
}

/***********************************************************************

    build_temp_xmrg_filename

    Builds the name of the XMRG file output to the temporary 
    directory. This file contains the RFC QPE mosaic.
    This is based upon the ending time and duration
    of the current RFC QPE file being processed.

 *************************************************************************/
const char * build_temp_xmrg_filename ( const gaq_options_struct * options,
                                        const qpe_info_struct * 
                                                        qpe_info_struct )
{
   static char xmrg_path [ MAX_FILE_LEN + 1 ];
   char time_string [ YYYYMMDDHH_LEN + 1 ];
   struct tm * pStructTm = NULL;

   /* Clear the path array. */
   memset ( xmrg_path, '\0', MAX_FILE_LEN + 1 );

   /* Convert ticks to struct tm. */
   pStructTm = gmtime ( & qpe_info_struct->endtime );

   /* Append the datetime in a YYYYMMDDHHz format. */
   strftime ( time_string, YYYYMMDDHH_LEN + 1, "%Y%m%d%H", pStructTm );

   /* Build the xmrg path and filename. */
   switch ( qpe_info_struct->dur )
   {
      case 1:
         sprintf ( xmrg_path, "%s/%s%02d%sz", options->output_xmrg_path,
                              RFC_QPE_BASE_STRING, 
                              qpe_info_struct->dur, time_string );
         break;

      case 6:
         sprintf ( xmrg_path, "%s/%s%02d%sz", options->output_xmrg_path,
                              RFC_QPE_BASE_STRING, 
                              qpe_info_struct->dur, time_string );
         break;

      case 24:
         sprintf ( xmrg_path, "%s/%s%02d%sz", options->output_xmrg_path,
                              RFC_QPE_BASE_STRING, 
                              qpe_info_struct->dur, time_string );
         break;

      default:
         printf ( "Unrecognized duration %d.\n", qpe_info_struct->dur );
         return NULL;
   }

   return xmrg_path;
     
}
