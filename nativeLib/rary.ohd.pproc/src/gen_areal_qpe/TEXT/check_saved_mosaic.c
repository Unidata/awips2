#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "gen_areal_qpe.h"

/******************************************************************************

PURPOSE:
   Checks for the existence of an XMRG file containing the RFC QPE Mosaic
   for a given QPE duration and ending time.  Returns FOUND if the file is
   found and NOT_FOUND if the file does not exist.
******************************************************************************/
int check_if_saved_mosaic ( const gaq_options_struct * options,
                            const qpe_info_struct * qpe_info )
{
   const char * xmrg_path = NULL;
   int file_exists = FOUND;
   int status;
   struct stat file_stat;
   
   /* Build the path and filename to the xmrg file. */
   xmrg_path = build_saved_xmrg_filename ( options, qpe_info );
 
   if ( xmrg_path == NULL )
   {
      file_exists = NOT_FOUND;
   }
   else
   {
      status = stat ( xmrg_path, & file_stat );

      if ( status == -1 )
      {
         file_exists = NOT_FOUND;
      }
   }

   return file_exists;
}
