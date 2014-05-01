/*******************************************************************************
* FILENAME:            get_rfc_mask
* DESCRIPTION:         Contains the code to read the national RFC mask file
*                      used by NPVU in producing the national mosaic
*                      of RFC QPE products.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       July 27, 2006
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Red Hat Enterprise 4 Linux
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
*     7/27/2006    Bryon Lawrence    Original Coding    
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>

#include "gen_areal_qpe.h"
#include "GeneralUtil.h"

/*******************************************************************************
* MODULE NAME:        get_rfc_mask
* PURPOSE:            Reads the RFC mask file used by the National 
*                     Precipitation Verification Unit (NPVU) when
*                     producing its mosaic of RFC QPE products.
*
*                     This product is stored in an 881 by 1121 array
*                     of integers which corresponds to the national
*                     HRAP grid. The values in this grid are as follows:
*
*                     -9999  missing
*                     150 ABRFC
*                     152 CBRFC
*                     153 CNRFC
*                     154 LMRFC
*                     155 MARFC
*                     156 MBRFC
*                     157 NCRFC
*                     158 NERFC
*                     159 NWRFC
*                     160 OHRFC
*                     161 SERFC
*                     162 WGRFC
*
*                     These values indicate which RFC a grib bin belongs to.
*                     QPE data from that RFC will be used to fill in the
*                     grid bin.
*
* ARGUMENTS:
*   TYPE           DATA TYPE   NAME                 DESCRIPTION/UNITS
*   input/output   int[][]     rfc_mask
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void get_rfc_mask ( int rfc_mask [ ] [ RFC_MASK_COLS ] )
{
   static char * gaq_rfc_mask_dir = "gaq_rfc_mask_dir";
   static const char * rfc_mask_filename = "national_hrap_rfc_mask";
   static char rfc_mask_path [ 200 ] = {'\0'};
   char token_value [ 150 ];
   FILE * pMaskFile = NULL;
   int i;
   int j;
   int request_length;
   int reply_length;

   /* Read the token which points to the location of the RFC mask file. */
   request_length = strlen ( gaq_rfc_mask_dir );
   get_apps_defaults ( gaq_rfc_mask_dir, & request_length, token_value, 
                       & reply_length ); 

   if ( reply_length > 0 )
   {
      /* Build the rfc mask file path. */
      sprintf ( rfc_mask_path, "%s/%s", token_value, rfc_mask_filename );

      /* Open the RFC mask file. */
      pMaskFile = fopen ( rfc_mask_path, "r" );

      if ( pMaskFile != NULL )
      {
         /* Skip the 20 byte header in the RFC mask file. */
         fseek ( pMaskFile, 20, SEEK_SET ); 

         /* Read the file into the rfc_mask array. */
         fread ( rfc_mask, sizeof ( int ) * RFC_MASK_ROWS * RFC_MASK_COLS, 1, 
                 pMaskFile ); 

         /* Close the file. */
         fclose ( pMaskFile );
      }
      else
      {
         /* If the file cannot be opened, then set the grid to contain 
            all -1's. */
         for ( i = 0; i < RFC_MASK_ROWS; ++i )
         {
            for ( j = 0; j < RFC_MASK_COLS; ++j )
            {
                rfc_mask [ i ] [ j ] = RFC_MASK_MISSING_VALUE; 
            }
         }
      }

   }
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
