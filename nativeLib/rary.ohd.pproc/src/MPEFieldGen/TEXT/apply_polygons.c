/*******************************************************************************
* FILENAME:            apply_polygons.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>

#include "delete_polygons_show.h"
#include "draw_precip_poly_RFCW.h" 
#include "mpe_fieldgen.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
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

static int ** xmrg_data_array = NULL;

void MPEFieldGen_editPolygonConstructor ( const geo_data_struct * pGeoData )
{
   int i;

   if ( xmrg_data_array == NULL )
   {
      /* Allocate memory for the temporary array.
         Data from the substitute field will be read into this array. */
      xmrg_data_array = ( int ** ) malloc ( ( pGeoData->num_cols )
                                              * sizeof ( int * ) );

      if ( xmrg_data_array == NULL )
      {
         sprintf ( message, "\nIn routine 'apply_edit_polygon_constructor':\n"
                            "Could not allocate memory for the "
                            "xmrg_data_array.\n" );
         return;
      }

      for ( i = 0; i < pGeoData->num_cols; ++i )
      {
         xmrg_data_array [ i ] = ( int * ) 
                                  malloc ( pGeoData->num_rows * 
                                           sizeof ( int ) );

         if ( xmrg_data_array [ i ] == NULL )
         {
            sprintf ( message, 
                      "\nIn routine 'apply_edit_polygon_constructor':\n"
                      "Could not allocate memory for the "
                      "xmrg_data_array.\n" );
            shutDownMPE ( message, logFile );
         }
      } 
   }
}

void MPEFieldGen_apply_mpe_polygons ( double ** real_mosaic,
                          const char * dateYMD,
                          int year,
                          int month,
                          int day,
                          int hour,
                          enum DisplayFieldData field,
                          const geo_data_struct * pGeoData,
                          double scale_factor,
                          int add_flag,
	                  int draw_only_persistent )
{
   const char * filename = NULL;
   double double_value;
   double units_factor = 25.4;
   int i;
   int j;
   int status;
   int value;

   /* Test to determine if there is a polygon file. If not, do not do 
      anything. */
   filename = check_for_polygon_file ( field, dateYMD, & status );

   sprintf ( message, "Check for polygon file %s", filename );
   printMessage ( message, logFile );
   
   if ( status == 0 )
   {
      sprintf ( message, "Polygon file not there." );
      printMessage ( message, logFile );
      return;
   }

   /* Copy the data array (which is double) to the output array
      which is integer. */
   for ( i = 0; i < pGeoData->num_rows; ++i )
   {
      for ( j = 0; j < pGeoData->num_cols; ++j )
      {
         value = (int) (real_mosaic[i][j] * scale_factor) ;
         xmrg_data_array [ j ] [ i ] = value;
      }
   }

   /* Determine if there are edit polygons to be drawn on this field.
      If there are, then draw them. */
   apply_edit_polygons ( xmrg_data_array,
                         dateYMD,
                         year,
                         month,
                         day,
                         hour,
                         scale_factor,
                         units_factor,
                         field,
                         pGeoData->num_rows,
                         pGeoData->num_cols,
                         pGeoData->hrap_x,
                         pGeoData->hrap_y,
                         add_flag,
		         draw_only_persistent ); 

   /* Copy the temporary int array to the double output array. */
   for ( i = 0; i < pGeoData->num_rows; ++i )
   {
      for ( j = 0; j < pGeoData->num_cols; ++j )
      {
         double_value = ( double )((( double ) xmrg_data_array [j][i]) / 
                                    scale_factor ) ;
         real_mosaic [i][j] = double_value ;
      }
   }

    /* The polygons have been applied.   Return this field to the calling
       routine so that it can be written out to a xmrg file. */
    return;

} /* end MPEFieldGen_apply_mpe_polygons */

void MPEFieldGen_editPolygonDestructor ( const geo_data_struct * pGeoData )
{
   int i;

   if ( xmrg_data_array != NULL )
   {
      for ( i = 0; i < pGeoData->num_cols; ++i )
      {
         free ( xmrg_data_array [ i ] );
         xmrg_data_array [ i ] = NULL;
      }

      free ( xmrg_data_array );
      xmrg_data_array = NULL;
   }

   free_poly_temp ( );
} /* end MPEFieldGen_editPolygonDestructor */

/* Honestly, this function seems to do absolutely NOTHING! */
bool MPEFieldGen_eliminateP3Gages ( int hrapx, int hrapy, enum DisplayFieldData field )
{
   return true;

} /* MPEFieldGen_eliminateP3Gages */
