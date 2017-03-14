/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "convert_hrap.h"
#include "read_stage1_decoded.h"

#define PI 3.141592654

/*******************************************************************************
* FILENAME:            calculate_pixel_height.c
*
* Purpose:
* This function is converted from FORTRAN code: pixheight.f.
* it takes the hrap coordinate (hrap_coord_x,hrap_coord_y)
* that the radar is located in, and computes the height
* of a pixel located at hrap location(hrap_x,hrap_y)
* assuming that the pixel originated from that radar.
* creates mosaics of the raw radar data
* and radar height data.
*
* calling function: createMosaic
* functions called: HrapToLatLong
*
* input variables
*
* hrap_coord_x - the hrap coordinate for x direction
*
* hrap_coord_y - the hrap coordinate for y direction
*
* hrap_x - the hrap location for x direction
*
* hrap_y - the hrap location for y direction
*
*
* output variables
*
* pixelHeight - the height of a pixel located at given hrap location
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   02/25/2005   Guoxian Zhou      finish conversion to C Language
*   09/13/2005   Guoxian Zhou      modification for using lat/long
*                                  instead of hrap coordinate value
*
********************************************************************************
*/

static void calculatePixelHeight(const double lon_coord,
                                 const double lat_coord,
                                 const double hrap_coord_x,
                                 const double hrap_coord_y,
			         const double hrap_x,
                                 const double hrap_y,
                                 double * pixelHeight  )
{
    double lat, lon ;
    double dtorad, earthr, in, phi1, phi2, dlam;
    double cost, thet, radi, sina, alph, cosa;
    double azmt, arg, disx, disy, range;

    /**
     * check for case where the two points are the same
     **/
    if ((hrap_coord_x == hrap_x) && (hrap_coord_y == hrap_y))
    {
        *pixelHeight = 0.0 ;
        return ;
    }

    /**
     * convert hrap coordinates to lat/lon
     **/
    HrapToLatLongByReference(hrap_y, hrap_x, &lat, &lon);

    /**
     * Compute distance on the earth's surface between two points
     * with the origin given by (lat_coord,lon_coord)
     **/
    dtorad = PI / 180.0 ;
    earthr = 6371.2 ;
    in   = 1.21 ;
    phi1 = lat_coord * dtorad ;
    phi2 = lat * dtorad ;
    dlam = (lon_coord - lon) * dtorad ;
    cost = sin(phi1) * sin(phi2) + cos(phi1) * cos(phi2) * cos(dlam) ;
    thet = acos(cost) ;
    radi = earthr * thet ;
    sina = cos(phi2) * sin(dlam) / sin(thet) ;
    alph = asin(sina) ;
    cosa = (sin(phi2) - cos(thet) * sin(phi1)) / (sin(thet) * cos(phi1)) ;

    if(cosa >= 0.0)
    {
        azmt = -asin(sina) / dtorad ;
    }
    else
    {
        azmt = -180.0 + asin(sina) / dtorad ;
    }

    arg  = azmt * dtorad ;
    disx = radi * sin(arg) ;
    disy = radi * cos(arg) ;

    /**
     * compute range to pixel
     **/
    range = sqrt(disx * disx + disy * disy) ;

    /**
     * compute slant range along path of radar beam
     **/
    range /= cos(0.45 * dtorad) ;

    /**
     * compute pixel height bases on nexrad equation for beam height
     * based on the lowest elevation of 0.45 degrees and an index of
     * of refraction of 1.21
     **/
    *pixelHeight = range * sin(0.45 * dtorad)
            + range * range / (2 * in * earthr) ;

    /**
     * convert to meters
     **/
    *pixelHeight *= 1000.0 ;

}
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

static void compute_height_data ( const char * path )
{
   static const char * filename = "mpe_radar_beam_height";
   char * pFilePath = NULL;
   static double radar_height [ NUM_DPA_ROWS ] [ NUM_DPA_COLS ];
   static const double lat = 37.76083333333333;
   static const double lon = 99.9688888888889;
   double col;
   double pixelHeight;
   double row;
   FILE * pFile = NULL;
   int filepath_len;
   int filename_len;
   int i;
   int intCol;
   int intRow;
   int is_slash = 0;
   int j;
   int path_len;
   int status;
   int tmpCol;
   int tmpRow;

   /* Build the file path. */
   filename_len = strlen ( filename );
   path_len = strlen ( path );

   if ( path [ path_len - 1 ] != '/' )
   {
      ++ path_len;
   }
   else
   {
      is_slash = 1;
   }

   filepath_len = path_len + filename_len + 1;

   pFilePath = (char *) malloc ( filepath_len * sizeof ( char ) );

   if (pFilePath == NULL)
   {
      fprintf ( stderr, "In routine 'compute_height_data': "
                        "dynamic memory allocation failure.\n" );
      return;
   }

   if ( is_slash == 1 )
   {
      sprintf ( pFilePath, "%s%s", path, filename );
   }
   else
   {
      sprintf ( pFilePath, "%s/%s", path, filename );
   }

   /* Attempt to open the file for writing. */
   pFile = fopen ( pFilePath, "w" );

   if ( pFile == NULL )
   {
      if ( pFilePath != NULL )
      {
         free ( pFilePath );
         pFilePath = NULL;
      }

      fprintf ( stderr, "In routine 'compute_height_data': "
                        "Could not open file %s.\n", pFilePath );
      return;
   }

   /* Convert the radar site lat/lon coords to HRAP coordinates. */
   LatLongToHrapByReference ( lat, lon, &row, &col );

   intCol = (int) col;
   intRow = (int) row;

   /* Loop over each HRAP grid bin in the DPA area.  Compute the height
      of the HRAP grid bin. */
   for ( i = 0; i < NUM_DPA_ROWS; ++i )
   {
      for ( j = 0; j < NUM_DPA_COLS; ++j )
      {
         tmpRow = intRow - 65 + i;
         tmpCol = intCol - 65 + j;

         /* Calculate the height of the HRAP grid bin. */
         calculatePixelHeight ( lon, lat, intCol, intRow, tmpCol, tmpRow,
                                & pixelHeight );

         radar_height [ i ] [ j ] = pixelHeight ;
      }
   }

   /* Write the radar file. */
   status = fwrite(radar_height, sizeof(double), NUM_DPA_ELEMENTS, pFile );

   if ( status != NUM_DPA_ELEMENTS )
   {
      fprintf ( stderr, "Error encountered writing to file %s.\n",
                pFilePath );
   }

   if ( pFilePath != NULL )
   {
      free ( pFilePath );
      pFilePath = NULL;
   }

   fclose ( pFile );
}


int create_mpe_beam_height_main ( int argc, const char ** argv )
{
   char * pPath = NULL;

   /* Retrieve the file path from the command line. */
   if ( argc != 2 )
   {
     fprintf ( stderr, "Usage:\n"
                       "  create_mpe_radar_height_file <height file path>\n" );
     return 1;
   }

   pPath = argv [ 1 ];
   compute_height_data ( pPath );

   return 0;
}
