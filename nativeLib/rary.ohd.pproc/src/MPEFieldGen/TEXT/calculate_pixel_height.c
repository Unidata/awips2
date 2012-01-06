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

#include <math.h>
#include "convert_hrap.h"
#define PI 3.141592654

void MPEFieldGen_calculatePixelHeight(const double lon_coord,
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
