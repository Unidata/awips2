/*=========================================================================*/
/*  FILE PATH/NAME:  mpe_util/hrap.c                                       */
/*  FUNCTIONS CONTAINED IN THIS FILE:   HrapToLatLongMpe                   */
/*                                      LatLongToHrapMpe                   */
/*  May 2003 - converted to use computed constants where possible          */
/*=========================================================================*/

#include <stdio.h>
#include <math.h>

#include "stage3.h"

/* constants */

#define STDLON  105.0  
#define RADDEG   57.2957795   
#define DEGRAD    0.017453293

/* computed constants
   RE = ((earthrad * (1 + sin(tlat))) / mesh_len) = 2496.3404    
   GI_SQUARED = ((earthrad * (1 + sin(tlat))) / mesh_len) = 2496.3404 */

#define RE             2496.3404
#define GI_SQUARED  6231715.29

/* description of notable constants:
   stdlat    - float; standard latitude  = 60. 
   STDLON    - float; standard longitude = 105. 
   earthrad  - float; earth's radius (km) = 6371.2
   mesh_len  - float; mesh length at stdlat (60 deg North) = 4.7625
   RADDEG    - float; conversion from radians to degrees = 57.29577951
   DEGRAD    - float; conversion degrees to radians = 0.017453293;
   tlat      - float; standard latitude in radians = stdlat/RADDEG
   */


/***************************************************************************/
/*  FUNCTION NAME:   HrapToLatLongMpe                                      */
/*       FUNCTION:   convert from HRAP coordinates to latitude-longitude   */
/***************************************************************************/

HRAP HrapToLatLongMpe (point hrap)
{      
   /* local variables */
      
   double x, y, rr, ang;
   HRAP  ll;
      
   
   /* apply offsets */
   
   x = (double )hrap.x - 401.;
   y = (double )hrap.y - 1601.;
   
   rr = x*x + y*y;
   
   
   /* determine the y coordinate */
   
   ll.y = asin((GI_SQUARED - rr) / (GI_SQUARED + rr)) * RADDEG;
   
   
   /* determine the x coordinate */
   
   ang = atan2(y, x) * RADDEG;
   if (ang < 0) ang = ang + 360.;
   
   ll.x = 270 + STDLON - ang;
   
   
   /* adjust if needed */
   
   if (ll.x < 0) 
      ll.x = ll.x + 360;
   
   else if (ll.x > 360) 
      ll.x = ll.x - 360;
   
   
   /* return with the resultant structure */
   
   return ll;
}


/***************************************************************************/
/*  FUNCTION NAME:   LatLongToHrapMpe                                      */
/*       FUNCTION:   convert latitude-longitude coordinates to DV point    */
/*                    structure at appropriate screen position             */
/***************************************************************************/

HRAP LatLongToHrapMpe (float lat, float lon)
{  
   /* local vars */
   
   double latrad, lonrad, r, x, y;
   HRAP   hrap;
   
   /* define as radians */
   
   latrad = lat * DEGRAD;
   lonrad = (lon + 180. - STDLON) * DEGRAD;
   
   
   /* define x and y */
   
   r = RE * cos(latrad) / (1. + sin(latrad));
   x = r * sin(lonrad);
   y = r * cos(lonrad);
   
  
   /* offset */
   
   hrap.x = x + 401;
   hrap.y = y + 1601;
   
   
   /* return with the resultant structure */
   
   return hrap;   
}

