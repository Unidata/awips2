/*=========================================================================*/
/*                    FILE PATH/NAME:  STAGE3_SOURCE/hrap.c                */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   HrapToLatLong                      */
/*                                      LatLongToHrap                      */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <math.h>
#include "stage3.h"

/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/hrap.c                                   */
/*  FUNCTION NAME:   HrapToLatLong                                         */
/*       FUNCTION:   convert from HRAP coordinates to latitude-longitude   */
/***************************************************************************

Function type:
   HRAP structure

Called by function:
   (callback) locate

Functions called:
   none

Local variables:
   tlat - float; standard latitude in radians
   earthr - float; earth's radius (km)
   xmesh - float; mesh length at 60 deg North
   raddeg - float; conversion from radians to degrees
   stlon - float; standard longitude
   ll - HRAP structure; latitude/longitude

******************************************** BEGIN HrapToLatLong ***********/

HRAP HrapToLatLongPproc(hrap)
   point hrap;
{
   double x, y, rr, gi, ang;
   double tlat, earthr, xmesh, raddeg, stlon;
   HRAP  ll;

 earthr=6371.2;
 stlon=105.;
 raddeg=57.29577951;
 xmesh=4.7625;
 tlat=60./raddeg;

 x = (double)hrap.x - 401.;
 y = (double)hrap.y - 1601.;
 rr = x*x + y*y;
 gi = ((earthr * (1+sin(tlat)))/xmesh);
 gi=gi*gi;
 ll.y = asin((gi-rr)/(gi+rr))*raddeg;
 ang = atan2(y,x)*raddeg;
 if (ang < 0) ang=ang+360.;
 ll.x = 270+stlon-ang;
 if (ll.x < 0) ll.x=ll.x+360;
 if (ll.x > 360) ll.x=ll.x-360;
 return ll;
}

/********************************************* END HrapToLatLong ***********/



/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/hrap.c                                   */
/*  FUNCTION NAME:   LatLongToHrap                                         */
/*       FUNCTION:   convert latitude-longitude coordinates to DV point    */
/*                    structure at appropriate screen position             */
/***************************************************************************

Function type:
   HRAP structure

Called by function:

Functions called:
   none

Local variables:
   lat - float; latitude
   lon - float; longitude
   degrad - float; conversion degrees to radians
   earthrad - float; radius of earth
   stdlat - float; standard latitude
   stdlon - float; standard longitude
   mesh_len - float; grid mesh length at standard latitude
   tlat - float; standard latitude in radians
   latrad - float; point latitude in radians
   lonrad - float; point longitude in radians
   hrap - HRAP structure; hrap coordinate

******************************************** BEGIN LatLongToHrap ***********/

HRAP LatLongToHrapPproc(lat, lon)
   float  lat, lon;
{
   double  degrad = 0.017453293;
   double  earthrad = 6371.2;
   double  stdlat = 60.;
   double  stdlon = 105.;
   double  mesh_len = 4.7625;
   double  tlat, re, latrad, lonrad, r, x, y;
   HRAP   hrap;

 tlat = stdlat*degrad;
 re = (earthrad*(1. + sin(tlat)))/mesh_len;
 latrad = lat * degrad;
 lonrad = (lon + 180. - stdlon) * degrad;
 r = re * cos(latrad) / (1. + sin(latrad));
 x = r * sin(lonrad);
 y = r * cos(lonrad);
 hrap.x = x + 401;
 hrap.y = y + 1601;
 return hrap;


}

/********************************************* END LatLongToHrap ***********/
