/*************************************************************
  
   Functions to convert between lat-lon and Hrap bins.   
   
   ***********************************************************/

#include <math.h>
#include "convert_hrap.h"


void LatLongToHrapByReference(double lat,
		   double lon,
		   double *row,
		   double *col)
{
   
   double 	tlat, re;
   double	latrad, lonrad;
   double	r;
   double	x, y;
   
   tlat = STDLAT * RADIANS_PER_DEGREE;
   
   re = (EARTH_RADIUS * (1. + sin(tlat))) / MESH_LEN;
   latrad = lat * RADIANS_PER_DEGREE;
   lonrad = (lon + 180. - STDLON) * RADIANS_PER_DEGREE;
   
   r = re * cos(latrad) / (1. + sin(latrad));
   x = r  * sin(lonrad);
   y = r  * cos(lonrad);
   
   *col = x +  401.;
   *row = y + 1601.;
   
   return;
}


void HrapToLatLongByReference(double row,
		   double col,
		   double *lat,
		   double *lon)
{
   
   double	x, y;
   double	rr, gi;
   double	ang, tlat;
   
   tlat = 60.0 / DEGREES_PER_RADIAN;
   
   x = col -  401.;
   y = row - 1601.;
   
   rr = (x * x) + (y * y);
   gi = ((EARTH_RADIUS * (1 + sin(tlat)))/MESH_LEN);
   gi = gi * gi;
   
   *lat = asin((gi - rr) / (gi + rr)) * DEGREES_PER_RADIAN;
   
   ang = atan2(y, x) * DEGREES_PER_RADIAN;
   
   if (ang < 0)
      ang=ang + 360.;
   
   *lon = 270 + STDLON - ang;
   
   if (*lon < 0)
      *lon= *lon + 360.;
   
   if (*lon > 360)
      *lon= *lon - 360.;
   
   return ;
}

