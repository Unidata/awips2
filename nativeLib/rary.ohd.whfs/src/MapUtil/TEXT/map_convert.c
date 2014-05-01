/*****************************************************************************
 *
 * Module: map_convert.c
 *
 * Description: contains routines to convert map coordinates
 *
 *****************************************************************************/

/* include files */

/* C includes */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>

#include "convert_hrap.h"
#include "GeneralUtil.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_library.h"
#include "map.h"

/* global variables */

float zoom_multiple = 1.0 ;
float original_zoom_multiple = 1.0 ;
long orig_scale_factor;
long init_scale_factor;
long map_scale_factor;
static int MAXX = 0 ;
static int MAXY = 0 ;
int offsetx,offsety;
int polar_offsety = 0 ;   /* Used for keeping track of the y-offset of the
                             HRAP and Polar Stereographic map projection. */
int polar_offsetx = 0 ;   /* Used for keeping track of the x-offset of the
                             HRAP projection. */
int projection=M_FLAT;
int prev_projection=M_FLAT;
static int XOR = 0 ;
static int YOR = 0 ;
int zoom_scale = 1;

float map_center_lat=33.0;
float map_center_lon=-114.0;
float original_center_lat ;
float original_center_lon ;
float top_lat,left_lon;
float width_in_nmi = 0.0 ;
float dtr = 0.017453292519943295 ;
float init_top_lat , init_width ;
int init_offsetx , init_zoom ;
float region_llat = -999.0 ,
      region_llon = -999.0 ,
      region_rlat = -999.0 ,
      region_rlon = -999.0 ;

static MotifCallback pan_routine = NULL ;
static MotifCallback zoom_routine = NULL ;

rubber_band_data rbdata ;

/*****************************************************************************
 *
 * Routine: mSetRegion
 *
 * Description: This routine defines the region that the user wishes to see 
 *              overlays displayed for.
 *
 ****************************************************************************/

void mSetRegionArea(float llat,float llon,float rlat,float rlon)
{
  region_llat = llat;
  region_llon = llon;
  region_rlat = rlat;
  region_rlon = rlon;
}

/*****************************************************************************
 *
 * Routine: mSetHrapArea
 *
 * Description: This routine defines the hrap region that is required
 *              by the HRAP map projection.
 *
 ****************************************************************************/
void mSetHrapArea ( int Hrap_XOR , int Hrap_YOR , int Hrap_MAXX , 
                    int Hrap_MAXY )
{
   XOR = Hrap_XOR ;
   YOR = Hrap_YOR ;
   MAXX = Hrap_MAXX ;
   MAXY = Hrap_MAXY ;
}

/*****************************************************************************
 *
 * Routine: _get_region
 *
 * Description: this routine returns the corners of the user define region.
 *
 ****************************************************************************/

void _get_region(float *llat,float *llon,float *rlat,float *rlon)
{
  *llat = region_llat;
  *llon = region_llon;
  *rlat = region_rlat;
  *rlon = region_rlon;
}

/*****************************************************************************
 *
 * Routine: _get_offsetx
 *
 * Description: this routine returns the x offset
 *
 ****************************************************************************/

int _get_offsetx()
{
  return(offsetx);
}

/*****************************************************************************
 *
 * Routine: _get_offsety
 *
 * Description: this routine returns the y offset
 *
 ****************************************************************************/

int _get_offsety()
{
  return(offsety);
}

/*****************************************************************************
 *
 * Routine: mGetCenterLatLon
 *
 * Description: this routine returns the center latitude longitude
 *
 ****************************************************************************/

void mGetCenterLatLon(float *lat,float *lon)
{
  *lat = map_center_lat;
  *lon = map_center_lon;
}

/*****************************************************************************
 *
 * Routine: mGetUpperLeftLatLon
 *
 * Description: this routine returns the upper left latitude longitude
 *
 ****************************************************************************/
void mGetUpperLeftLatLon(float *lat,float *lon)
{
  *lat = top_lat;
  *lon = left_lon;
}

/*****************************************************************************
 *
 * Routine: _get_center_lon
 *
 * Description: this routine returns the center longitude
 *
 ****************************************************************************/

float _get_center_lon()
{
  return(map_center_lon);
}

/*****************************************************************************
 *
 * Routine: _get_center_lat
 *
 * Description: this routine returns the center latitude
 *
 ****************************************************************************/

float _get_center_lat()
{
  return(map_center_lat);
}

/*****************************************************************************
 *
 * Routine: _get_left_lon
 *
 * Description: this routine returns the left edge longitude
 *
 ****************************************************************************/

float _get_left_lon()
{
  return(left_lon);
}

/*****************************************************************************
 *
 * Routine: _get_top_lat
 *
 * Description: this routine returns the top edge latitude
 *
 ****************************************************************************/

float _get_top_lat()
{
  return(top_lat);
}

/*****************************************************************************
 *
 * Routine: _get_map_projection
 *
 * Description: this routine returns the map projection
 *
 ****************************************************************************/

int _get_map_projection()
{
  return(projection);
}

/*****************************************************************************
 *
 * Routine: _convert_latlon_xy_hrap
 *
 * Description: this routine converts lat lon coordinates to x y coordinates
 *
 ****************************************************************************/

void _convert_latlon_xy_hrap(float lat,float lon,int *x,int *y)
{
  float k,temp,center;

  lat = lat * dtr;
  lon = lon * dtr;
  center = HRAP_REFERENCE_LON * dtr ;
  temp =  1 + sin(lat);
  if (temp == 0.0)
  {
    printf("Error\n");
  }
  else
  {
     k = ( float ) map_scale_factor;

     temp = 2.0 * k * tan((3.14159 /4.0) - (lat / 2.0)) * sin(lon - center);
     *x = (int)temp + offsetx;

     temp = -2.0 * k * tan((3.14159 /4.0) - (lat /2.0)) * cos(lon - center);
     *y = offsety - (int)temp;
  }
}

/*****************************************************************************
 *
 * Routine: _convert_latlon_xy_polar
 *
 * Description: this routine converts lat lon coordinates to x y coordinates
 *
 ****************************************************************************/

void _convert_latlon_xy_polar(float lat,float lon,int *x,int *y)
{
  float k,temp,center;

  lat = lat * dtr;
  lon = lon * dtr;
  center = map_center_lon * dtr;
  temp =  1 + sin(lat);
  if (temp == 0.0)
    printf("Error\n");
  else{
    k = map_scale_factor;

    temp = 2.0 * k * tan((3.14159 /4.0) - (lat / 2.0)) * sin(lon - center);
    *x = (int)temp + offsetx;

    temp = -2.0 * k * tan((3.14159 /4.0) - (lat /2.0)) * cos(lon - center);
    *y = offsety - (int)temp;
  }
}

/*****************************************************************************
 *
 * Routine: _alter_projection_settings
 *
 * Description:  This routine does any projection specific modifications
 *               that may be necessary when panning or zooming.
 ****************************************************************************/

static void _alter_projection_settings ( )
{
  int new_x ;
  int new_y ;

  if ( projection == M_POLAR )
  {
     _convert_latlon_xy_polar ( map_center_lat , map_center_lon , & new_x ,
                                & new_y ) ;
     polar_offsety = offsety - new_y ;
  }
  else if ( projection == M_HRAP )
  {
     _convert_latlon_xy_hrap ( map_center_lat , map_center_lon ,
                               & new_x , & new_y ) ;
     polar_offsety = offsety - new_y ;
     polar_offsetx = offsetx - new_x ;
  }

}

/*****************************************************************************
 *
 * Routine: mSetCenterLatLon
 *
 * Description: this routine sets the center lat lon coordinate for the maps
 *
 ****************************************************************************/

void mSetCenterLatLon ( float lat , float lon )
{
  static int first = 1 ;

  if ( first == 1 )
  {
     original_center_lat = lat ;
     original_center_lon = lon ;
     first = 0 ;
  }

  map_center_lat = lat ;
  map_center_lon = lon ;
  _set_make_map ( M_ON ) ;

  /* Make certain that the top latitude and left longitude are changed
     whenever the center latitude and longitude are changed. */
  mConvertXY2LatLon ( 0 , 0 , & top_lat , & left_lon ) ;

  /* If this is the polar stereographic projection, determine what
     the polar x and y offsets are.  By default, the polar stereo graphic
     projection displays with the north pole in the middle of the viewer. */
  _alter_projection_settings ( ) ;
}

/******************************************************************************
 *
 * Routine: _pan_map_up
 * 
 * Description: this routine moves the map up 
 *
 *****************************************************************************/

void _pan_map_up ( Widget wid , XtPointer client_data , XtPointer call_data )
{
  int i ;
  float temp , q ;

  /* Set up the CTRL-Z Hot Key so that it will return the user to
     the initial view of the map and any previously drawn rectangles
     are ignored. */
  rbdata.zoom_state = True ;
  rbdata.use_rectangle = False ;

  q = ((top_lat - map_center_lat) / 2);
  temp = top_lat + q;
  map_center_lat = map_center_lat + q;
  top_lat = temp;

  if (top_lat > 90.0)
  {
    temp = top_lat - 90.0;
    top_lat = 90.0;
    map_center_lat = map_center_lat - temp;
  }

  _alter_projection_settings ( ) ;

  /* Perform any user-supplied functionality associated with 
     panning the maps. */
  if ( pan_routine != NULL )
  {
     pan_routine ( wid , client_data , call_data ) ;
  }

  _set_make_map ( M_ON ) ;

  for ( i = 0 ; i < _get_num_of_maps ( ) ; ++ i )
  {
     _set_map_change ( i ) ;
     mUpdateMap ( i ) ;
  }

}


/******************************************************************************
 *
 * Routine: _pan_map_down
 * 
 * Description: this routine moves the map down
 *
 *****************************************************************************/

void _pan_map_down(Widget wid,XtPointer client_data,XtPointer call_data)
{
  int i ;
  float temp , q ;

  /* Set up the CTRL-Z Hot Key so that it will return the user to
     the initial view of the map and any previously drawn rectangles
     are ignored. */
  rbdata.zoom_state = True ;
  rbdata.use_rectangle = False ;

  q = ((top_lat - map_center_lat) / 2);
  temp = map_center_lat - q;
  map_center_lat = temp;
  top_lat = top_lat - q;

  _alter_projection_settings ( ) ;

  /* Perform any user-supplied functionality associated with panning the
     map down. */
  if ( pan_routine != NULL )
  {
     pan_routine ( wid , client_data , call_data ) ;
  }

  _set_make_map(M_ON);

  for(i=0;i < _get_num_of_maps(); i++)
  {
    _set_map_change(i);
    mUpdateMap(i);
  }
}


/******************************************************************************
 *
 * Routine: _pan_map_left
 * 
 * Description: this routine moves the map left
 *
 *****************************************************************************/

void _pan_map_left(Widget wid,XtPointer client_data,XtPointer call_data)
{
  int i;
  float temp,q;

  /* Set up the CTRL-Z Hot Key so that it will return the user to
     the initial view of the map and any previously drawn rectangles
     are ignored. */
  rbdata.zoom_state = True ;
  rbdata.use_rectangle = False ;

  if ((left_lon < 0.0 && map_center_lon < 0.0) ||
      (left_lon >= 0.0 && map_center_lon >= 0.0)){
    q = fabs((left_lon - map_center_lon)) / 2.0;
  }
  else if (left_lon < 0.0){
    temp = 360.0 + left_lon;
    q = (temp - map_center_lon) / 2.0;
  }
  else{
    temp = 360.0 + map_center_lon;
    q = (temp - left_lon) / 2.0;
  }

  temp = map_center_lon - q;

  if (temp < -180.0 )
    temp = 180 + (temp + 180.0);

  if (temp > 180)
    temp = (temp - 180.0) - 180.0;
 
  map_center_lon = temp;
  left_lon = left_lon - q;
  if (left_lon < -180.0 )
    left_lon = 180 +(left_lon + 180.0);

  if (left_lon > 180)
    left_lon = (left_lon - 180.0) - 180.0;

  _alter_projection_settings ( ) ;

  /* Perform any user-supplied functionality associated with panning
     the map left. */
  if ( pan_routine != NULL )
  {
     pan_routine ( wid , client_data , call_data ) ;
  }

  _set_make_map(M_ON);

  for ( i = 0 ; i < _get_num_of_maps ( ) ; i++ )
  {
    _set_map_change ( i ) ;
    mUpdateMap ( i ) ;
  }

}


/******************************************************************************
 *
 * Routine: _pan_map_right
 * 
 * Description: this routine moves the map right
 *
 *****************************************************************************/

void _pan_map_right(Widget wid,XtPointer client_data,XtPointer call_data)
{
  int i ;
  float temp , q ;

  /* Set up the CTRL-Z Hot Key so that it will return the user to
     the initial view of the map and any previously drawn rectangles
     are ignored. */
  rbdata.zoom_state = True ;
  rbdata.use_rectangle = False ;

  if ((left_lon < 0.0 && map_center_lon < 0.0) ||
      (left_lon >= 0.0 && map_center_lon >= 0.0)){
    q = fabs((left_lon - map_center_lon)) / 2;
  }
  else if (left_lon < 0.0){
    temp = 360.0 + left_lon;
    q = (temp - map_center_lon) / 2;
  }
  else{
    temp = 360.0 + map_center_lon;
    q = (temp - left_lon) / 2;
  }

  temp = map_center_lon + q;
  if (temp < -180.0 )
    temp = temp + 360.0;

  if (temp > 180.0)
    temp = temp - 360.0;
 
  map_center_lon = temp;
  left_lon = left_lon + q;
  if (left_lon < -180.0 )
    left_lon = 360.0 + left_lon;

  if (left_lon > 180.0)
    left_lon = left_lon - 360.0;

  _alter_projection_settings ( ) ;

  /* Perform any user-supplied functionality associated with panning the
     map to the right. */
  if ( pan_routine != NULL )
  {
     pan_routine ( wid , client_data , call_data ) ;
  }

  _set_make_map ( M_ON ) ;

  for ( i = 0 ; i < _get_num_of_maps ( ) ; i++ )
  {
     _set_map_change ( i ) ;
     mUpdateMap ( i ) ;
  }

}

/*****************************************************************************
 *
 * Routine: _compute_width_in_nmi()
 *
 * Description: this routine computes the width in nautical miles.
 *
 ****************************************************************************/

void _compute_width_in_nmi()
{
  float degs,clon,llon;

  if (map_center_lon < 0)
    clon = 360 + map_center_lon;
  else
    clon = map_center_lon;

  if (left_lon < 0)
    llon = 360 + left_lon;
  else
    llon = left_lon;

  degs = 2.0 * (clon - llon);
  width_in_nmi = ( degs * 60.0 ) * cos ( map_center_lat * dtr ) ;

}

/*****************************************************************************
 *
 * Routine: _compute_flat_scale()
 *
 * Description: this routine computes the scale factor for flat projection
 *
 ****************************************************************************/

void _compute_flat_scale()
{
  long scale ;
  float half ;

  half = ( width_in_nmi / 2.0 ) / ( 60.0 * cos ( map_center_lat * dtr ) ) ;

  scale = ( long ) abs ( offsetx / half ) ;

  map_scale_factor = scale * zoom_scale ;
}

/*****************************************************************************
 *
 * Routine: _compute_lambart_scale()
 *
 * Description: this routine computes the scale factor for lambart projection
 *
 ****************************************************************************/

void _compute_lambart_scale()
{
  double dellat,dellon,lat,llon,temp1,temp2,temp;
  int x,y;

  x = 0;
  y = offsety;

  llon = (-1.0 * map_center_lon)  - (((width_in_nmi / 2) / 60.0) * cos (map_center_lat * dtr)); 
  dellat = (0 / 2) * dtr;
  dellon = llon -  (-1.0 * map_center_lon);
  dellon = (dellon/2) * dtr;

  temp1 = sin(dellat) * sin(dellat);
  temp2 = sin(dellon) * sin(dellon);

  lat = map_center_lat * dtr;
  temp = temp1 + (cos(lat) * cos(lat) * temp2);
  temp1 = sqrt((x*x) + (y*y));
  temp2 = 2 * sqrt(temp);

  temp = temp1/temp2; 
  map_scale_factor = (long)(temp);
  map_scale_factor = (map_scale_factor * 2) * zoom_scale;


  /* not sure if the calculation for scale factor is right */

  map_scale_factor = 350 * zoom_scale;
}

/*****************************************************************************
 *
 * Routine: _compute_mercator_scale()
 *
 * Description: this routine computes the scale factor for mercator projection
 *
 ****************************************************************************/
void _compute_mercator_scale()
{
    _compute_flat_scale();
    map_scale_factor = (map_scale_factor * 50) * zoom_scale;
}


static void find_best_hrap_scale ( )
{
   double lat ;
   double lat1 ;
   double lon ;
   double lon1 ;

   int height ;
   int width ;

   height = _get_map_height ( 0 ) ; /* Get map height. */
   width = _get_map_width ( 0 ) ; /* Get map height */

   /* Get the lat lon coordinates for the lower left and upper right
      corners of the MPE area. */
   HrapToLatLongByReference ( ( double ) YOR ,
                   ( double ) XOR ,
                   & lat ,
                   & lon ) ;
   lon *= -1 ;

   HrapToLatLongByReference ( ( double ) YOR + MAXY ,
                   ( double ) XOR + MAXX ,
                   & lat1 ,
                   & lon1 ) ;
   lon1 *= -1 ;

   mOptimizeScaleFactor ( init_scale_factor , lat , lon , lat1, lon1 ,
                           height , width , 'R' ) ;
}

/*****************************************************************************
 *
 * Routine: _compute_scale_factor()
 *
 * Description: this routine computes the scale factor for the  projection
 *
 ****************************************************************************/

void _compute_scale_factor()
{
  if (projection == M_LAMBART){
     _compute_lambart_scale();
  }
  else if (projection == M_FLAT){
    _compute_flat_scale();
  }
  else if (projection == M_MERCATOR){
    _compute_mercator_scale();
  }
  else{
    map_scale_factor = 300;
  }
}

/*****************************************************************************
 *
 * Routine: _set_projection_scale ( )
 *
 * Description: this routine computes the scale factor for the  projection
 *
 ****************************************************************************/

void _set_projection_scale ( )
{
   if ( projection == M_FLAT )
   {
      init_scale_factor = orig_scale_factor ;
      map_scale_factor = init_scale_factor * zoom_multiple ;
   }
   else if ( projection == M_POLAR )
   {
      init_scale_factor = orig_scale_factor * POLAR_SCALE_MULTIPLE ;
      map_scale_factor = init_scale_factor * zoom_multiple;
   }
   else if ( projection == M_HRAP )
   {
      init_scale_factor = orig_scale_factor * POLAR_SCALE_MULTIPLE ;
      map_scale_factor = init_scale_factor * zoom_multiple ;

      /* Optimize the init_scale_factor scale so that the MPE area
         fits well into the viewing area. */ 
      find_best_hrap_scale ( ) ;
   }
}

/*****************************************************************************
 *
 * Routine: _set_map_projection
 *
 * Description: this routine sets the map projection
 *
 ****************************************************************************/

void  _set_map_projection ( int proj )
{
  Boolean ReturnInitialDate = True ;
  double latitude ;
  double longitude ;

  int new_x ;
  int new_y ;

  Widget map_widget ;

  if ( prev_projection == proj && prev_projection != M_HRAP )
  {
     return ;
  }

  /* Turn off any previous areal zooms. */
  rbdata.zoom_state = False ;
  rbdata.use_rectangle = False ;
  zoom_multiple = 1.0 ;

  projection = proj;

  _set_make_map(M_ON);

  _set_projection_scale ( ) ;

  if ( projection == M_POLAR )
  {
     _alter_projection_settings ( ) ;
  }

  if ( projection == M_HRAP )
  {
     /* Attempt to size the window to fit the office's MPE area.
        Get the latitude/longitude of the XOR, YOR HRAP
        coordinates representing the origin of the local MPE area grid. */ 
     HrapToLatLongByReference ( ( double ) YOR + MAXY ,
                     ( double ) XOR ,
                     & latitude ,
                     & longitude ) ;
     longitude *= -1 ;
     _convert_latlon_xy_hrap ( ( float ) latitude , 
                               ( float ) longitude , 
                               & new_x , 
                               & new_y ) ; 
     polar_offsety = -1 * new_y ;
     polar_offsetx = -1 * new_x ;
  }

  prev_projection = projection ;

  /* Run any user specific functionality associated with the zooming in
     of the map. */
  if ( zoom_routine != NULL )
  {
     map_widget = _get_map_widget ( 0 ) ;
     zoom_routine ( map_widget , ( XtPointer ) & ReturnInitialDate , NULL ) ;
  }

}

/*****************************************************************************
 *
 * Routine: _convert_latlon_xy_mercator
 *
 * Description: this routine converts lat lon coordinates to x y coordinates
 *
 ****************************************************************************/

void _convert_latlon_xy_mercator(float lat,float lon,int *x,int *y)
{
  double temp,dellon,c;

  if (map_center_lon < 0)
    c = 360 + map_center_lon;
  else
    c = map_center_lon;

  if (lon < 0 )
    lon = 360 + lon;

  dellon = (lon - c) * dtr;

  *x = (int)(map_scale_factor * dellon) + offsetx;

  if (lat >= 90.0)
    y = 0;
  else{
    lat = (lat - map_center_lat) * dtr;
    temp = log((1 + sin(lat))/(1 - sin(lat)));
    *y = (int)((map_scale_factor / 2.0) * temp);
    *y = offsety - *y;
  }
}

/*****************************************************************************
 *
 * Routine: _convert_latlon_xy_lambart
 *
 * Description: this routine converts lat lon coordinates to x y coordinates
 *
 ****************************************************************************/

void _convert_latlon_xy_lambart(float lat,float lon,int *x,int *y)
{
  double dellon,temp1,temp2,temp,lato;


  dellon = lon -  map_center_lon;
  dellon = dellon * dtr;

  lat = (-1.0 * lat) * dtr;
  lato = (-1.0 * map_center_lat) * dtr;

  temp1 = cos(lat) * sin(dellon);
  temp2 = 1 + (sin(lato) * sin(lat)) + (cos(lato) * cos(lat) * cos(dellon));
  temp2 = sqrt(temp2);
  temp = map_scale_factor * sqrt(2.0) * (temp1/temp2);
  *x = ((int) temp) + offsetx;

  temp1 = (cos(lato) * sin(lat)) - (sin(lato) * cos(lat) * cos(dellon));
  temp = map_scale_factor * sqrt(2.0) * (temp1/temp2);
  *y = (int)temp + offsety;
  
}

/*****************************************************************************
 *
 * Routine: mCovertLatLon2XY
 *
 * Description: this routine converts lat lon coordinates to x y coordinates
 *
 ****************************************************************************/

inline void mConvertLatLon2XY(float lat,float lon,int *x,int *y)
{
  offsetx = _get_map_width(0) / 2;
  offsety = _get_map_height(0) / 2;

  if (projection == M_FLAT){
    if (map_center_lon > 0 && lon < 0)
      lon = lon + 360;
    
    if (map_center_lon < 0 && lon > 0)
      lon = lon - 360;
    
    *x = ((lon - map_center_lon) * map_scale_factor) + offsetx;
    *y = offsety - ((lat - map_center_lat) * map_scale_factor);
  }
  else if (projection == M_LAMBART){
    _convert_latlon_xy_lambart(lat,lon,x,y);
  }
  else if (projection == M_MERCATOR){
    _convert_latlon_xy_mercator(lat,lon,x,y);
  }
  else if (projection == M_POLAR){
    _convert_latlon_xy_polar(lat,lon,x,y);
   
    /* Shift the polar projection up or down to compensate for recentering. */
    *y = *y + polar_offsety ;
  }
  else if (projection == M_HRAP ){
    _convert_latlon_xy_hrap(lat,lon,x,y);
    *y = *y + polar_offsety ;
    *x = *x + polar_offsetx ;

  }
  else
    printf("Error: Invalid projection of %d\n",projection);
}

/*****************************************************************************
 *
 * Routine: _convert_xy_latlon_polar
 *
 * Description: this routine converts x y coordinates to lat lon coordinates
 *
 ****************************************************************************/

void _convert_xy_latlon_polar(int x,int y,float *lat,float *lon)
{
  float c;
  double rtd,py,px,p;

  rtd = 180.0 / 3.14159; 

  /* Be sure to take into account any panning/recentering effects. */
  x = x - offsetx;
  y = (-1 * y) + offsety;

  py = (double)y;
  px = (double)x;

  *lon = ((map_center_lon * dtr) + atan2(py,px)) * rtd + 90.0;

  p = pow(((px*px) + (py*py)),0.5);
  c = 2.0 * atan(p/(2.0 * ( double ) map_scale_factor));
  *lat = asin(cos(c) * sin((90.0 *dtr)) + (py * sin(c) * cos((90.0 * dtr))/p)) * rtd;
}

/*****************************************************************************
 *
 * Routine: _convert_xy_latlon_hrap
 *
 * Description: this routine converts x y coordinates to lat lon coordinates
 *
 ****************************************************************************/

void _convert_xy_latlon_hrap(int x,int y,float *lat,float *lon)
{
  float c;
  double rtd,py,px,p;

  rtd = 180.0 / 3.14159; 

  /* Be sure to take into account any panning/recentering effects. */
  x = x - offsetx;
  y = (-1 * y) + offsety;

  py = ( double ) y;
  px = ( double ) x;

  *lon = (( HRAP_REFERENCE_LON * dtr ) + atan2(py,px)) * rtd + 90.0;

  p = pow(((px*px) + (py*py)),0.5);
  c = 2.0 * atan(p/(2.0 * map_scale_factor));
  *lat = asin(cos(c) * sin((90.0 *dtr)) + (py * sin(c) * cos((90.0 * dtr))/p)) * rtd;
}

/*****************************************************************************
 *
 * Routine: _convert_xy_latlon_mercator
 *
 * Description: this routine converts x y coordinates to lat lon coordinates
 *
 ****************************************************************************/

void _convert_xy_latlon_mercator(int x, int y,float *lat,float *lon)
{
  double temp,temp1,rtd,p,s;

  rtd = 180.0 / 3.14159; 
  x = x - offsetx;
  y = y - offsety;
  s = (double)map_scale_factor;
  p = (double)y;

  temp = map_center_lat * dtr;
  temp1 = temp - atan(sinh(p / s));
  *lat = temp1 * rtd;

  p = (double)x;
  temp = p / s;
  temp1 = temp + (dtr * map_center_lon);
  *lon = temp1 * rtd;
}

/*****************************************************************************
 *
 * Routine: _convert_xy_latlon_lambart
 *
 * Description: this routine converts x y coordinates to lat lon coordinates
 *
 ****************************************************************************/

void _convert_xy_latlon_lambart(int x,int y,float *lat,float *lon)
{
  double temp1,temp,temp2,temp3,rho,c;

  x = x - offsetx;
  y = y - offsety;

  temp = (-1.0 * map_center_lat) * dtr;

  rho = (double) sqrt((double) ((x*x) + (y*y)));
  temp1 = rho/(2.0 * map_scale_factor); 
  c = 2.0 * asin(temp1);

  *lat = (float) asin((cos(c) * sin(temp)) + (y * sin(c) * cos(temp)/rho));

  temp1 = map_center_lon * dtr;

  if ((*lat < 89.9) && (*lat > -89.9)){
    temp3 = x *sin(c);
    temp2 = atan2(temp3,((rho *cos(temp)*cos(c)) - (y * sin(temp)*sin(c))));
    if (temp3 < 0)
      temp2 = (2 * 3.14159) + temp2;

    *lon = (float) temp1 + temp2;
  }

  *lon = *lon * 180.0 / 3.14159;
  *lat = (-1.0 * (*lat)) * 180.0 / 3.14159;
}

/*****************************************************************************
 *
 * Routine: mConvertXY2LatLon
 *
 * Description: this routine converts x y coordinates to lat lon coordinates
 *
 ****************************************************************************/

void mConvertXY2LatLon ( int x , int y , float *lat , float *lon )
{
  offsetx = _get_map_width ( 0 ) / 2 ;
  offsety = _get_map_height ( 0 ) / 2 ;

  if (projection == M_FLAT){
    *lon = ( ( ( float ) (x - offsetx) / ( float ) map_scale_factor )
           + ( float ) map_center_lon) ;
    *lat = ( float ) map_center_lat - ( ( float ) ( y - offsety ) /
                              ( float ) map_scale_factor ) ;
  }
  else if ( projection == M_LAMBART ) {
    _convert_xy_latlon_lambart(x,y,lat,lon);
  }
  else if ( projection == M_MERCATOR ) {
    _convert_xy_latlon_mercator(x,y,lat,lon);
  }
  else if ( projection == M_POLAR ){
    y -= polar_offsety ;
    _convert_xy_latlon_polar(x,y,lat,lon);
  }
  else if ( projection == M_HRAP ){
    y -= polar_offsety ;
    x -= polar_offsetx ;
    _convert_xy_latlon_hrap(x,y,lat,lon);
  }
  else{
    printf("Error: Invalid projection of %d\n",projection);
    return;
  }

  if (*lon < -180.0){
    *lon = *lon + 360;
  }

  if (*lon > 180){
    *lon = *lon - 360;
  }
}

/*****************************************************************************
 *
 * Routine: mSetMapProjection
 *
 * Description: this routine sets the map projection
 *
 ****************************************************************************/

void  mSetMapProjection(int proj)
{
  _set_map_projection(proj);
}
/*****************************************************************************
 *
 * Routine: mGetMapProjection
 *
 * Description: this routine returns the map projection
 *
 ****************************************************************************/

int mGetMapProjection()
{
  return(projection);
}
/*****************************************************************************
 *
 * Routine: mSetWidthInNmi
 *
 * Description: this routine sets the width_in_nmi variable
 *
 ****************************************************************************/

void  mSetWidthInNmi(float width)
{
  width_in_nmi = width;
  _compute_scale_factor();
  _set_make_map(M_ON);
}

/*****************************************************************************
 *
 * Routine: mGetWidthInNmi
 *
 * Description: this routine returns the width in nautical miles of the 
 *              map. 
 *
 ****************************************************************************/
float mGetWidthInNmi ( )
{
   _compute_width_in_nmi ( ) ;
   return width_in_nmi ;
}

/*****************************************************************************
 *
 * Routine: _init_map_variables
 *
 * Description: this routine sets the map projection
 *
 ****************************************************************************/

void _init_map_variables ( float nmi )
{
  float degrees_lon ;
  projection = M_FLAT ;
  width_in_nmi = nmi ;

  offsetx = _get_map_width ( 0 ) / 2 ;
  offsety = _get_map_height ( 0 ) / 2 ;

  /* New logic added by Bryon Lawrence on January 15, 2002.  This is an
     attempt to incorporate an initial measurement in nautical miles into
     the initial computation of the scale factor. */
  degrees_lon = width_in_nmi / 
              ( ( float ) NUM_NMI_PER_DEGREE * cos ( map_center_lat * dtr ) ) ;
  map_scale_factor = ( long ) ( 2.0 * ( float ) offsetx / degrees_lon ) ;

  /* Compute the top latitude and the left longitude. */
  mConvertXY2LatLon(0,0,&top_lat,&left_lon) ;
  init_scale_factor = map_scale_factor ;
  orig_scale_factor = init_scale_factor ;
  init_top_lat = top_lat ;
  init_width = width_in_nmi ;
  init_offsetx = offsetx ;
}

/*****************************************************************************
 *
 * Routine: _update_map_variables
 *
 * Description: this routine updates the map projection
 *
 ****************************************************************************/

void _update_map_variables()
{
  offsetx = _get_map_width(0) / 2;
  offsety = _get_map_height(0) / 2;
  _compute_width_in_nmi();
  _compute_scale_factor();
}

/******************************************************************************
 *
 * Routine: _zoom_map_in
 * 
 * Description: this routine zooms map in
 *
 *****************************************************************************/

void _zoom_map_in ( Widget wid , XtPointer client_data , XtPointer call_data )
{
  int i ;

  /* Set up the CTRL-Z Hot Key so that it will return the user to
     the initial view of the map and any previously drawn rectangles
     are ignored. */
  rbdata.zoom_state = True ;
  rbdata.use_rectangle = False ;

  if ( projection != M_HRAP )
  {
     original_zoom_multiple = zoom_multiple ;
  }

  zoom_scale ++ ;
  zoom_multiple *= 2.0 ;
  map_scale_factor = ( long ) ( ( float ) init_scale_factor * zoom_multiple ) ;

  mConvertXY2LatLon(0,0,&top_lat,&left_lon);
  _compute_width_in_nmi();
  _alter_projection_settings ( ) ;

  /* Run any user specific functionality associated with the zooming in
     of the map. */
  if ( zoom_routine != NULL )
  {
     zoom_routine ( wid , client_data , call_data ) ;
  }

  _set_make_map ( M_ON ) ;

  for ( i = 0 ; i < _get_num_of_maps ( ) ; i++ )
  {
     mUpdateMap ( i ) ;
  }

}

/******************************************************************************
 *
 * Routine: _zoom_map_out
 * 
 * Description: this routine zooms map out
 *
 *****************************************************************************/

void _zoom_map_out ( Widget wid , XtPointer client_data , XtPointer call_data )
{
  char reply [ MAX_MAP_STRING_SIZE ] ;
  static char * zoom_out_limit_token_name = "hv_zoom_out_limit" ;
  int i ;
  long previous_scale_factor ;
  int reply_len ;
  int request_len ;
  int status ;
  float previous_zoom_multiple ;
  static long zoom_out_limit = -1 ;

  /* Set up the CTRL-Z Hot Key so that it will return the user to
     the initial view of the map and any previously drawn rectangles
     are ignored. */
  rbdata.zoom_state = True ;
  rbdata.use_rectangle = False ;

  /* Retrieve the zoom out limit if this is the first time this routine
     is being called. */
  if ( zoom_out_limit == -1 )
  {
     request_len = strlen ( zoom_out_limit_token_name ) ;
     status = get_apps_defaults ( zoom_out_limit_token_name ,
                                  & request_len , reply , & reply_len ) ; 

     if ( status != 0 )
     {
        fprintf ( stderr , "\nIn routine \"_zoom_map_out\":\n"
                           "Couldn't retrieve the zoom out limit from token\n"
                           "%s.  Setting %d as the default limiting value.\n" ,
                           zoom_out_limit_token_name , 
                           ZOOM_OUT_LIMIT_DEFAULT ) ;
        zoom_out_limit = ZOOM_OUT_LIMIT_DEFAULT ;
     }
     else
     {

        zoom_out_limit = atol ( reply ) ;

        if ( zoom_out_limit < 0 )
        {
           fprintf ( stderr , "\nIn routine \"_zoom_map_out\":\n"
                              "Invalid value %ld specified by zoom out limit\n"
                              "token %s.  Setting %d as the default limiting\n"
                              "value.\n" , zoom_out_limit ,
                              zoom_out_limit_token_name ,
                              ZOOM_OUT_LIMIT_DEFAULT ) ;
           zoom_out_limit = ZOOM_OUT_LIMIT_DEFAULT ;
        }
     }
  }

  if ( projection != M_HRAP )
  {
     original_zoom_multiple = zoom_multiple ;
  }

  zoom_scale -- ;
  previous_zoom_multiple = zoom_multiple ;
  zoom_multiple /= 2.0 ;
  previous_scale_factor = map_scale_factor ;
  map_scale_factor = ( long ) ( ( float ) init_scale_factor * zoom_multiple ) ;

  /* Prevent the user from "zooming out" too far. */
  if ( projection == M_HRAP || projection == M_POLAR )
  {
     if ( ( map_scale_factor / POLAR_SCALE_MULTIPLE ) <= zoom_out_limit )
     {
        map_scale_factor = previous_scale_factor ;
        zoom_multiple = previous_zoom_multiple ;
     }

  }
  else
  {
     if ( map_scale_factor <= zoom_out_limit )
     {
        map_scale_factor = previous_scale_factor ;
        zoom_multiple = previous_zoom_multiple ;
     }
  }

  _alter_projection_settings ( ) ;
  mConvertXY2LatLon(0,0,&top_lat,&left_lon);
  _compute_width_in_nmi ( ) ;

  /* Run any user-specific functionality associated with the 
     zooming out of the map. */ 
  if ( zoom_routine != NULL )
  {
     zoom_routine ( wid , client_data , call_data ) ;
  }

  _set_make_map(M_ON);

  for ( i = 0 ; i < _get_num_of_maps ( ) ; ++ i )
  {
    _set_map_change ( i ) ;
    mUpdateMap ( i ) ;
  }

}

void mAddZoomRoutine ( MotifCallback zoom_function )
{
   zoom_routine = zoom_function ;
} 

void mAddPanRoutine ( MotifCallback pan_function )
{
  pan_routine = pan_function ;
}

void mOptimizeScaleFactor ( long previous_scale ,
                            double lat , double lon , double lat1 ,
                            double lon1 , int height , int width ,
                            char direction )
{
   int x ;
   int x1 ;
   int y ;
   int y1 ;
   long prev_scale_factor ;

   /* Convert the southwest and northeast HRAP coordinates
      into pixel coordinates. */
   switch ( projection )
   {

      case M_HRAP :

         _convert_latlon_xy_hrap ( ( float ) lat , ( float ) lon , & x , 
                                   & y ) ;
         _convert_latlon_xy_hrap ( ( float ) lat1 , ( float ) lon1 , & x1 ,
                                   & y1 ) ;
         break ;

      case M_POLAR :

         _convert_latlon_xy_polar ( ( float ) lat , ( float ) lon , & x , 
                                    & y ) ;
         _convert_latlon_xy_polar ( ( float ) lat1 , ( float ) lon1 , & x1 , 
                                    & y1 ) ;
         break ;

      case M_FLAT :

         mConvertLatLon2XY ( ( float ) lat , ( float ) lon , & x ,
                             & y ) ;
         mConvertLatLon2XY ( ( float ) lat1 , ( float ) lon1 , & x1 ,
                             & y1 ) ;
         break ;

      default :

         fprintf ( stdout , "\nIn routine 'mOptimizeScaleFactor':\n"
                            "Unsupported map projection. Projection\n"
                            "value %d\n" , projection ) ;
         break ;
   }

   /* Do they fit on the screen? */
   if  ( ( ( x1 - x ) < width ) && 
         ( ( y - y1 ) < ( height - 60 ) ) )
   {
      /* Recursively call the mOptimizeScaleFactor routine. */
      prev_scale_factor = init_scale_factor ;
      init_scale_factor = init_scale_factor + ( init_scale_factor / 25 ) ;
      map_scale_factor = init_scale_factor * zoom_multiple ;
      _alter_projection_settings ( ) ;
      mOptimizeScaleFactor ( prev_scale_factor , lat , lon , lat1 , 
                              lon1 , height , width , 'F' ) ;
   }
   else if ( direction == 'F' )
   {
      init_scale_factor = previous_scale ;
      map_scale_factor = init_scale_factor * zoom_multiple ;
      _alter_projection_settings ( ) ;
   }
   else
   {
      prev_scale_factor = init_scale_factor ;
      init_scale_factor /= 2 ;
      map_scale_factor = init_scale_factor * zoom_multiple ;
      _alter_projection_settings ( ) ;
      mOptimizeScaleFactor ( prev_scale_factor , lat , lon , lat1 , 
                              lon1 , height , width , 'R' ) ;
   }
}

void mArealZoom ( Widget w , XtPointer clientdata , XtPointer calldata )
{
   Boolean ReturnInitialDate = True ;
   double latitude ;
   double longitude ;
   float center_lat ;
   float center_lon ;
   float lat_lower_left ;
   float lat_upper_right ;
   float lon_lower_left ;
   float lon_upper_right ;
   
   int center_pixel_x ;
   int center_pixel_y ;
   int i ;
   int map_projection ;
   int map_height_in_pixels ;
   int map_width_in_pixels ;
   int new_x ;
   int new_y ;
  
   Widget map_widget ;

   if ( rbdata.zoom_state == True )
   {
      rbdata.zoom_state = False ;

      if ( projection == M_HRAP )
      {
         zoom_multiple = 1.0 ;
      }
      else
      {
         zoom_multiple = original_zoom_multiple ;
      }

      _set_projection_scale ( ) ; 

      mSetCenterLatLon ( original_center_lat ,
                         original_center_lon ) ; 

      if ( projection == M_HRAP )
      {
         /* Attempt to size the window to fit the office's MPE area.
            Get the latitude/longitude of the XOR, YOR HRAP
            coordinates representing the origin of the local MPE area grid. */ 
         HrapToLatLongByReference ( ( double ) YOR + MAXY ,
                         ( double ) XOR ,
                         & latitude ,
                         & longitude ) ;
         longitude *= -1 ;
         _convert_latlon_xy_hrap ( ( float ) latitude , 
                                   ( float ) longitude , 
                                   & new_x , 
                                   & new_y ) ; 
         polar_offsety = -1 * new_y ;
         polar_offsetx = -1 * new_x ;
      }
      else
      {
         _alter_projection_settings ( ) ;
      }
   }
   else
   {
      if ( rbdata.use_rectangle == False )
      {
         return ;
      }  

      rbdata.zoom_state = True ;   

      original_center_lat = map_center_lat ;
      original_center_lon = map_center_lon ;
      original_zoom_multiple = zoom_multiple ;

      if ( rbdata.start_x <= 0 && rbdata.start_y <= 0 &&
           rbdata.last_x <= 0 && rbdata.last_y <= 0 )
      {
         /* There is no rectangle to zoom into. */
         return ;
      }

      /* Retrieve the Hydroview/MPE viewing area's height and width. */
      map_width_in_pixels = _get_map_width ( 0 ) ;
      map_height_in_pixels = _get_map_height ( 0 ) ;

      /* Retrieve the center point in pixels. */
      center_pixel_x = rbdata.start_x +
                       ( rbdata.last_x - rbdata.start_x ) / 2 ;
      center_pixel_y = rbdata.start_y +
                       ( rbdata.last_y - rbdata.start_y ) / 2 ;
      _set_make_map ( M_ON ) ;

      /* Test the projection. The quick zoom method depends on which
         projection is being displayed. */
      map_projection = _get_map_projection ( ) ;
      /* Convert the center point to latitude/longitude. */
      mConvertXY2LatLon ( center_pixel_x ,
                          center_pixel_y ,
                          & center_lat ,
                          & center_lon ) ;

      mSetCenterLatLon ( center_lat , center_lon ) ;
      _alter_projection_settings ( ) ;

      /* Convert the lower left and upper right corners of the zoom box
      into latitude / longitude values. */
      mConvertXY2LatLon ( rbdata.start_x ,
                          rbdata.last_y ,
                          & lat_lower_left ,
                          & lon_lower_left ) ;
      mConvertXY2LatLon ( rbdata.last_x ,
                          rbdata.start_y ,
                          & lat_upper_right ,
                          & lon_upper_right ) ;

      mOptimizeScaleFactor ( init_scale_factor , lat_lower_left ,
                             lon_lower_left , lat_upper_right ,
                             lon_upper_right , map_height_in_pixels , 
                             map_width_in_pixels , 'R' ) ;
   }

   /* Set the top latitude and left longitude values. */
   mConvertXY2LatLon(0,0,&top_lat,&left_lon);

   /* Run any user specific functionality associated with the zooming in
     of the map. */
   if ( zoom_routine != NULL )
   {
      map_widget = _get_map_widget ( 0 ) ;
      zoom_routine ( map_widget , ( XtPointer ) & ReturnInitialDate , NULL ) ;
   }


   for ( i = 0 ; i < _get_num_of_maps ( ) ; ++ i )
   {
     _set_map_change ( i ) ;
     mUpdateMap ( i ) ;
   }

}

void mSaveOriginalCenterLatLon ( )
{
   if ( projection != M_HRAP )
   {
      mGetCenterLatLon ( & original_center_lat , 
                         & original_center_lon ) ;
   }
}
