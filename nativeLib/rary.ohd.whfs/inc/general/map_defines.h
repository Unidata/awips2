/****************************************************************************
*
* Header: map_defines.h
*
* Description: contain definations needed for map api
*
***************************************************************************/

/*-------------------------------------------------------------------------
 
 Variable definations

 --------------------------------------------------------------------------*/
#ifndef MAP_DEFINES_H
#define MAP_DEFINES_H

#include <Xm/Xm.h>

#define MAX_NUM_MAPS 4

enum MapState {  M_OFF , M_ON } ;
enum MapGCtype { M_DEFAULT_GC , M_XOR_GC } ;

#define M_AVAILBALE 1
#define M_NOT_AVAILABLE 0

/* drawing areas */

#define M_MAP 0
#define M_LEGEND 1
#define M_EXPOSE 2
#define M_USER 3

/* directions */

#define M_TOP 0
#define M_RIGHT 1
#define M_BOTTOM 2
#define M_OUTSIDE 3

/* cursor types */

#define M_NORMAL 0
#define M_WATCH 1
#define M_SELECT 2
#define M_GROUP_SELECT 3
#define M_HELP 4

/* font types */

#define M_MINISCULE 0
#define M_TINY 1
#define M_SMALL 2
#define M_MEDIUM 3
#define M_LARGE 4
#define M_HUGE 5
#define M_NUM_OF_FONTS 6

/*Point data icon size*/

#define M_S_VSMALL 0
#define M_S_SMALL 1
#define M_S_MEDIUM 2
#define M_S_LARGE 3
#define M_NUM_OF_PDSIZES 4

/* map overlays */

/* If a change is made to this enumeration, the change must also be made to
   the OverlayNames array in map_menubar_cb.c */ 
enum MapOverlays { M_TOPOGRAPHY, /* Topography */
                   M_STATE ,  /* States */
                   M_BASINS , /* Basins */
                   M_CITY_TOWN , /* Cities and Towns */
                   M_COUNTY , /* Counties */
                   M_CWA , /* County Warning Areas */
                   M_HIGHWAYS , /* Highways */
                   M_ROADS , /* Roads */
	           M_NOHIGHWAYS , /* No Highways or Roads */
	           M_HRAP_GRID , /* HRAP Grids */
                   M_LAKES ,     /* Lakes/Reservoirs */
                   M_RIVERS , /* Major Rivers */
                   M_STREAMS , /* Minor Streams */
                   M_NOSTREAMS , /* Minor Streams */
                   M_LAT_LON_LINES , /* Latitude / Longitude Lines. */
                   M_RADAR_LOCATIONS , /* Radar Locations */
                   M_RADAR_RINGS , /* Radar Rings */
                   M_RFC_BOUNDARY , /* RFC Boundaries */
                   M_TIMEZONE , /* Time Zones */
                   M_ZONES , /* Forecast Zones */
                   M_TOPOGRAPHY_CONTOUR, /* Topography Contour */
                   M_TOPOGRAPHY_NONE, /* No topography. */
                   M_OVERLAYS } ; /*  Number of Overlays */

/* map projections */

#define M_FLAT 0
#define M_POLAR 1
#define M_LAMBART 2
#define M_MERCATOR 3
#define M_PROJECTION 4
#define M_HRAP 5

/* symbol types */

#define M_WEATHER_STATION 0
#define M_RIVER_DATA_POINT 1
#define M_RIVER_FORECAST_POINT 2
#define M_RIVER_DATA_POINT_AT_RESERVOIR 3
#define M_RIVER_FORECAST_POINT_AT_RESERVOIR 4
#define M_UNDEFINE_STATION 5
#define M_RIVER_FORECAST_POINT_WITH_WEATHER_STATION 6
#define M_OTHER_STATION 7
#define M_RESERVOIR_POINT 8
#define M_DAMCREST_POINT 9

/* region types */

#define M_WFO 0
#define M_RFC 1
#define M_ALL 2

/* error codes */
#define M_OK 0
#define M_ERROR 1

/* Shapefile types */
enum ShapeFileType { S_POINT = 1 ,
                     S_POLYLINE = 3 ,
                     S_POLYGON = 5 } ;

/* overlay types */

/* If a change is made to this enumeration, the change must also be made to
   the MapOverlayTypes array in map_menubar_cb.c */ 
enum MapOverlayTypes { M_SHAPE ,
                       M_BCD ,
                       M_CALCULATE ,
                       M_EXTERNAL ,
                       M_BITMAP ,
                       M_OVERLAY_END } ;

/* possible overlay file types */

#define M_SHAPEFILE 0
#define M_BCDFILE 1
#define M_BOTH 2
#define M_NONE 3

/* Typedefs. */
typedef  void  ( * MotionCallback ) ( Widget wid ,
                                      XtPointer calldata ,
                                      XEvent * event ) ;

typedef void ( * GeneralCallback ) ( ) ;

typedef void ( * ExposeCallback ) ( int * map_index );

typedef void ( * MotifCallback ) ( Widget wid , 
                                   XtPointer clientdata ,
                                   XtPointer calldata ) ;

typedef int ( * CleanCallback ) ( );

typedef void ( * MouseClickEventCallback ) ( Widget w,
                                             XtPointer clientdata,
                                             XEvent * event,
                                             Boolean * flag );

struct _Map {
  Pixmap pixmap ;
  Widget map ;
  Widget legend ;
  Pixmap lmap ;
  int x ;
  int y ;
  int width ;
  int height ;
  int lx ;
  int ly ;
  unsigned int legend_change ;
  unsigned int map_change ;
  GeneralCallback froutine ;
  ExposeCallback lroutine ;
  ExposeCallback routine ;
  GeneralCallback sroutine ;
  MotionCallback legend_draw ;
  MotionCallback mroutine ;
  MotionCallback wroutine ;
  MouseClickEventCallback lsroutine;
  int lheight;
} ;

typedef struct _clicks
{
   Widget wid ;
   int first_button_press ;
   int second_button_press ;
   int x ;
   int y ;
   int map_num;
} clicks ;

#endif /* #ifndef MAP_DEFINES_H */
