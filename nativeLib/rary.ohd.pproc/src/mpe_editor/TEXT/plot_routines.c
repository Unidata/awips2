
/*******************************************************************************
* FILENAME:            plot_routines.c
* NUMBER OF MODULES:  10 
* GENERAL INFORMATION:
*   MODULE 1:   _draw_radar_rings
* DESCRIPTION:  This routine plots the radar rings for all of the radar sites. 
*   MODULE 2:   _show_radar_rings (for Hydromap).
* DESCRIPTION:  This routine draws radar rings for the WFO or RFC that is being
*               viewed (for Hmap_mpe).
*   MODULE 3:   _draw_lat_lon_lines
* DESCRIPTION:  This routine draws latitude and longitude lines on the map.
*   MODULE 4:   _draw_radar_locations
* DESCRIPTION:  This routine plots the locations of WSR-88D radar sites on
*               the map. 
*   MODULE 5:   _draw_fsl_city_locations
* DESCRIPTION:  This routine plots fsl city information.
*   MODULE 6:   _draw_mpe_city_locations
* DESCRIPTION:  This routine plots cities and towns MPE style.
*   MODULE 7:   _draw_whfs_city_locations
* DESCRIPTION:  Plots the city and town information stored within the City
*               table of the whfs database.
*   MODULE 8:   _draw_hrap_grid
* DESCRIPTION:  This routine draws the hrap grid for the WFO or RFC currently
*               being viewed.
*   MODULE 9:   _draw_hrap_boundary
* DESCRIPTION:  This routine draws the hrap boundary for the WFO or RFC
*               currently being viewed.
*   MODULE 10:  _draw_hrap_area
* DESCRIPTION:  This routine draws and fills the area of a hrap grid for the
*               WFO or RFC currently being processed.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       May 21, 2002
* ORGANIZATION:        HSEB / OHD
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*       1-8        5/21/2002     Bryon Lawrence    Original Coding. 
*         9        2/25/2003     Bryon Lawrence    Original Coding.
*        10        3/23/2004     Bryon Lawrence    Original Coding.
********************************************************************************
*/
#include <errno.h>
#include <stdio.h>
#include <Xm/Xm.h>

#include "City.h"
#include "drawa.h"
#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "map.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_menubar_cb.h"
#include "map_resource.h"
#include "map_shapefile.h"
#include "mpe_log_utils.h"
#include "plot_routines.h"
#include "post_functions.h" /* For the "getLatLonGrid" routine prototype. */
#include "read_overlay_configuration.h"
#include "read_precip_data.h"
#include "read_rresult.h" /* For the RadarIgnoreFlag enumeration. */
#include "rfcwide_callbacks.h" /* For the add_mpe_gage_ids and the
                                  add_mpe_gage_values routine prototypes */
#include "stage3.h"  /* For "HRAP" definition. */
#include "stage3_globals.h" /* For the "get_pixel_by_name" routine. */

#define KILOMETERS_PER_HRAP_GRID 4
#define KILOMETERS_TO_NAUTICAL_MILES 0.5396

const int CrossHalfLength = 2 ; /* For the location "crosses" drawn on the
                                   single site radar window. The length
                                   of a line in the cross is twice this 
                                   value. */

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   _draw_radar_rings
* PURPOSE:       This routine  plots the radar rings for all of the radar 
*                sites across the country.  This routine is currently not
*                being used in the hmap_mpe application.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME      DESCRIPTION/UNITS
*   Input  int         index     The index of the map library map being drawn
*                                to.  Not used in this routine. Kept only to
*                                keep the interface to this routine generic. 
*   Input  void *      pData     A pointer to a generic data structure, which
*                                enables the interface to this routine to
*                                support many different types of drawing
*                                routines.  This routine expects a pointer
*                                to a struct _Overlay type to be passed in
*                                here.
*   Input  Pixmap      map       The pixmap to draw the radar rings on. 
*   Input  void *      pUserData User supplied data specific to this routine. 
*                                Not used here.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME               HEADER FILE     DESCRIPTION
*   _area_draw_circle  map_draw.h      Draws a circle on the pixmap. 
*   mConvertLatLon2XY  map_convert.h   Converts a latitude, longitude
*                                      coordinate into a x,y point on the
*                                      map.
*   mSetColor          map_library.h   Sets the color of the radar rings
*                                      being drawn.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE          NAME          DESCRIPTION
*   char               filename      The name of the file containing the
*                                    radar identifiers and central 
*                                    coordinates.
*   char               name          The name of the radar site.  
*   double             latr          The number of degrees latitude of the top
*                                    most extent of the radar coverage.
*   double             lonr          The number of degrees longitude of the 
*                                    farthest right most extent of the radar
*                                    coverage.
*   FILE *             pFile         Points the the FILE structure describing
*                                    the file containing the radar 
*                                    locations.  
*   float              disclosure    The disclosure limit which determines
*                                    the density and number of cities displayed
*                                    for a given zoom level.
*   float              latitude      The latitude of the radar site as read
*                                    from the file.
*   float              longitude     The longitude of the radar site
*   float              templat       The latitude of the farthest topmost 
*                                    extent of the radar coverage.
*   float              templon       The longitude of the rightmost extend of
*                                    the radar coverage.
*   int                h             The height of the rectangle that just
*                                    contains the radar coverage ring. 
*   int                x             The x-coordinate of the upper left corner
*                                    of the rectangle that just encompasses
*                                    the radar coverage ring.
*   int                y             The y-coordinate of the upper left corner
*                                    of the rectangle that just contains the
*                                    radar coverage ring.
*   int                w             The width of the rectangle that just 
*                                    contains the the radar coverage area.
*   struct _Overlay * pOverlay       Contains information pertaining to the
*                                   the radar ring overlay. See 
*                                    "map_menubar_cb.h".
*
* DATA FILES AND/OR DATABASE:
*   This routine relies on the "88D.lpi" which resides in the fxa 
*   localization tree for the site that this application is being fun for.
*
* ERROR HANDLING:
*   A non-existent file or a file that cannot be opened will prompt this
*   routine to print out an error message and return before drawing the
*   radar rings.
*    
********************************************************************************
*/
void _draw_radar_rings ( int index , void * pData ,
                         Pixmap map , void * pUserData )
{
   char filename [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   char name [ MAX_RADAR_ID_LENGTH ] ;
   double latr , lonr ;
   float latitude ;
   float longitude ;
   float disclosure ;
   float templat , templon ;
   int x , y , w , h ;
   FILE * pFile = NULL ;
   struct _Overlay * pOverlay = NULL ;

   pOverlay = ( struct _Overlay * ) pData ;

   /* Build the filepath. */
   if ( pOverlay->filename [ 0 ] == NULL )
   {
      return ;
   }

   if ( pOverlay->filepath [ 0 ] != NULL )
   {
      sprintf ( filename , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
   }
   else
   {
      sprintf ( filename , "%s" , pOverlay->filename [ 0 ] ) ;
   }

   /* Attempt to open the file containing the locations of the
      radar sites.  The latitude and longitude coordinates are
      needed to draw the radar rings. */
   pFile = fopen ( filename , "r" ) ;

   if ( pFile == NULL )
   {

      flogMessage ( stderr , "In routine _draw_radar_rings:\n\n"
                         "Could not open file %s. Aborting the drawing of"
                         "the radar rings.\n" , filename ) ;
      return ;
   }

   /* Set the color for the radar location information. */
   mSetColor ( pOverlay->color ) ;
   mSetLineWidth ( pOverlay->line_width ) ;

   latr = 230000.0 / ( 1852.0 * 60.0 ) ;

   /* Read the file, plotting each station coordinate. */
   fscanf ( pFile , "%f%f%f%s" , & latitude, & longitude , & disclosure , 
            name ) ;

   while ( ! feof ( pFile ) )
   {
      templat = ( 3.14159265 / 180.0 ) * latitude ;
      templat = cos ( templat ) ;
      lonr = 230000.0 / ( 1852.0 * 60.0 * templat ) ;
      templat = latitude + latr ;
      templon = longitude - lonr ;
      mConvertLatLon2XY ( templat , templon , & x , & y );
      templat = latitude - latr ;
      templon = longitude + lonr ;
      mConvertLatLon2XY ( templat , templon , & w , & h ) ;
      w = abs ( w - x ) ;
      h = abs ( h - y ) ;
      _area_draw_circle ( map , x , y , w , h ) ;

      /* Read the next record from the file. */
      fscanf ( pFile , "%f%f%f%s" , & latitude, & longitude , & disclosure , 
               name ) ;
   }

   fclose ( pFile ) ;
   pFile = NULL ;

   mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;

}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   _show_radar_rings
* PURPOSE:       This routine draws radar rings for the WFO or RFC that is
*                currently being processed in hmap_mpe.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME      DESCRIPTION/UNITS
*   Input  int         index     The number of the map library pixmap
*                                currently being drawn to.
*   Input  void *      pData     A pointer to the struct _Overlay that contains
*                                data for the radar rings.
*   Input  Pixmap      map       The pixmap to draw the radar rings on. 
*   Input  void *      pUserData Other user supplied data. Not used here.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE      DESCRIPTION
*   _get_map_display     map_resource.h   Returns the display the map resides
*                                         in. 
*   get_pixel_by_name    stage3_globals.h Gets the color pixel value of the
*                                         named color.
*   HrapToLatLongMpe     stage3.h         Converts a HRAP coordinate to a
*                                         Latitude / Longitude coordinate.
*   isThereMpeData       post_functions.h Determines if there is MPE data drawn
*                                         on the screen.  If there is 
*                                         data, then color the radar rings
*                                         according to active and inactive
*                                         radar sites. 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE    NAME          DESCRIPTION
*   float        radius        The radius of a radar ring in nautical miles.
*   float        left_lat      The latitude of the northwest corner of the
*                              rectangle containing the radar ring.
*   float        left_lon      The longitude of the northwest corner of the
*                              rectangle containing the radar ring. 
*   float        top_lat       The latitude of the top of the rectangle
*                              which encloses the radar ring.
*   float        top_lon       The longitude of the radar site.
*   int          data_flag     Indicates whether or not there is MPE data
*                              drawn on the screen.
*   int          height        The height of the rectangle that just contains
*                              the radar ring.
*   int          i             A loop indexing variable.
*   int          width         The width of the rectangle that just contains
*                              the radar ring.
*   GC           gc2           A graphics context used in drawing the radar
*                              rings of a specific color.
*   GC           gc4           A graphics context used in drawing the radar
*                              rings of a specific color.
*   Display  *   dpy           The display that the application is running
*                              under and viewable in.
*   point        center_hrap   The hrap coordinate of the radar site.   
*   HRAP         center_latlon The latitude/longitude of the radar site. 
*   int          center_xpt    The pixel x point of the radar site.
*   int          center_ypt    The pixel y point of the radar site.
*   int          left_xpt      The pixel x point of the northwest corner of the
*                              rectangle that encompasses the radar ring.
*   int          left_ypt      The pixel y point of the northwest corner of the
*                              rectangle that encompasses the radar ring.
*   int          top_xpt       The pixel x point of the radar site.
*   int          top_ypt       The pixel y point of the top of the rectangle
*                              containing the radar ring.  
*   int          mask          The mask specifying what type of graphics 
*                              context is being used.
*   XGCValues    gcv2          A graphics context from drawing rings for
*                              radars that are active.
*   XGCValues    gcv4          A graphics context from drawing rings for
*                              radars that are inactive.
*   draw_struct * data         Contains graphic resources for the MPE
*                              data. 
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
********************************************************************************
*/
const float degrees_to_radians =  0.017453292519943295 ;

void _show_radar_rings ( int index , void * pData , Pixmap map , 
                         void * pUserData )
{
   float        radius ;
   float        left_lat ;
   float        left_lon ;
   float        top_lat ;
   float        top_lon ;
   int          data_flag ;
   unsigned int          height ;
   int          i ;
   unsigned int          width ;
   GC           gc2 , gc4 ; 
   Display  *   dpy = NULL ;
   point        center_hrap ;
   HRAP         center_latlon ;
   int          center_xpt , center_ypt ;
   int          left_xpt , left_ypt ;
   int          top_xpt , top_ypt ;
   int          mask = GCForeground ;
   struct _Overlay * pOverlay = NULL ;
   XGCValues    gcv2 , gcv4 ;

   pOverlay = ( struct _Overlay * ) pData ;

   draw_struct * data = & rad_data [ index ] ;
   dpy = _get_map_display ( ) ;

   /*-------------------------------------------------------------*/
   /*     create graphics contexts                                */
   /*     display legend                                          */
   /*-------------------------------------------------------------*/
   gcv2.foreground = get_pixel_by_name ( data->w , 
                                         "green" ) ;
   gcv4.foreground = get_pixel_by_name ( data->w , 
                                         "red" ) ;
   gc2 = XCreateGC ( dpy , DefaultRootWindow ( dpy ) , mask , & gcv2 ) ;
   gc4 = XCreateGC ( dpy , DefaultRootWindow ( dpy ) , mask , & gcv4 ) ;

   /* For Linux, the line width needs to be set to at least 1 to keep
      the radar rings from disappearing when zooming really closely 
      into the data field. */
   XSetLineAttributes ( dpy , gc4 , pOverlay->line_width , LineSolid , 
		        CapButt , JoinMiter ) ;
   XSetLineAttributes ( dpy , gc2 , pOverlay->line_width , LineSolid ,                                  CapButt , JoinMiter ) ;

   /* Check to determine if there is any MPE data currently
      plotted on the viewer. */
   data_flag = isThereMpeData ( ) ;

   /*-------------------------------------------------------------*/
   /*     display rings                                           */
   /*     rings are color coded by no data/radar only/gage only/  */
   /*        radar + gage                                         */
   /*-------------------------------------------------------------*/

   for ( i = 0 ; i < NRADARS ; ++ i )
   {
      /* Convert the Hrap coordinate of the radar center into
         the latitude/longitude coordinate pairs representing the
         radar center location and the upper left-hand corner of the 
         invisible rectangle that just encompasses the radar ring.*/
      center_hrap.x = nexrad [ i ].ctr.x ;
      center_hrap.y = nexrad [ i ].ctr.y ;
      center_latlon = HrapToLatLongMpe ( center_hrap ) ;

      /* At this point we must do some hard core mathematics.  We know
         the radius of the circle in HRAP grids.  We know that 
         each HRAP grid box is 4km by 4km.  Translate the radius into
         kilometers.  Then translate the radius into nautical miles.
         This value can be used to convert directly into latitude and
         longitude coordinates of the height and the width of the 
         circle relative to the pixmap coordinate axes. */
      radius = KILOMETERS_PER_HRAP_GRID * ( float ) nexrad [ i ].ngrd ;
      radius *= KILOMETERS_TO_NAUTICAL_MILES ;

      left_lat = center_latlon.y ;
      top_lon = -1 * center_latlon.x ;
      left_lon =  top_lon -
                  radius / ( cos ( left_lat * degrees_to_radians ) * 60 ) ;
      top_lat = left_lat + radius / 60 ;

      /* Convert the Latitude/Longitude coordinate into the x, y
         pixel coordinates of the radar center, the left-most point of
         the circle, and the top-most point of the circle. */
      mConvertLatLon2XY ( left_lat , top_lon , & center_xpt , & center_ypt ) ;
      mConvertLatLon2XY ( left_lat , left_lon , & left_xpt , & left_ypt ) ;
      mConvertLatLon2XY ( top_lat , top_lon , & top_xpt , & top_ypt ) ;

      /* Compute the major and minor axis of the circle to be drawn. */
      width = ( unsigned int ) ( 2 * ( center_xpt - left_xpt ) ) ;
      height = ( unsigned int ) ( 2 * ( center_ypt - top_ypt ) ) ;

      if ( ( data_flag == 0 ) || ( iflarad [ i ] == 1 ) ||
           ( iflign [ i ] == IgnoreRadar ) ) /* No Data. */
      {
         XDrawArc ( dpy , map , gc4 , left_xpt , top_ypt , 
                    width , height , 0 , 64*360 ) ;
      }
      else /* There is radar data. */
      {
         XDrawArc ( dpy , map , gc2 , left_xpt , top_ypt ,
                    width , height , 0 , 64*360 ) ;
      }

   }

   XFreeGC ( dpy , gc2 ) ;
   XFreeGC ( dpy , gc4 ) ;
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:    _draw_lat_lon_lines
* PURPOSE:        This routine draws latitude and longitude lines on 
*                 the Hmap_mpe viewing area.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME      DESCRIPTION/UNITS
*   Input  int         index     The index of the map library map being drawn.
*   Input  void *      pData     The pointer to the overlay configuration
*                                structure which contains data about the 
*                                overlay being drawn.
*   I/O    Pixmap      map       The pixmap upon which to draw the latitude/
*                                longitude lines.
*   Input  void *      pUserData Optional data of the user's choice. In this
*                                routine it is not used.  Any data supplied 
*                                through this parameter are ignored.
*                                   
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE    DESCRIPTION
*   _area_draw_line      map_draw.h     Draws a line on a drawable 
*                                       (e.g. Pixmap)
*   _area_draw_text      map_draw.h     Draws text on a drawable such as a 
*                                       Pixmap 
*   _get_map_projection  map_convert.h  Retrieves the map projection currently
*                                       in use. 
*   mConvertLatLon2XY    map_convert.h  Converts a latitude/longitude
*                                       coordinate into a pixel coordinate.
*   _set_foreground      map_resource.h Sets the foreground color.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
* DATA TYPE            NAME    DESCRIPTION
* char                 str     Used in constructing the latitude and 
*                              longitude labels.
* float                lat     Contains the value of the latitude line 
*                              being drawn.
* float                lon     Contains the value of the longitude 
*                              line being drawn.
* int                  first   Keeps track of the number of iterations 
*                              through this routine.
* int                  i       Loop variable. 
* int                  j       Loop variable.
* int                  proj    The number of the map projection.
* int                  temp    A temporary variable used in various 
*                              calculations. 
* int                  top     The top latitude.
* int                  x       Pixel x coordinate of beginning of line.
* int                  x2      Pixel x coordinate of end of line.
* int                  y       Pixel y coordinate of beginning of line.
* int                  y2      Pixel y coordinate of end of line.
* struct _Overlay * pOverlay   A pointer to the overlay structure containing
*                              overlay data.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
void _draw_lat_lon_lines ( int index , void * pData ,  
                           Pixmap map , void * pUserData )
{
  int i,j,x,y,x2,y2,top,temp,proj,first=1;
  float lat,lon;
  char str[10];
  struct _Overlay * pOverlay = NULL ;

  pOverlay = ( struct _Overlay * ) pData ;

  mSetLineWidth ( pOverlay->line_width ) ;

  proj = _get_map_projection();
  if (proj == M_LAMBART){
    top = (_get_top_lat() * .1) * 10;
  }
  else if (proj == M_POLAR)
    top = 80;
  else
    top = 90;

  for (i=top; i > -90; i= i - 10){
    lat = (float) i;
    sprintf(str,"%3.0f",lat);
    _set_foreground(pOverlay->color);
    if (proj != M_LAMBART)
      mConvertLatLon2XY(lat,_get_center_lon(),&x,&y);

    if (proj == M_LAMBART){
      first = 1;
      for (j=-180;j <= 180;j = j + 10){
	lon = (float) j;
	if (first == 1){
	  first = 0;
	  mConvertLatLon2XY(lat,lon,&x,&y);
	}
	else{
	  mConvertLatLon2XY(lat,lon,&x2,&y2);
	  _area_draw_line(map,x,y,x2,y2);
	  x = x2;
	  y = y2;
	}
      }

      _set_foreground("Black");
      _area_draw_text(map,x2,y2,str);
    }
    else if (proj == M_POLAR){
      temp  = (_get_offsety() - y) * 2;
      mConvertLatLon2XY(90.0,_get_center_lon(),&x2,&y2);
      y = y2 - (temp/2);
      x = x2 - (temp/2); 
      _area_draw_circle(map,x,y,temp,temp);
      _set_foreground("Black");
      _area_draw_text(map,x-15,y2,str);
    }
    else{
      _area_draw_line(map,0,y,_get_map_width(index),y);
      _set_foreground("Black");
      _area_draw_text(map,1,y,str);
    }
  }   

  for (i=-180;i < 180;i = i + 10){
    _set_foreground(pOverlay->color);
    lon = (float)i;
    sprintf(str,"%4.0f",lon);
    if (proj == M_LAMBART){
      first = 1;
      for (j=80; j > -90; j = j - 10){
	lat = (float) j;
	if (first == 1){
	  first = 0;
	  mConvertLatLon2XY(lat,lon,&x,&y);
	}
	else{
	  mConvertLatLon2XY(lat,lon,&x2,&y2);
	  _area_draw_line(map,x,y,x2,y2);
	  x = x2;
	  y = y2;
	}
      }
      _set_foreground("Black");

      mConvertLatLon2XY(0.0,lon,&x2,&y2);
      _area_draw_text(map,x2-20,y2,str);
    }
    else if (proj == M_POLAR){
      mConvertLatLon2XY(80.0,lon,&x,&y);
      mConvertLatLon2XY(0.0,lon,&x2,&y2);
      _area_draw_line(map,x,y,x2,y2);
      _set_foreground("Black");
      _area_draw_text(map,x2,y2,str);
    }
    else{
      mConvertLatLon2XY(_get_top_lat(),lon,&x,&y);
      _area_draw_line(map,x,0,x,_get_map_height(index));
      _set_foreground("Black");
      _area_draw_text(map,x-15,10,str);
    }
  } 

  mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
}
/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME: _draw_radar_locations
* PURPOSE:     This routine plots the locations of WSR-88D radar sites
*              across the country.  It also displays the name of each of the
*              sites.   This information is acquired from a FSL flat file
*              containing radar data in the following format:
*
*              39.7867 -104.5458   150.876 KFTG
*              60.7258 -151.3514   313.045 PAHG
*              56.8528 -135.5292  1112.046 PACG
*              65.0350 -147.5017  7613.901 PAPD
*              64.5114 -165.2950   842.316 PAEC
*              58.6794 -156.6294   792.520 PAKC
*              60.7919 -161.8764   376.107 PABC
*
*              where the columns represent the latitude, longitude, disclosure,
*              and name of the radar site.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME       DESCRIPTION/UNITS
*   Input  int         index      The number of the map library map being
*                                 displayed.
*   Input  void *      pData      A pointer to an overlay struct containing
*                                 overlay data. 
*   Input  Pixmap      map        The pixmap to draw the radar sites on.
*   Input  void *      pUserData  A pointer to optional additional data.  Not
*                                 Used in this routine.
*
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                HEADER FILE         DESCRIPTION
*   mConvertLatLon2XY 
*   mDrawFillCircle 
*   mDrawText 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE        NAME       DESCRIPTION
*  char [ ]          filename   The name of the file to retrieve the
*                               radar locations from.
*  char [ ]          name       The name of the radar site currently
*                               being processed.
*  float             latitude   The latitude of the radar site.
*  float             longitude  The longitude of the radar site.
*  float             disclosure The disclosure factor.
*  int               x          The pixel "x" coordinate of the radar location.
*  int               y          The pixel "y" coordinate of the radar location.
*  FILE *            pFile      A pointer to the FILE struct representing the
*                               file containing the radar information.
*  struct _Overlay * pOverlay   A pointer the overlay structure containing
*                               information specific to the overlay.
*
* DATA FILES AND/OR DATABASE:
*   This routine requires a file containing radar location information
*   formated as described above in the "Purpose" section.
*
* ERROR HANDLING:
*
********************************************************************************
*/
void _draw_radar_locations ( int index , void * pData ,  
                             Pixmap map , void * pUserData )
{
   char filename [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   char name [ MAX_RADAR_ID_LENGTH ] ;
   float latitude ;
   float longitude ;
   float disclosure ;
   int x , y ;
   FILE * pFile = NULL ;

   struct _Overlay * pOverlay = NULL ;

   pOverlay = ( struct _Overlay * ) pData ;

   /* Build the filepath. */
   if ( pOverlay->filename[0] == NULL )
   {
      return ;
   }

   if ( pOverlay->filepath[0] != NULL )
   {
      sprintf ( filename , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
   }
   else
   {
      sprintf ( filename , "%s" , pOverlay->filename [ 0 ] ) ;
   }

   /* Attempt to open the file containing the locations of the
      radar sites. */
   pFile = fopen ( filename , "r" ) ;

   if ( pFile == NULL )
   {

      flogMessage ( stderr , "In routine _draw_radar_locations:\n\n"
                         "Could not open file %s. Aborting the drawing of"
                         "the radar locations.\n" , filename ) ;
      return ;
   }


   /* Set the color for the radar location information. */
   mSetColor ( pOverlay->color ) ;
   mSetLineWidth ( pOverlay->line_width ) ;

   /* Read the file, plotting each station coordinate. */
   fscanf ( pFile , "%f%f%f%s" , & latitude, & longitude , & disclosure , 
            name ) ;

   while ( ! feof ( pFile ) )
   {
      /* Convert the latitude and longitude into a meaningful 
         coordinate. */
      mConvertLatLon2XY ( latitude , longitude , & x , & y ) ;

      /* Plot the location as a filled circle. */
      mDrawFillCircle ( M_EXPOSE , 0 , x , y , RADAR_CIRCLE_DIAMETER , 
                        RADAR_CIRCLE_DIAMETER ) ;

      mDrawText ( M_EXPOSE , 0 , x + RADAR_LABEL_X_OFFSET , 
                                 y + RADAR_LABEL_Y_OFFSET , name ) ;
     
      /* Read the next record from the file. */
      fscanf ( pFile , "%f%f%f%s" , & latitude, & longitude , & disclosure , 
               name ) ;
   }
    
   fclose ( pFile ) ;
   pFile = NULL ;
   mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
}

/*******************************************************************************
* MODULE NUMBER:  5
* MODULE NAME:    _draw_fsl_city_locations
* PURPOSE:        This routine plots city information to a user-supplied
*                 pixmap using FSL-formatted city ascii data files.
*                 The format of these files is as follows:
*
*                 41.7235  -73.2438    20.213 BANTAM|CT
*                 41.3715  -73.4116     6.045 BETHEL|CT
*                 42.0316  -73.3314   271.197 CANAAN|CT
*                 41.4021  -73.4715    13.025 DANBURY|CT
*                 41.0511  -73.4797     6.893 DARIEN|CT
*                 41.2473  -73.4334     6.290 GEORGETOWN|CT
*                 41.5885  -73.4068    35.838 NEW MILFORD|CT
*                 41.6817  -73.3543    10.282 NEW PRESTON|CT
*                 41.4132  -73.3160    20.904 NEWTOWN|CT
*                 41.0939  -73.4202    11.106 NORWALK|CT
*                 41.2738  -73.4999    76.796 RIDGEFIELD|CT
*                 41.0967  -73.5527    18.924 STAMFORD|CT
*                 38.9051  -77.0162     7.976 WASHINGTON|DC
*
*                 Where the columsn are as follows:
*
*                 Latitude of radar site.
*                 Longitude of radar site.
*                 Disclosure factor for zooming purposes.
*                 The town and state (pipe delimeted) that the radar site 
*                 is located in.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME       DESCRIPTION/UNITS
*   Input  int         index      The number of the map library map being
*                                 displayed.
*   Input  void *      pData      A pointer to an overlay struct containing
*                                 overlay data. 
*   Input  Pixmap      map        The pixmap to draw the radar sites on.
*   Input  void *      pUserData  A pointer to optional additional data.
*                                 When this routine is being called from
*                                 the single site radar popup window,
*                                 this argument needs to be a draw_struct *
*                                 variable.  If this variable is NULL, it
*                                 tells this routine to draw to the main
*                                 viewing area using the map library routines.
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME               HEADER FILE    DESCRIPTION
*   get_apps_defaults  GeneralUtil.h  Retrieves the value of a token from the
*                                     environment or apps defaults tokens. 
*   get_pixel_by_name  globals.h      Retrieves color value of a named color.
*   _get_screen_width  map.h          Returns the width of the main map screen.
*   LatLongToHrapMpe   stage3.h       Converts a lat/lon coordinate to a
*                                     Hrap coordinate.
*   mConvertLatLon2XY  map_convert.h  Converts a lat/lon coordinate to a
*                                     pixel coordinate.
*   mDrawFillCircle    map_library.h  Draws a filled circle.
*   mDrawText          map_library.h  Draws text on a drawable widget.
*   mGetWidthInNmi     map_library.h  Returns the width in nautical miles
*                                     of the map viewing area. 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME                 DESCRIPTION
*   Arg [ ]           wargs                Used in retrieving the height
*                                          and width values of the
*                                          drawing area widget being drawn
*                                          on.
*   char [ ]          filename             The name of the file to retrieve
*                                          the city location data from.
*   char *            hv_disclosure_limit  Used in determining how many 
*                                          cities to plot for a particular
*                                          zoom level.
*   char *            pToken               Used in parsing the record of
*                                          city data read in from the flat
*                                          file.
*   char              record               A single record read in from the
*                                          city flat file.
*   char              reply                The value of a token as returned
*                                          from "get_apps_defaults".
*   Dimension         height               The height, in pixels, of the
*                                          Drawing Area widget being
*                                          written to.
*   Dimension         width                The width, in pixels of the drawing
*                                          area widget being written to.
*   Display *         dpy                  The display the application is 
*                                          being viewed on.
*   draw_struct *     pDrawStruct          Contains additional information
*                                          required when drawing to the
*                                          single site radar popup window.
*   FILE *            pFile                Points to the FILE structure 
*                                          that represents the file the city
*                                          data is being read from. 
*   float             disclosure           The disclosure value as read in
*                                          from the FSL data file.
*   float             disclosure_limit     The limiting value which determines
*                                          how many cities are display at a 
*                                          given zoom level.
*   float             latitude             The latitude of the city.
*   float             longitude            The longitude of the city.
*   float             width_in_nmi         The width in nautical miles of the
*                                          viewing area.
*   GC                gc                   The graphics context of the 
*                                          application.
*   HRAP              hrap                 Represents the coordinates of a
*                                          single HRAP point. 
*   int               len                  The length of a token name being
*                                          supplied to "get_apps_defaults". 
*   int               mask                 Used in setting values in the
*                                          graphics context.
*   int               n                    Keeps track of the number of
*                                          arguments being stored in "wargs".
*   int               num_disclosure_pixels Used in computing the number
*                                           of cities to display at a given
*                                           zoom level.
*   int               pixels                Used in computing the number
*                                           of cities to display at a given
*                                           zoom level.
*   int               x                     A loop variable.
*   int               y                     A loop variable.
*   int               reply_len             The length of the token value
*                                           returned in the "reply" variable
*                                           from a call to "get_apps_defaults".
*   int               request_len           The length of the requested token
*                                           name being passed into
*                                           "get_apps_defaults".
*   int               status                A value indicating the success
*                                           or failure of the call to
*                                           get_apps_defaults. 
*   struct _Overlay * pOverlay              A pointer to the overlay structure
*                                           containing definitions specific
*                                           to the cities overlay.
*   XGCValues         gcv                   A pointer to a XGCValues structure
*                                           which is used in defining a 
*                                           Graphics Context.
*
* DATA FILES AND/OR DATABASE:
*   This file requires file specified by file name and path arguments
*   of the overlay structure.  The file must have the format as outlined
*   in the purpose field above. 
*
* ERROR HANDLING:
*   None.
*
********************************************************************************
*/

void _draw_fsl_city_locations ( int index , void * pData ,  
                                Pixmap map , void * pUserData )
{
   Arg wargs [ 5 ] ;
   char filename [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   static char * hv_disclosure_limit = "hv_disclosure_limit" ;
   char * pToken = NULL ;
   char record [ MAX_RECORD_LENGTH ] ;
   char reply [ LEN_REPLY ] ;
   Dimension height ;
   Dimension width ;
   Display * dpy = NULL ;
   draw_struct * pDrawStruct = NULL ;
   float disclosure ;
   float disclosure_limit ;
   float latitude ;
   float longitude ;
   float width_in_nmi ;
   GC gc = NULL ;
   HRAP hrap ;
   int len ;
   int mask = GCForeground ;
   int n ;
   int num_disclosure_pixels ;
   int pixels , x , y ;
   int reply_len ;
   int request_len ;
   int status ;
   XGCValues gcv ;

   FILE * pFile = NULL ;

   struct _Overlay * pOverlay = NULL ;
   pDrawStruct = ( draw_struct * ) pUserData ;
   pOverlay = ( struct _Overlay * ) pData ;


   /* Build the filepath. */
   if ( pOverlay->filename[0] == NULL )
   {
      return ;
   }

   if ( pOverlay->filepath[0] != NULL )
   {
      sprintf ( filename , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
   }
   else
   {
      sprintf ( filename , "%s" , pOverlay->filename [ 0 ] ) ;
   }

   /* Attempt to open the file containing the city names and their 
      latitude/longitude coordinates. */
   pFile = fopen ( filename , "r" ) ;

   if ( pFile == NULL )
   {
      flogMessage ( stderr , "In routine \"_draw_fsl_city_locations\":\n\n"
                         "Could not open file %s. Aborting the plotting of"
                         "the city locations.\n" , filename ) ;
      return ;
   }

   mSetLineWidth ( pOverlay->line_width ) ;

   /* Test to determine who is calling this routine. */
   if ( pDrawStruct != NULL )
   {
      /* Get a pointer to the display. */ 
      dpy = XtDisplay ( pDrawStruct->w ) ;

      /* Determine the width and the height of the single site
         radar pixmap being drawn to. */
      n = 0 ;
      XtSetArg ( wargs[n] , XmNwidth , & width ) ; n++ ;
      XtSetArg ( wargs[n] , XmNheight , & height ) ; n++ ;
      XtGetValues ( pDrawStruct->w , wargs , n ) ;

      /* Determine the number of pixels per HRAP bin. */
      x = ( float ) width / ( float ) pDrawStruct->maximum_columns ;
      y = ( float ) height / ( float ) pDrawStruct->maximum_rows ;

      if (x > y)
         x = y;
      else if (y > x)
         y = x;

      /* Create the graphics context for the city location plots. */
      gcv.foreground = get_pixel_by_name( pDrawStruct->w ,
                                          pOverlay->color ) ;
      gc = XCreateGC ( dpy , DefaultRootWindow ( dpy ) , mask , & gcv ) ;

      /* Read the file, plotting each station coordinate. */
      fgets ( record , MAX_RECORD_LENGTH , pFile ) ;

      while ( ! feof ( pFile ) )
      {
         pToken = strtok ( record , " " ) ;
         latitude = atof ( pToken ) ;
         pToken = strtok ( NULL , " " ) ;
         longitude = atof ( pToken ) ;
         pToken = strtok ( NULL , " " ) ;
         disclosure = atof ( pToken ) ;

         /* Retrieve the name of the city. */
         pToken = strtok ( NULL , "|" ) ;

         /* Convert the latitude / longitude coordinate to a HRAP 
            coordinate. */
         hrap = LatLongToHrapMpe ( latitude , ( -1 ) * longitude ) ; 

         hrap.x -= pDrawStruct->origin.x ;
         hrap.y -= pDrawStruct->origin.y ;

         hrap.x *= x ;
         hrap.y = ( pDrawStruct->maximum_rows - hrap.y ) * y ;

         XDrawLine ( dpy , pDrawStruct->pix , gc , 
                     hrap.x - CrossHalfLength , 
                     hrap.y , hrap.x + CrossHalfLength , hrap.y ) ;
         XDrawLine ( dpy , pDrawStruct->pix , gc , hrap.x , 
                     hrap.y - CrossHalfLength  ,
                     hrap.x , hrap.y + CrossHalfLength ) ;

         len = strlen ( pToken ) ; 
         XDrawString ( dpy , pDrawStruct->pix , gc , 
                       hrap.x + CrossHalfLength ,
                       hrap.y - CrossHalfLength ,
                       pToken , len ) ;

         fgets ( record , MAX_RECORD_LENGTH , pFile ) ;
      }

      XFreeGC ( dpy , gc ) ;
   }
   else
   {
      /* Retrieve the progressive disclosure factor from the .Apps_defaults
         national token file. */
      request_len = strlen ( hv_disclosure_limit ) ;
      status = get_apps_defaults ( hv_disclosure_limit ,
                                   & request_len ,
                                   reply ,
                                   & reply_len ) ;

      if ( status != 0 )
      {
         flogMessage ( stderr , "\nIn routine \"_draw_fsl_city_locations\":\n"
                            "Could not retrieve the hydroview\n"
                            "progressive disclosure value from the\n"
                            "environment or apps defaults token file.\n"
                            "The cities and towns cannot be displayed.\n" ) ;
         mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
         return ;
      }

      num_disclosure_pixels = atoi ( reply ) ;

      /* Set the color for the city location information. */
      mSetColor ( pOverlay->color ) ;

      /* Determine the disclosure factor. */
      pixels = _get_screen_width ( ) ;
      width_in_nmi = mGetWidthInNmi ( ) ;

      disclosure_limit = ( width_in_nmi / ( float ) pixels ) 
                         * ( float ) num_disclosure_pixels ;

      /* Read the file, plotting each station coordinate. */
      fgets ( record , MAX_RECORD_LENGTH , pFile ) ;

      while ( ! feof ( pFile ) )
      {
         pToken = strtok ( record , " " ) ;
         latitude = atof ( pToken ) ;
         pToken = strtok ( NULL , " " ) ;
         longitude = atof ( pToken ) ;
         pToken = strtok ( NULL , " " ) ;
         disclosure = atof ( pToken ) ;

         if ( disclosure >= disclosure_limit )
         {
            pToken = strtok ( NULL , "|" ) ;

            /* Convert the latitude and longitude into a meaningful 
               coordinate. */
            mConvertLatLon2XY ( latitude , longitude , & x , & y ) ;

            /* Plot the location as a filled circle. */
            mDrawFillCircle ( M_EXPOSE , 0 , x , y , CITY_CIRCLE_DIAMETER , 
                              CITY_CIRCLE_DIAMETER ) ;

            mDrawText ( M_EXPOSE , 0 , x + CITY_LABEL_X_OFFSET , 
                                       y + CITY_LABEL_Y_OFFSET , pToken ) ;
         }

         /* Read the next record from the file. */
         fgets ( record , MAX_RECORD_LENGTH , pFile ) ;
      }
   } 

   fclose ( pFile ) ;
   pFile = NULL ;
   mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;

}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:  _draw_mpe_city_locations
* PURPOSE:      This routine plots cities and towns MPE style.  It uses the
*               "town.dat" and "town_zoom.dat" files to display cities and
*               WFOs.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME       DESCRIPTION/UNITS
*   Input  int         index      The number of the map library map being
*                                 displayed.
*   Input  void *      pData      A pointer to an overlay struct containing
*                                 overlay data.
*   Input  Pixmap      map        The pixmap to draw the radar sites on.
*   Input  void *      pUserData  A pointer to optional additional data.
*                                 When this routine is being called from
*                                 the single site radar popup window,
*                                 this argument needs to be a draw_struct *
*                                 variable.  If this variable is NULL, it
*                                 tells this routine to draw to the main
*                                 viewing area using the map library routines.
*
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE        DESCRIPTION
*   get_apps_defaults    GeneralUtil.h      This routine retrieves the value
*                                           off an environmental variable or
*                                           apps defaults token.
*   LatLongToHrapMpe     stage3.h           Converts a latitude/longitude
*                                           coordinate into a Hrap coordinate.
*   mConvertLatLon2XY    map_convert.h      Converts a map latitude/longitude
*                                           point into a pixmap x,y coordinate.
*   mDrawFillCircle                         Draws circle on a pixmap.
*   mDrawText                               Draws text on a pixmap.
*   mGetWidthInNmi                          Retrieves the width in nautical
*                                           miles of the main map area.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME                      DESCRIPTION
*   Arg [ ]           wargs                     Used in retrieving the 
*                                               width and height of the
*                                               Drawing Area Widgets of the
*                                               single site radar window.
*   char [ ]          city                      The name of the current city
*                                               being plotted. 
*   char *            filename                  The full path and name of the
*                                               file containing the 
*                                               the city files.
*   char [ ]          reply                     The token value as returned
*                                               from "get_apps_defaults".
*   char [ ]          zi_filename               The name of the file containing
*                                               the zoomed in city data.
*   char [ ]          zo_filename               The name of the file containing
*                                               the zoomed out city data.
*   char [ ]          zoom_threshold_token_name The name of the token 
*                                               containing the zoom threshold
*                                               value.  This is a value
*                                               in nautical miles below which
*                                               the "zoomed in" city values
*                                               are plotted.
*   Dimension         height                    The height of the drawing area
*                                               widget the cities are being
*                                               plotted on.
*   Dimension         width                     The width of the drawing area
*                                               widget the cities are being
*                                               plotted on. 
*   Display *         dpy                       The display the application
*                                               is being viewed in.
*   draw_struct *     pDrawStruct               The draw structure containing
*                                               data necessary for drawing
*                                               cities on the single site 
*                                               popup windows. 
*   FILE *            pFile                     A pointer to the FILE structure
*                                               containing information 
*                                               pertinent to the files 
*                                               containing the Mpe city 
*                                               geo data.
*   float             latitude                  The latitude of the city.
*   float             longitude                 The longitude of the city.
*   float             map_width_in_nmi          The width in nautical miles
*                                               of the main map viewing region.
*   GC                gc                        The graphics context to use
*                                               when map library drawing 
*                                               routines are not being used.
*   HRAP              hrap                      Represents the coordinates of
*                                               a HRAP point.
*   int               len                       Used to keep track of the
*                                               lengths of the city names
*                                               being drawn.
*   int               mask                      Used to set values in the
*                                               graphics context.
*   int               n                         Keeps track of the number of
*                                               arguments being stored in
*                                               the "wargs" array.
*   int               reply_len                 The length of the token value
*                                               stored in the "reply" variable
*                                               as returned from a call to
*                                               "get_apps_defaults".
*   int               request_len               The length of a token name
*                                               being supplied to 
*                                               "get_apps_defaults".
*   int               status                    The return status of the 
*                                               calls to "get_apps_defaults".
*   int               x                         A loop indexing variable.
*   int               y                         A loop indexing variable. 
*   int               zoom_threshold            The threshold level
*                                               (in nautical miles) which
*                                               determines whether to display
*                                               course or fine resolution 
*                                               city information.
*   struct _Overlay * pOverlay                  A pointer to the overlay
*                                               structure containing
*                                               information pertinent to 
*                                               to the cities overlay.
*   XGCValues         gcv                       Contains values which are
*                                               in turn used to define a
*                                               a graphics context.
*
* DATA FILES AND/OR DATABASE:
*
*   This routine expects two filenames to be supplied via the overlay
*   structure. The first filename should be the name of the zoomed out city
*   data.  The second filename should be the file containing the zoomed in
*   city information.
*   The format of the data in these files should be as follows:
*
*     ERI 42.080 80.180
*     UCA 43.150 75.380
*     ACY 39.450 74.580
*     DOV 39.130 75.470
*     LYH 37.330 79.200
*     MRB 39.400 77.980
*     MARFC 40.800 77.900
*     AVP 41.400 75.550
*
*   The first column contains the name of the cities to be plotted,
*   the second column contains the latitude of the city to be plotted,
*   the third column contains the longitude of the city to be plotted.
*
* ERROR HANDLING:
*   All error diagnostics are printed onto the standard output stream.
********************************************************************************
*/
void _draw_mpe_city_locations ( int index , void * pData ,  
                                Pixmap map , void * pUserData )
{
   Arg wargs [ 5 ] ;
   char city [ MAX_CITY_NAME_LENGTH ] ;
   char * filename = NULL ;
   char reply [ LEN_REPLY ] ;
   char zi_filename [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   char zo_filename [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   static char zoom_threshold_token_name [ ] = "hv_zoom_threshold" ;
   Dimension height ;
   Dimension width ;
   Display * dpy = NULL ;
   draw_struct * pDrawStruct = NULL ;
   FILE * pFile = NULL ;
   float latitude ;
   float longitude ;
   float map_width_in_nmi ;
   GC gc = NULL ;
   HRAP hrap ;
   int len ;
   int mask = GCForeground ;
   int n ;
   int reply_len ;
   int request_len ;
   int status ;
   int x ;
   int y ;
   int zoom_threshold  ;
   XGCValues gcv ;

   struct _Overlay * pOverlay = NULL ;
   pDrawStruct = ( draw_struct * ) pUserData ;
   pOverlay = ( struct _Overlay * ) pData ;


   /* Was any overlay data supplied to this routine? */
   if ( pOverlay == NULL )
   {
      flogMessage ( stderr , "\nIn routine \"_draw_mpe_city_locations\":\n"
                         "\"pOverlay\" is NULL.  No overlay information has\n"
                         "been provided to this routine.  Aborting the\n"
                         "plotting of the city locations.\n" ) ;
      return ;
   }

   /* This routine is expecting two files.  One is for the "zoomed out"
      view of the cities and towns.  One is for the "zoomed in" view
      of the cities and towns. */
   if ( pOverlay->n_files != 2 )
   {
      flogMessage ( stderr , "\nIn routine \"_draw_mpe_city_locations\":\n"
                         "This routine requires two files for drawing\n"
                         "MPE cities and towns.  One file is for the\n"
                         "\"zoomed out\" view and one file is for the\n"
                         "\"zoomed in\" view of the cities and towns.\n" ) ;
      return ;
   }

   /* Build the filepath for the zoomed out view of the cities and towns. */
   if ( pOverlay->filename[0] == NULL )
   {
      return ;
   }

   if ( pOverlay->filepath[0] != NULL )
   {
      sprintf ( zo_filename , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
   }
   else
   {
      sprintf ( zo_filename , "%s" , pOverlay->filename [ 0 ] ) ;
   }

   /* Build the filepath for the zoomed in view of the cities and towns. */
   if ( pOverlay->filename[1] == NULL )
   {
      return ;
   }

   if ( pOverlay->filepath[1] != NULL )
   {
      sprintf ( zi_filename , "%s/%s" , pOverlay->filepath [ 1 ] ,
                pOverlay->filename [ 1 ] ) ;
   }
   else
   {
      sprintf ( zi_filename , "%s" , pOverlay->filename [ 1 ] ) ;
   }

   mSetLineWidth ( pOverlay->line_width ) ;

   /* Test to determine who is calling this routine. */
   if ( pDrawStruct != NULL )
   {
      /* Retrieve a pointer to the display. */ 
      dpy = XtDisplay ( pDrawStruct->w ) ;

      /* Determine the width and the height of the single site
         radar pixmap being drawn to. */
      n = 0 ;
      XtSetArg ( wargs[n] , XmNwidth , & width ) ; n++ ;
      XtSetArg ( wargs[n] , XmNheight , & height ) ; n++ ;
      XtGetValues ( pDrawStruct->w , wargs , n ) ;

      /* Determine the number of pixels per HRAP bin. */
      x = ( float ) width / ( float ) pDrawStruct->maximum_columns ;
      y = ( float ) height / ( float ) pDrawStruct->maximum_rows ;

      if (x > y)
         x = y;
      else if (y > x)
         y = x;

      /* Create the graphics context for the city location plots. */
      gcv.foreground = get_pixel_by_name( pDrawStruct->w ,
                                          pOverlay->color ) ;
      gc = XCreateGC ( dpy , DefaultRootWindow ( dpy ) , mask , & gcv ) ;

      /* Attempt to open the file containing the city names and their 
         latitude/longitude coordinates. */
      pFile = fopen ( zo_filename , "r" ) ;

      if ( pFile == NULL )
      {

         flogMessage ( stderr , "\nIn routine \"_draw_mpe_city_locations\":\n\n"
                            "Could not open file %s. Aborting the plotting of"
                            "the city locations.\n" , zo_filename ) ;
         mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
         return ;
      }

      /* Read the file, plotting each station coordinate. */
      fscanf ( pFile , "%s %f %f", city , & latitude , & longitude ) ;

      while ( ! feof ( pFile ) )
      {

         /* Convert the latitude / longitude coordinate to a HRAP 
            coordinate. */
         hrap = LatLongToHrapMpe ( latitude , longitude ) ; 

         hrap.x -= pDrawStruct->origin.x ;
         hrap.y -= pDrawStruct->origin.y ;

         hrap.x *= x ;
         hrap.y = ( pDrawStruct->maximum_rows - hrap.y ) * y ;

         XDrawLine ( dpy , pDrawStruct->pix , gc , hrap.x - CrossHalfLength , 
                     hrap.y ,
                     hrap.x + CrossHalfLength , hrap.y ) ;
         XDrawLine ( dpy , pDrawStruct->pix , gc , hrap.x , 
                     hrap.y - CrossHalfLength ,
                     hrap.x , hrap.y + CrossHalfLength ) ;

         len = strlen ( city ) ; 
         XDrawString ( dpy , pDrawStruct->pix , gc , 
                       hrap.x + CrossHalfLength ,
                       hrap.y - CrossHalfLength ,
                       city , len ) ;


         /* Read the next record from the file. */
         fscanf ( pFile , "%s %f %f", city , & latitude , & longitude ) ;
      }

      XFreeGC ( dpy , gc ) ;
   }
   else
   {

      /* Which file is displayed depends upon a token defined 
         threshold value in nautical miles. */
      request_len = strlen ( zoom_threshold_token_name ) ;
      status = get_apps_defaults ( zoom_threshold_token_name , & request_len ,
                                   reply , & reply_len ) ;

      if ( status != 0 )
      {
         flogMessage ( stderr , "\nIn routine \"_draw_mpe_city_locations\":\n"
                            "Could not retrieve the value for token \"%s\".\n"
                            "The ability to view \"zoomed in\" cities and\n"
                            "towns has been disabled.\n" , 
                            zoom_threshold_token_name ) ;
         filename = zo_filename ;
      }
      else
      {
         map_width_in_nmi = mGetWidthInNmi ( ) ;  
         errno = 0 ;
         zoom_threshold = atof ( reply ) ;

         if ( errno == ERANGE )
         {
            flogMessage ( stderr , "\nIn routine \"_draw_mpe_city_locations\":\n"
                               "The call to atof failed.  Could not convert\n"
                               "\"%s\" to a floating point value.\n" , reply ) ;
            filename = zo_filename ; 
         }
         else if ( map_width_in_nmi <= zoom_threshold )
         {
            filename = zi_filename ;
         }
         else
         {
            filename = zo_filename ;
         }
      }
      
      /* Attempt to open the file containing the city names and their 
         latitude/longitude coordinates. */
      pFile = fopen ( filename , "r" ) ;

      if ( pFile == NULL )
      {

         flogMessage ( stderr , "\nIn routine \"_draw_mpe_city_locations\":\n\n"
                            "Could not open file %s. Aborting the plotting of"
                            "the city locations.\n" , filename ) ;
         mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
         return ;
      }

      /* Set the color for the city location information. */
      mSetColor ( pOverlay->color ) ;

      /* Read the file, plotting each station coordinate. */
      fscanf ( pFile , "%s %f %f", city , & latitude , & longitude ) ;

      while ( ! feof ( pFile ) )
      {
         /* Convert the latitude and longitude into a meaningful 
            coordinate. */
         mConvertLatLon2XY ( latitude , -1 * longitude , & x , & y ) ;

         /* Plot the location as a filled circle. */
         mDrawFillCircle ( M_EXPOSE , 0 , x , y , CITY_CIRCLE_DIAMETER , 
                           CITY_CIRCLE_DIAMETER ) ;

         mDrawText ( M_EXPOSE , 0 , x + CITY_LABEL_X_OFFSET , 
                                    y + CITY_LABEL_Y_OFFSET , city ) ;

         /* Read the next record from the file. */
         fscanf ( pFile , "%s %f %f", city , & latitude , & longitude ) ;
      }
    
   }

   fclose ( pFile ) ;
   pFile = NULL ;
   mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;

}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   _draw_whfs_city_locations
* PURPOSE:       Plot the city and town information stored within the City
*                table of the whfs database.  This routine can be used to
*                plot the city information either on the main Hmap_mpe viewing
*                area (using the map library plotting routines) or on the
*                single site radar viewing area using the mpe style for 
*                drawing geo data.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  int         index      The number of the map library map being
*                                 displayed.
*   Input  void *      pData      A pointer to an overlay struct containing
*                                 overlay data.
*   Input  Pixmap      map        The pixmap to draw the radar sites on.
*   Input  void *      pUserData  A pointer to optional additional data.
*                                 When this routine is being called from
*                                 the single site radar popup window,
*                                 this argument needs to be a draw_struct *
*                                 variable.  If this variable is NULL, it
*                                 tells this routine to draw to the main
*                                 viewing area using the map library routines.
*
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                  HEADER FILE    DESCRIPTION
*   get_apps_defaults     GeneralUtil.h  Retrieves the value of a token.
*   get_pixel_by_name     Globals.h      Retrieves the color for a given color
*                                        name.
*   GetCity               City.h         Retrieves the next city in the linked
*                                        list of city information read from the
*                                        City table in the whfs table.
*   LatLongToHrapMpe      stage3.h       Converts a latitude / longitude 
*                                        coordinate into an HRAP coordinate.
*   ListCount             List.h         Retrieves the number of nodes in
*                                        a linked list.
*   ListFirst             List.h         Retrieves the head node in a linked
*                                        list.
*   ListNext              List.h         Retrieves the next node in the linked
*                                        list of city information.
*   mConvertLatLon2XY     map_convert.h  Converts a latitude /longitude 
*                                        coordinate into a pixmap x,y 
*                                        coordinate. 
*   mDrawFillCircle       map_library.h  Draws a filled circle on a pixmap.
*   mDrawText             map_library.h  Draws text onto a pixmap.
*   mGetWidthInNmi        map_library.h  Retrieves the width of the drawing
*                                        area in nautical miles.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                               DESCRIPTION
*   Arg [ ]             wargs                     Used in retrieving the
*                                                 width and the height
*                                                 of the Drawing Area that
*                                                 the cities are being
*                                                 plotted on.
*   char [ ]            reply                     Contains the value of a
*                                                 token as read from the
*                                                 "get_apps_defaults" routine.
*   char [ ]            where_clause              Specifies the condition
*                                                 to use in selecting rows from
*                                                 the City database table.
*   char [ ]            zoom_threshold_token_name The name of the token which
*                                                 specifies the zoom 
*                                                 threshold.
*   City *              pCityList                 A pointer to the linked
*                                                 list of city information.
*   City *              pCityTown                 A pointer used to walk
*                                                 through the linked list
*                                                 of city and town information.
*   Dimension           height                    The height of the drawing
*                                                 area the cities are being
*                                                 plotted on.
*   Dimension           width                     The width of the drawing
*                                                 area the cities are being 
*                                                 plotted on.
*   Display *           dpy                       The display the application
*                                                 is being viewed from.
*   enum DispPrecedence disp_precedence           An enumerated value 
*                                                 specifying whether to
*                                                 display cities (coarse
*                                                 resolution) or cities and
*                                                 towns (fine resolution). 
*   draw_struct *       pDrawStruct               Points to a draw_struct
*                                                 structure containing
*                                                 information required
*                                                 for plotting cities
*                                                 on the single site radar
*                                                 window.
*   float               map_width_in_nmi          The width of the viewing
*                                                 area in nautical miles.
*   float               zoom_threshold            The value, taken from
*                                                 token, which specifies
*                                                 whether to display cities
*                                                 or both cities and towns.
*   GC                  gc                        The graphics context to
*                                                 use in drawing the city
*                                                 data.
*   HRAP                hrap                      Contains the coordinates
*                                                 representing a HRAP point.
*   int                 len                       The length of the city/
*                                                 town being displayed. 
*   int                 mask                      The mask indicating
*                                                 how to use the graphics
*                                                 context 
*   int                 n                         The number of args contained
*                                                 within the wargs routine.
*   int                 num_nodes                 The number of nodes in the
*                                                 linked list.
*   int                 reply_len                 The length of the token
*                                                 value returned in "reply"
*                                                 from the call to 
*                                                 "get_apps_defaults". 
*   int                 request_len               The length of the token being
*                                                 sent into the call to
*                                                 "get_apps_defaults" 
*   int                 status                    A value indicating the 
*                                                 success or failure of 
*                                                 the call to
*                                                 "get_apps_defaults"
*   int                 x                         A loop indexing variable.
*   int                 y                         A loop indexing variable.
*   struct _Overlay *   pOverlay                  A pointer to a struct
*                                                 _Overlay containing
*                                                 information.
*   XGCValues           gcv                       Contains values 
*                                                 used in the definition
*                                                 GC.
*
* DATA FILES AND/OR DATABASE:
*   This routine relies on the City table in the whfs database.
*
* ERROR HANDLING:
*
********************************************************************************
*/
void _draw_whfs_city_locations ( int index , void * pData ,  
                                 Pixmap map , void * pUserData )
{
   Arg wargs [ 5 ] ;
   char reply [ LEN_REPLY ] ;
   char where_clause [ MAX_RECORD_LENGTH ] ;
   static char zoom_threshold_token_name [ ] = "hv_zoom_threshold" ;
   City * pCityList = NULL ;
   City * pCityTown = NULL ;
   Dimension height ;
   Dimension width ;
   Display * dpy = NULL ;
   draw_struct * pDrawStruct = NULL ;
   enum DispPrecedence { CITIES = 1 , TOWNS = 2 } ;
   enum DispPrecedence disp_precedence ;
   float map_width_in_nmi ;
   float zoom_threshold ;
   GC gc = NULL ;
   HRAP hrap ;
   int len ;
   int mask = GCForeground ;
   int n ;
   int num_nodes = 0 ;
   int request_len ;
   int reply_len ;
   int status ;
   int x ;
   int y ;
   XGCValues gcv ;

   struct _Overlay * pOverlay = NULL ;
   pDrawStruct = ( draw_struct * ) pUserData ;
   pOverlay = ( struct _Overlay * ) pData ;

   mSetLineWidth ( pOverlay->line_width ) ;

   /* The resolution of the cities and towns depends on the
      "hv_zoom_threshold" token set by the user. */
   if ( pDrawStruct != NULL )
   {
      disp_precedence = CITIES ;

      /* Get a pointer to the display. */ 
      dpy = XtDisplay ( pDrawStruct->w ) ;

      /* Determine the width and the height of the single site
         radar pixmap being drawn to. */

      n = 0 ;
      XtSetArg ( wargs[n] , XmNwidth , & width ) ; n++ ;
      XtSetArg ( wargs[n] , XmNheight , & height ) ; n++ ;
      XtGetValues ( pDrawStruct->w , wargs , n ) ;

      /* Determine the number of pixels per HRAP bin. */
      x = ( float ) width / ( float ) pDrawStruct->maximum_columns ;
      y = ( float ) height / ( float ) pDrawStruct->maximum_rows ;

      if (x > y)
         x = y;
      else if (y > x)
         y = x;

      /* Create the graphics context for the city location plots. */
      gcv.foreground = get_pixel_by_name( pDrawStruct->w ,
                                          pOverlay->color ) ;
      gc = XCreateGC ( dpy , DefaultRootWindow ( dpy ) , mask , & gcv ) ;

   }
   else
   {

      request_len = strlen ( zoom_threshold_token_name ) ;
      status = get_apps_defaults ( zoom_threshold_token_name , & request_len ,
                                   reply , & reply_len ) ;

      if ( status != 0 )
      {
         flogMessage ( stderr , "\nIn routine \"_draw_whfs_city_locations\":\n"
                            "Could not retrieve the value of token \"%s\".\n"
                            "Only cities and towns with a display precedence\n"
                            "of 1 will be displayed.\n" , 
                            zoom_threshold_token_name ) ;
         disp_precedence = CITIES ;
      }
      else
      {
         /* Retrieve the map width in nautical miles. */
         map_width_in_nmi = mGetWidthInNmi ( ) ;  

         errno = 0 ;
         zoom_threshold = atof ( reply ) ;

         if ( errno == ERANGE )
         {
            flogMessage ( stderr , "\nIn routine \"_draw_whfs_city_locations\":\n"
                               "The call to atof failed.  Could not convert\n"
                               "\"%s\" to a floating point value.\n" , reply ) ;
            disp_precedence = CITIES ;
         }
         else if ( map_width_in_nmi <= zoom_threshold )
         {
            disp_precedence = TOWNS ;
         }
         else
         {
            disp_precedence = CITIES ;
         }
      }
      
   }
  
   /* Construct the where clause based upon the zoom threshold
      specified by the user in the "hv_zoom_threshold" token. */
   sprintf ( where_clause , "WHERE disp_precedence = %s" ,
             ( disp_precedence == CITIES ? "1" : 
               "1 OR disp_precedence = 2" ) ) ;
   
   pCityList = GetCity ( where_clause ) ;

   /* Walk through the linked list of "City" structures. */
   if ( pCityList != NULL )
   {
      num_nodes = ListCount ( & pCityList->list ) ;
     
      if ( num_nodes != 0 )
      {
         /* Set the color for the city location information. */
         mSetColor ( pOverlay->color ) ;

         pCityTown = ( City * ) ListFirst ( & pCityList-> list ) ;

         while ( pCityTown != NULL )
         {
            if ( pDrawStruct != NULL )
            {
               /* Convert the latitude / longitude coordinate to a HRAP 
                  coordinate. */
               hrap = LatLongToHrapMpe ( pCityTown->lat , pCityTown->lon ) ; 

               hrap.x -= pDrawStruct->origin.x ;
               hrap.y -= pDrawStruct->origin.y ;

               hrap.x *= x ;
               hrap.y = ( pDrawStruct->maximum_rows - hrap.y ) * y ;

               XDrawLine ( dpy , pDrawStruct->pix , gc , 
                           hrap.x - CrossHalfLength , hrap.y ,
                           hrap.x + CrossHalfLength , hrap.y ) ;
               XDrawLine ( dpy , pDrawStruct->pix , gc , hrap.x , 
                           hrap.y - CrossHalfLength ,
                           hrap.x , hrap.y + CrossHalfLength ) ;

               len = strlen ( pCityTown->name ) ; 
               XDrawString ( dpy , pDrawStruct->pix , gc , 
                             hrap.x + CrossHalfLength ,
                             hrap.y - CrossHalfLength ,
                             pCityTown->name , len ) ;


            }
            else
            {
               /* Convert the latitude and longitude into a meaningful 
                  coordinate. */
               mConvertLatLon2XY ( ( float ) pCityTown->lat , 
                                    ( float ) ( -1 * pCityTown->lon ) , 
                                    & x , & y ) ;

               /* Plot the location as a filled circle. */
               mDrawFillCircle ( M_EXPOSE , 0 , x , y , CITY_CIRCLE_DIAMETER , 
                                 CITY_CIRCLE_DIAMETER ) ;

               mDrawText ( M_EXPOSE , 0 , x + CITY_LABEL_X_OFFSET , 
                                y + CITY_LABEL_Y_OFFSET , pCityTown->name ) ;
  
            } 

            /* Read the next node in the linked list. */
            pCityTown = ( City * ) ListNext ( & pCityTown->node ) ;
             
         }
      } 
   }

   if ( num_nodes == 0 )
   {
      flogMessage ( stderr , "\nIn routine \"_draw_whfs_city_locations\":\n"
                         "No data could be read from the \"City\" table\n"
                         "in the database for query \"SELECT * FROM City\n"
                         "%s\".\n" , where_clause ) ;
   }

   while ( pCityList != NULL )
   {
      pCityTown = ( City * ) ListNext ( & pCityList->node ) ;
      free ( ( void * ) pCityList ) ;
      pCityList = pCityTown ;
   }

   mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
   
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   _draw_hrap_grid
* PURPOSE:       This routine draws the hrap grid for the WFO or RFC currently
*                being processed.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  int         index      The number of the map library map being
*                                 displayed.
*   Input  void *      pData      A pointer to an overlay struct containing
*                                 overlay data.
*   Input  Pixmap      map        The pixmap to draw the radar sites on.
*   Input  void *      pUserData  A pointer to optional additional data.
*                                 This is not used in this routine.
*
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE      DESCRIPTION
*   getLatLonGrid        post_functions.h Returns a pointer to the array
*                                         used to convert HRAP points
*                                         to lat/lon coordinates.
*   mSetColor            map_library.h    Sets the color for the graphics
*                                         about to be drawn.
*   mConvertLatLon2XY    map_convert.h    Converts a latitude / longitude
*                                         value into a x,y coordinate on
*                                         a pixmap.
*   mDrawLine            map_library.h    Draws a line on a pixmap.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME         DESCRIPTION
*   const HRAP **     hrap_lat_lon A pointer to the array of latitude/longitude
*                                  values on a HRAP pointer basis.
*   int               i            A loop indexing variable.
*   int               j            A loop indexing variable.
*   int               xpos1        The x position of the lower point of the
*                                  line. 
*   int               xpos2        The x position of the upper point of the
*                                  line.
*   int               ypos1        The y position of the lower point of the
*                                  line.
*   int               ypos2        The y position of the upper point of the
*                                  line.
*   struct _Overlay * pOverlay     A pointer to the overlay structure which
*                                  contains the information pertinent to the
*                                  display of the hrap overlay.
*   XPoint [ ] prev_points         Added for efficiency.  Keeps track of 
*                                  previous column of HRAP points to be
*                                  processed so that they do not need to
*                                  be recomputed.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   Error messages are printed out to the standard error stream if
*   problems are encountered.  If an error occurs, then this routine will
*   not try to draw the overlay.
* 
********************************************************************************
*/
void _draw_hrap_grid ( int index , void * pData ,  
                       Pixmap map , void * pUserData )
{
   const HRAP ** hrap_lat_lon = NULL ;
   int i , j ;
   int xpos1 ;
   int xpos2 ;
   int ypos1 ;
   int ypos2 ;
   struct _Overlay * pOverlay = NULL ;
   XPoint prev_points [ MAXY ] ;

   pOverlay = ( struct _Overlay * ) pData ;

   if ( pOverlay == NULL )
   {
      flogMessage ( stderr , "In routine \"_draw_hrap_grid\":\n"
                         "NULL overlay information was passed into this\n"
                         "routine.  Could not draw the hrap grid.\n" ) ;
      return ;
   }

   /* Retrieve a pointer to the Latitude / Longitude "look up" array. */
   hrap_lat_lon = getLatLonGrid ( ) ;

   if ( hrap_lat_lon == NULL )
   {
      flogMessage ( stderr , "In routine \"_draw_hrap_grid\":\n"
                         "The attempt to draw the HRAP grid overlay\n"
                         "has failed.  The latitude / longitude \"look up\"\n"
                         "array is undefined.  Routine \"createLatLonGrid\"\n"
                         "must first be called to create this array.\n" ) ;
      return ;
   }

   /* Set the color for the HRAP grid. */
   mSetColor ( pOverlay->color ) ;
   mSetLineWidth ( pOverlay->line_width ) ;

   /* The latitude / longitude lookup array has been found.  Draw the
      HRAP grid for this WFO or RFC. */
   for ( i = 0 ; i <= MAXX ; ++ i )
   {
      for ( j = 0 ; j < MAXY ; ++ j )
      {
         if ( j == 0 )
         {
            mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                                hrap_lat_lon [ i ] [ j ].x ,
                                & xpos1 , & ypos1 ) ;
         }
         else
         {
            xpos1 = xpos2 ;
            ypos1 = ypos2 ;
         }
     
         mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j + 1 ].y ,
                             hrap_lat_lon [ i ] [ j + 1 ].x ,
                             & xpos2 , & ypos2 ) ;

         /* Draw the line in the "Y" direction. */
         mDrawLine ( M_EXPOSE , 0 , xpos1 , ypos1 , xpos2, ypos2 ) ;

         if ( i != 0 )
         {
            /* Draw the horizontal lines. */
            if ( j == 0 )
            {
               mDrawLine ( M_EXPOSE , 0 , xpos1 , ypos1 ,
                           prev_points [ j ].x , 
                           prev_points [ j ].y ) ;
            }

            mDrawLine ( M_EXPOSE , 0 , xpos2 , ypos2 ,
                        prev_points [ j + 1 ].x , 
                        prev_points [ j + 1 ].y ) ;
           
         }

         if ( j == 0 )
         {
           prev_points [ j ].x = xpos1 ;
           prev_points [ j ].y = ypos1 ;
         }

         prev_points [ j + 1 ].x = xpos2 ;
         prev_points [ j + 1 ].y = ypos2 ;


      }

   }

   mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
   
}

/*******************************************************************************
* MODULE NUMBER: 9
* MODULE NAME:   _draw_hrap_boundary
* PURPOSE:       This routine draws the boundary of a hrap grid for the WFO
*                or RFC currently being processed.  This boundary was developed
*                to replace the gray border currently displayed in
*                Hydroview/MPE to represent areas which are outside of the
*                MPE region of forecast responsibility.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  int         index      The number of the map library map being
*                                 displayed.
*   Input  void *      pData      A pointer to an overlay struct containing
*                                 overlay data.
*   Input  Pixmap      map        The pixmap to draw the radar sites on.
*   Input  void *      pUserData  A pointer to optional additional data.
*                                 This is not used in this routine.
*
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE      DESCRIPTION
*   getLatLonGrid        post_functions.h Returns a pointer to the array
*                                         used to convert HRAP points
*                                         to lat/lon coordinates.
*   mSetColor            map_library.h    Sets the color for the graphics
*                                         about to be drawn.
*   mConvertLatLon2XY    map_convert.h    Converts a latitude / longitude
*                                         value into a x,y coordinate on
*                                         a pixmap.
*   mDrawLine            map_library.h    Draws a line on a pixmap.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME         DESCRIPTION
*   const HRAP **     hrap_lat_lon A pointer to the array of latitude/longitude
*                                  values on a HRAP pointer basis.
*   int               i            A loop indexing variable.
*   int               j            A loop indexing variable.
*   int               xpos1        The x position of the lower point of the
*                                  line. 
*   int               xpos2        The x position of the upper point of the
*                                  line.
*   int               ypos1        The y position of the lower point of the
*                                  line.
*   int               ypos2        The y position of the upper point of the
*                                  line.
*   struct _Overlay * pOverlay     A pointer to the overlay structure which
*                                  contains the information pertinent to the
*                                  display of the hrap overlay.
*   XPoint [ ] prev_points         Added for efficiency.  Keeps track of 
*                                  previous column of HRAP points to be
*                                  processed so that they do not need to
*                                  be recomputed.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   Error messages are printed out to the standard error stream if
*   problems are encountered.  If an error occurs, then this routine will
*   not try to draw the overlay.
* 
********************************************************************************
*/
void _draw_hrap_boundary ( int index , void * pData ,  
                           Pixmap map , void * pUserData )
{
   const HRAP ** hrap_lat_lon = NULL ;
   int i , j ;
   int xpos1 ;
   int xpos2 ;
   int ypos1 ;
   int ypos2 ;
   struct _Overlay * pOverlay = NULL ;

   pOverlay = ( struct _Overlay * ) pData ;

   if ( pOverlay == NULL )
   {
      flogMessage ( stderr , "In routine \"_draw_hrap_boundary\":\n"
                         "NULL overlay information was passed into this\n"
                         "routine.  Could not draw the hrap grid.\n" ) ;
      return ;
   }

   /* Retrieve a pointer to the Latitude / Longitude "look up" array. */
   hrap_lat_lon = getLatLonGrid ( ) ;

   if ( hrap_lat_lon == NULL )
   {
      flogMessage ( stderr , "In routine \"_draw_hrap_boundary\":\n"
                         "The attempt to draw the HRAP grid overlay\n"
                         "has failed.  The latitude / longitude \"look up\"\n"
                         "array is undefined.  Routine \"createLatLonGrid\"\n"
                         "must first be called to create this array.\n" ) ;
      return ;
   }

   /* Set the color for the HRAP grid. */
   mSetColor ( pOverlay->color ) ;
   mSetLineWidth ( pOverlay->line_width ) ;

   /* The latitude / longitude lookup array has been found.  Draw the
      HRAP boundary for this WFO or RFC. */
   
   /* First draw the left and right sides of the box. */
   for ( i = 0 ; i < MAXX ; i += MAXX - 1 )
   {
      for ( j = 0 ; j < MAXY ; ++ j )
      {
         if ( j == 0 )
         {
            mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                                hrap_lat_lon [ i ] [ j ].x ,
                                & xpos1 , & ypos1 ) ;
         }
         else
         {
            xpos1 = xpos2 ;
            ypos1 = ypos2 ;
         }
     
         mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j + 1 ].y ,
                             hrap_lat_lon [ i ] [ j + 1 ].x ,
                             & xpos2 , & ypos2 ) ;

         /* Draw the line in the "Y" direction. */
         mDrawLine ( M_EXPOSE , 0 , xpos1 , ypos1 , xpos2, ypos2 ) ;

      }
   }

   /* Now, draw the right and left sides of the box. */
   for ( j = 0 ; j < MAXY ; j += MAXY - 1 )
   {
      for ( i = 0 ; i < MAXX ; ++ i )
      {
         if ( i == 0 )
         {
            mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                                hrap_lat_lon [ i ] [ j ].x ,
                                & xpos1 , & ypos1 ) ;
         }
         else
         {
            xpos1 = xpos2 ;
            ypos1 = ypos2 ;
         }
     
         mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j + 1 ].y ,
                             hrap_lat_lon [ i ] [ j + 1 ].x ,
                             & xpos2 , & ypos2 ) ;

         /* Draw the line in the "Y" direction. */
         mDrawLine ( M_EXPOSE , 0 , xpos1 , ypos1 , xpos2, ypos2 ) ;

      }
   }

   mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
}

/*******************************************************************************
* MODULE NUMBER: 10 
* MODULE NAME:   _draw_hrap_area
* PURPOSE:       This routine draws and fills the area of a hrap grid for the
*                WFO or RFC currently being processed.  This boundary was
*                developed to replace the gray border currently displayed in
*                Hydroview/MPE to represent areas which are outside of the
*                MPE region of forecast responsibility.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  int         index      The number of the map library map being
*                                 displayed.
*   Input  void *      pData      A pointer to an overlay struct containing
*                                 overlay data.
*   Input  Pixmap      map        The pixmap to draw the radar sites on.
*   Input  void *      pUserData  A pointer to optional additional data.
*                                 This is not used in this routine.
*
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE      DESCRIPTION
*   getLatLonGrid        post_functions.h Returns a pointer to the array
*                                         used to convert HRAP points
*                                         to lat/lon coordinates.
*   mSetColor            map_library.h    Sets the color for the graphics
*                                         about to be drawn.
*   mConvertLatLon2XY    map_convert.h    Converts a latitude / longitude
*                                         value into a x,y coordinate on
*                                         a pixmap.
*   mDrawLine            map_library.h    Draws a line on a pixmap.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME         DESCRIPTION
*   const HRAP **     hrap_lat_lon A pointer to the array of latitude/longitude
*                                  values on a HRAP pointer basis.
*   int               i            A loop indexing variable.
*   int               j            A loop indexing variable.
*   int               xpos1        The x position of the lower point of the
*                                  line. 
*   int               xpos2        The x position of the upper point of the
*                                  line.
*   int               ypos1        The y position of the lower point of the
*                                  line.
*   int               ypos2        The y position of the upper point of the
*                                  line.
*   struct _Overlay * pOverlay     A pointer to the overlay structure which
*                                  contains the information pertinent to the
*                                  display of the hrap overlay.
*   XPoint [ ] prev_points         Added for efficiency.  Keeps track of 
*                                  previous column of HRAP points to be
*                                  processed so that they do not need to
*                                  be recomputed.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   Error messages are printed out to the standard error stream if
*   problems are encountered.  If an error occurs, then this routine will
*   not try to draw the overlay.
* 
********************************************************************************
*/
void _draw_hrap_area ( int index , void * pData ,
                       Pixmap map , void * pUserData )
{
   const HRAP ** hrap_lat_lon = NULL ;
   int i , j ;
   int num_points ;
   int point_index ;
   int xpos ;
   int ypos ;
   struct _Overlay * pOverlay = NULL ;
   static XPoint * pPolyPoints = NULL ;

   pOverlay = ( struct _Overlay * ) pData ;

   if ( pOverlay == NULL )
   {
      flogMessage ( stderr , "In routine \"_draw_hrap_boundary\":\n"
                         "NULL overlay information was passed into this\n"
                         "routine.  Could not draw the hrap grid.\n" ) ;
      return ;
   }

   /* Retrieve a pointer to the Latitude / Longitude "look up" array. */
   hrap_lat_lon = getLatLonGrid ( ) ;

   if ( hrap_lat_lon == NULL )
   {
      flogMessage ( stderr , "In routine \"_draw_hrap_boundary\":\n"
                         "The attempt to draw the HRAP grid overlay\n"
                         "has failed.  The latitude / longitude \"look up\"\n"
                         "array is undefined.  Routine \"createLatLonGrid\"\n"
                         "must first be called to create this array.\n" ) ;
      return ;
   }

   /* Set the color for the HRAP grid. */
   mSetColor ( pOverlay->color ) ;
   mSetLineWidth ( pOverlay->line_width ) ;

   /* The latitude / longitude lookup array has been found.  Draw the
      HRAP area for this WFO or RFC. */
   
   /* Collect all of the points defining the polygon which represents this
      area. */

   /* Determine the number of points required */ 
   num_points = ( 2 * MAXX ) + ( 2 * MAXY ) ;

   /* Allocate memory for the the array of XPoint structures. This
      only needs to be done once. */
   if ( pPolyPoints == NULL )
   {
      pPolyPoints = ( XPoint * ) malloc ( num_points * sizeof ( XPoint  ) ) ;

      if ( pPolyPoints == NULL )
      {
         flogMessage ( stderr , "\nIn routine '':\n"
                            "Could not allocate %d bytes of memory for\n"
                            "the array of XPoint strucutures.\n" ,
                            num_points * sizeof ( XPoint ) ) ;
         exit ( 1 ) ;
      }
   }
   
   point_index = 0 ;

   /* Process the points on the left side. */

   for ( j = 0 ; j < MAXY ; ++ j )
   {
      mConvertLatLon2XY ( hrap_lat_lon [ 0 ] [ j ].y ,
                          hrap_lat_lon [ 0 ] [ j ].x ,
                          & xpos , & ypos ) ;
     
      pPolyPoints [ point_index ].x = ( short ) xpos ;
      pPolyPoints [ point_index ++ ].y = ( short ) ypos ;

   }

   /* Now, draw the top side of the box. */
   j = MAXY - 1 ;
   for ( i = 0 ; i < MAXX ; ++ i )
   {
      mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                          hrap_lat_lon [ i ] [ j ].x ,
                          & xpos , & ypos ) ;

      pPolyPoints [ point_index ].x = ( short ) xpos ;
      pPolyPoints [ point_index ++ ].y = ( short ) ypos ;

   }

   /* Process the points on the right side. */
   i = MAXX - 1 ;

   for ( j = MAXY - 1 ; j >= 0 ; -- j )
   {
      mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                          hrap_lat_lon [ i ] [ j ].x ,
                          & xpos , & ypos ) ;
     
      pPolyPoints [ point_index ].x = ( short ) xpos ;
      pPolyPoints [ point_index ++ ].y = ( short ) ypos ;

   }

   /* Now, draw the bottom of the box. */
   j = 0 ;

   for ( i = MAXX - 1 ; i >= 0 ; -- i )
   {
      mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                          hrap_lat_lon [ i ] [ j ].x ,
                          & xpos , & ypos ) ;

      pPolyPoints [ point_index ].x = ( short ) xpos ;
      pPolyPoints [ point_index ++ ].y = ( short ) ypos ;

   }

   /* Fill the Polygon. */
   mSetColor ( "White" ) ;
   mDrawFillPolygon ( M_EXPOSE , 0 , pPolyPoints , point_index - 1 ,
                      Nonconvex , CoordModeOrigin ) ;

   mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
