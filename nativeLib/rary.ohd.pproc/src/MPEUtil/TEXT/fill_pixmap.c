
/*******************************************************************************
* FILENAME:            fill_pixmap.c
* NUMBER OF MODULES:   11
* GENERAL INFORMATION:
*   MODULE 1:          isThereMpeData
* DESCRIPTION:          Returns a value of '0' if there is no Mpe data to draw.
*                       Returns a value of '1' if there is Mpe data to draw.
*   MODULE 2:          turnOffMpeData
* DESCRIPTION:          Turns "off" the Mpe data.  This prevents it from being
*                       drawn by the map library exposure routines.
*   MODULE 3:          turnOnMpeData
* DESCRIPTION:          Turns "on" the Mpe data so that it is recognized and
*                       drawn by the map library routines.
*   MODULE 4:          getMpeDataPixmap
* DESCRIPTION:          Returns the pixmap upon which the Mpe data has been
*                       drawn.
*   MODULE 5:          getMpeDataGC
* DESCRIPTION:          Returns the graphic context of the Mpe Data.
*   MODULE 6:          createLatLonGrid
* DESCRIPTION:          Creates the latitude / longitude "look up" array
*                       for the Hrap grid.  Each point on the grid is
*                       represented by a latitude longitude pair in this
*                       array.
*   MODULE 7:          getLatLonGrid
* DESCRIPTION:          Returns a pointer to the latitude / longitude 
*                       "look up" array.
*   MODULE 8:          freeLatLonGrid
* DESCRIPTION:          Frees the memory dynamically allocated to create the
*                       the latitude / longitude array.
*   MODULE 9:          plotHrapData
* DESCRIPTION:          Plots the Hrap data.
*   MODULE 10:         fill_pixmap
* DESCRIPTION:          Processed the pixmap onto which the Hrap data is 
*                       drawn.
*   MODULE 11:         setDrawStruct 
* DESCRIPTION:         Sets the drawing structure ( initialized by the
*                      calling routine ) to be used by fill_pixmap.
*   MODULE 12:         setPixmapToDraw
* DESCRIPTION:         Sets the pixmap to use.  This overrides the default 
*                      behavior of the "fill_pixmap" routine which is 
*                      to recompute the pixmap.
*   MODULE 13:         unsetPixmapToDraw
* DESCRIPTION:         Returns the "fill_pixmap" routine to its original
*                      behavior which is to recompute the pixmap from the
*                      stored data structure.
*   MODULE 14:         getDrawStruct
* DESCRIPTION:         Returns the draw struct currently being maintained by
*                      the fill_pixmap routines.  The user is given a pointer
*                      to this structure and has the liberty to modify it.
*                      This is generally not a good practice, but it will
*                      make this program function more efficiently. 
*                     
*
* ORIGINAL AUTHOR:     Unknown
* CREATION DATE:       Unknown
* ORGANIZATION:        HSEB / OHD
* MACHINE:             HP UNIX / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #    DATE              PROGRAMMER        DESCRIPTION/REASON
*     1 - 10    February 19, 2002 Bryon Lawrence    Major revision and
*                                                   creation of new routines
*                                                   to plot the Hrap data.  
*         11    March 19, 2002    Bryon Lawrence    New routine that allows
*                                                   the user to set the
*                                                   drawing structure to
*                                                   be used to fill the pixmap.
*         12    March 22, 2002    Bryon Lawrence    New routine that allows the
*                                                   user to supply new 
*                                                   values for
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>

#include "drawa.h"
#include "HydroStatus.h"
#include "map.h"
#include "map_convert.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "mpe_field_names.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "stage3_interface.h"
#include "post_functions.h"

/* The scope of these variables is limited to this file.  An attempt is
   being made to encapsulate this data and force the user to use the 
   provided accessor functions where appropriate. */
static draw_struct * data = NULL ;
static GC mpe_GC = 0 ; /* The graphics context of the mpe data. */
static HRAP ** hrap_to_lat_lon = NULL ; /* The lat/lon "look up" array. */
static int first_hrap = 0 ; /* Ensures that the lat/lon look up" array
                               is defined only once. */
static int max_hrap_columns = 0 ; /* The maximum number of columns in a
                                     Hrap grid. */
static int max_hrap_rows = 0 ; /* The maximum number of rows in a
                                  Hrap grid. */
static int mpe_data_to_draw = 0 ; /* Flag indicating whether or not there
                                     is mpe data to draw. */
static int use_predefined_pixmap = 0 ; /* Flag indicating whether or not 
                                          to just use a user-defined
                                          pixmap. */
static Pixmap mpe_pixmap = 0 ; /* The pixmap containing the mpe data. */ 
static point ** pixel_grid = NULL ; /* The grid of pixels which has a 
                                       one to one correspondence with
                                       the array of latitude / 
                                              longitudes. */

static Boolean plotDataAsImage = True;
static Boolean plotDataAsContour = False;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   isThereMpeData
* PURPOSE:       This is an accessor function that provides the user access 
*                to the flag which indicates whether or not there is
*                any Mpe data to draw.  A value of '0' means that there is
*                no data.  A value of '1' means that there is data. 
*
* ARGUMENTS:
*    None
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*    None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None
*
* DATA FILES AND/OR DATABASE:
*    None Required.
*
* ERROR HANDLING:
*    None Required.
*
********************************************************************************
*/
int isThereMpeData ( ) 
{
   return mpe_data_to_draw ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   turnOffMpeData
* PURPOSE:       Sets the Mpe data flag to '0' indicating that there is no
*                Mpe data to draw.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*    None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None
*
* DATA FILES AND/OR DATABASE:
*    None Required.
*
* ERROR HANDLING:
*    None Required.
*
********************************************************************************
*/
void turnOffMpeData ( )
{
   mpe_data_to_draw = 0 ;
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   turnOnMpeData
* PURPOSE:       This routine sets the Mpe data flag to '1'.  This indicates
*                to callers of the "isThereMpeData" routine that there is
*                Mpe data to draw or process.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*    None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None  
*
* DATA FILES AND/OR DATABASE:
*    None Required.
*
* ERROR HANDLING:
*    None Required.
*
********************************************************************************
*/
void turnOnMpeData ( ) 
{
   mpe_data_to_draw = 1 ;
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   getMpeDataPixmap
* PURPOSE:       This routine returns the pixmap containing the drawn Mpe data.
*                If there is no Mpe data to draw, then this routine returns a
*                value of '0'.
*
* ARGUMENTS:
*    None.
*
* RETURNS:
*    DATA TYPE   NAME    DESCRIPTION
*    Pixmap      N/A     The pixmap containing the drawn Mpe data.             
*
* APIs UTILIZED:
*    None.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None.   
*
* DATA FILES AND/OR DATABASE:
*    None.
*
* ERROR HANDLING:
*    This routine will return a value of '0' if the pixmap is not defined
*    or if there is no Mpe data to be drawn.
********************************************************************************
*/
Pixmap getMpeDataPixmap ( Pixmap user_pixmap )
{
   if ( ( data != NULL ) && ( mpe_data_to_draw == 1 ) )
   {
      if ( use_predefined_pixmap != 1 )
      { 
         fill_pixmap ( user_pixmap ) ;
      }
   }
   
   return mpe_data_to_draw ? mpe_pixmap : 0 ;
}

/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   getMpeDataGC
* PURPOSE:       Returns the Graphics Context (GC) of the Mpe data for 
*                X-Motif drawing purposes.
*
* ARGUMENTS:
*    None.
*
* RETURNS:
*    DATA TYPE   NAME       DESCRIPTION
*    GC          N/A        The Graphics Context of the Mpe Data to be
*                          drawn.
* APIs UTILIZED:
*    None.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None. 
*
* DATA FILES AND/OR DATABASE:
*    None Required.
*
* ERROR HANDLING:
*    None Required.
*
********************************************************************************
*/
GC getMpeDataGC ( )
{
   return mpe_data_to_draw ? mpe_GC : 0 ;
}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   createLatLonGrid
* PURPOSE:       This routine dynamically creates the "look up" array
*                that maps lat/lon coordinates to each hrap bin corner.
*                This routine only needs to be called once from within
*                a program.  When done using the array, the user MUST
*                remember to free the memory it uses by calling the
*                freeLatLonGrid routine.
*
*                This routine must be called before the Mpe data can be
*                successfully plotted and displayed.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME           DESCRIPTION/UNITS
*   Input  int         hrap_origin_x  The origin Hrap x coordinate for the RFC
*                                     based on the national Hrap grid.
*   Input  int         hrap_origin_y  The origin y coordinate for the RFC
*                                     Hrap grid based on the national Hrap
*                                     grid. 
*   Input  int         max_hrap_x     The maximum x coordinate for the Hrap
*                                     grid taking 0 as the x origin.
*   Input  int         max_hrap_y     The maximum y coordinate for the Hrap
*                                     grid taking 0 as the y origin.
*
* RETURNS:
*   DATA TYPE   NAME    DESCRIPTION
*   HydroStatus N/A     The status of the creation of the lat/lon grid.
*
* APIs UTILIZED:
*   Only system utilities are used.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME        DESCRIPTION
*   HRAP       hrap_point  An HRAP point.  Contains the x,y point to be
*                                          converted into a longitude /
*                                          latitude pair.
*   int        i                           A loop indexing variable.
*   int        j                           A loop indexing variable.
*
* DATA FILES AND/OR DATABASE:
*   None Required.
*
* ERROR HANDLING:
*    ERROR CODE            DESCRIPTION
*    HydroStatus_BadMalloc An error was encountered allocating memory for the
*                          latitude / longitude grid.
*    HydroStatus_OK        The routine functioned within the bounds of the
*                          error checks.
********************************************************************************
*/
HydroStatus createLatLonGrid ( int hrap_origin_x , int hrap_origin_y , 
                               int max_hrap_x , int max_hrap_y )
{
  point hrap_point ;
  int i ;
  int j ;

  /* On the first time this routine is called, the hrap_to_lat_lon
     array containing the Lat/Lon coordinates for the corners of the
     Hrap bin must be dynamically created.  The caller of this routine
     MUST remember to free this memory when done using it by calling 
     the freeLatLonGrid routine. */
  if ( first_hrap == 0 )
  {
     /* Define the static maximum number of columns value that will be
        needed when the hrap_to_lat_lon array is later deallocated. */ 
     max_hrap_columns = max_hrap_x ;
     max_hrap_rows = max_hrap_y ;

     /* Define the number of columns in the array. This is the x-axis. */
     hrap_to_lat_lon = ( HRAP ** ) malloc ( ( max_hrap_x + 1 ) * 
                                              sizeof ( HRAP * ) ) ;

     if ( hrap_to_lat_lon == NULL )
     {
        return HydroStatus_BadMalloc ;
     } 

     for ( i = 0 ; i <= max_hrap_x ; ++i ) 
     {
        /* For each column in the HRAP grid define the rows in it.  This
           is the y axis. */
        hrap_to_lat_lon [ i ] = ( HRAP * ) malloc ( ( max_hrap_y + 1 ) * 
                                                      sizeof ( HRAP ) ) ;
        
        if ( hrap_to_lat_lon [ i ] == NULL )
        {
           for ( j = 0 ; j < i ; ++j )
           {
              free ( hrap_to_lat_lon [ j ] ) ;
           }

           free ( hrap_to_lat_lon ) ;

           return HydroStatus_BadMalloc ;
           
        }
     } 


     for ( i = 0 ; i <= max_hrap_x ; ++i )
     {
        hrap_point.x = ( float ) ( i + hrap_origin_x ) ;

        for ( j = 0 ; j <= max_hrap_y ; ++j )
        {
           hrap_point.y = ( float ) ( hrap_origin_y + j ) ;
           hrap_to_lat_lon [ i ] [ j ] = HrapToLatLongMpe ( hrap_point ) ;

           /* The "map library" routines expect Western Hemisphere longitudes
              to be negative.  "HrapToLatLongMpe" makes Western Hemisphere
              longitudes positive.  Negate these longitudes here so that the
              "map library" routines don't flip out. */
           hrap_to_lat_lon [ i ] [ j ].x *= -1 ;
        }
     }

     first_hrap = 1 ; 

  }

  return HydroStatus_OK ;
} 

/*******************************************************************************
* MODULE NUMBER:  7
* MODULE NAME:    getLatLonGrid
* PURPOSE:        Returns a constant pointer to the Hrap lat / lon 
*                 "look up" array.
*
* ARGUMENTS:
*    None.
*
* RETURNS:
*   DATA TYPE      NAME             DESCRIPTION
*   const HRAP **  hrap_to_lat_lon  The latitude / longitude "look up" array
*                                   required to convert HRAP points to
*                                   Cartesian coordinates for drawing on
*                                   a pixmap.
* APIs UTILIZED:
*   None.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None.
*
* DATA FILES AND/OR DATABASE:
*   None Required.
*
* ERROR HANDLING:
*   If there is no hrap_to_lat_lon array defined, then this routine will
*   return a NULL pointer.  This situation can be remedied by calling 
*   the "createLatLonGrid" routine.  
*
********************************************************************************
*/
const HRAP ** getLatLonGrid ( )
{
   return ( const HRAP ** ) hrap_to_lat_lon ;
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   freeLatLonGrid
* PURPOSE:       This routine destroys the latitude / longitude "look up"
*                array and returns the memory to the operating system.
*
* ARGUMENTS:
*    None.
*
* RETURNS:
*    None.
*
* APIs UTILIZED:
*    Only system routines are used here.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    DATA TYPE  NAME    DESCRIPTION
*    int        i       A loop indexing variable.
*
* DATA FILES AND/OR DATABASE:
*    None Required.
*
* ERROR HANDLING:
*    None Required.
*
********************************************************************************
*/
void freeLatLonGrid ( )
{
   int i ;

   if ( hrap_to_lat_lon != NULL )
   {
      for ( i = 0 ; i <= max_hrap_columns ; ++ i )
      {
          free ( hrap_to_lat_lon [ i ] ) ;
      }

      free ( hrap_to_lat_lon ) ;
      hrap_to_lat_lon = NULL ;
   }
}

/*******************************************************************************
* MODULE NUMBER: 9
* MODULE NAME:   plotHrapData
* PURPOSE:       This routine draws the HRAP multisensor precipitation
*                data onto the base pixmap pointed to by the data
*                draw_struct pointer.  This routine takes into
*                consideration the fact that many different map
*                projections may be used by the user and the
*                orientation of the base map is completely user
*                configurable based upon whichever "pan" and "zoom"
*                options he has elected to use.  Hence, while plotting
*                data, the latitude and longitude of the four corners
*                of each HRAP bin must be computed in order to ensure
*                that it is drawn in the correct position and
*                orientation on the map.
*                                            
*                It is important to note that HRAP coordinates are
*                based upon the the lower left corner with the rows of
*                data being read from left to right from the bottom of
*                the grid up.  This must be taken into consideration
*                while plotting the data.
*
*                This routine employs an algorithm to ensure that all
*                Hrap points are converted to x,y cartesian coordinates
*                at most ONCE.  As much efficiency as possible must be 
*                implemented in this routine as it is very computational
*                intensive.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME    DESCRIPTION/UNITS
*   Input  draw_struct * data    The data pertaining to the Mpe product being
*                                drawn.  The "draw_struct" structure is
*                                defined in drawa.h .
* RETURNS:
*   Nothing ... Nothing at all.
*
* APIs UTILIZED:
*   NAME              HEADER FILE      DESCRIPTION
*   getLatLonGrid     post_functions.h Returns a handle to the lat / lon
*                                      "look up" array.
*   get_vip_level                      Returns the vip level of the datum to
*                                      be plotted. 
*   mConvertLatLon2XY                  Converts a latitude / longitude
*                                      value to a x, y coordinate to be
*                                      plotted on a "drawable".
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE     NAME              DESCRIPTION
*   Boolean       contiguous
*   Display *     display           The display for X-Motif drawing purposes.
*   const HRAP ** hrap_lat_lon      The array of lat / lon values
*                                   corresponding to each vertex in the
*                                   HRAP grid.
*   int           i                 A looping variable.
*   int           j                 A looping variable.
*   int           vip               The vip level corresponding to the HRAP
*                                   bin currently being processed.
*   int           xpos              The x coordinate of the converted lat
*                                   lon pair. 
*   int           ypos              The y coordinate of the converted lat 
*                                   lon pair.
*   XPoint [ ]    points            An array of XPoint data elements defining
*                                   a HRAP bin.
*   XPoint [ ]    prev_right_points An array of XPoint data elements defining
*                                   the right side of the previous column
*                                   of HRAP bins to be processed.
*
* DATA FILES AND/OR DATABASE:
*   None Required.
*
* ERROR HANDLING:
*   If the latitude / longitude "look up" array is not defined, then
*   this routine logs an error message to the standard error stream
*   and quits drawing the multisensor precipitation data. 
*
********************************************************************************
*/
static void plotHrapData ( draw_struct * data,
                           Pixmap user_pixmap ) 
{
  Boolean contiguous = TRUE ;
  Display * display = XtDisplay ( data->w ) ;
  int count = 0 ;
  int i ;
  int j ;
  int vip ;
  XPoint points [ NUM_CORNERS_PER_HRAP_BIN ] ;
  const point ** pixel_grid = NULL ; 

  /* Retrieve the grid of pixels corresponding to the lat / lon 
     lookup array. */
  pixel_grid = getPixelGrid ( ) ;

  if ( pixel_grid == NULL )
  {
     flogMessage ( stderr, "In routine 'plotHrapData':\n"
                       "Could not retrieve the pixel grid. Make sure that\n"
                       "the latitude/longitude array has been allocated\n" ) ;
     return ;
  }
   
  enum DisplayFieldData display_field_type ;
  display_field_type = rad_data [ user_pixmap ].field_type;
   
  for ( i = 0 ; i < data->maximum_columns ; i++ )
  {
     for ( j = 0 ; j < data->maximum_rows ; j++ )
     {
         switch(display_field_type)
         {
         case display_diffField:
         case display_ratioField:


             vip =  get_vip_level_allow_negative ( data->num_levels , data->levels ,
                                 data->data_array [ i ] [ j ] ) ;
             break;

         default:
        /* Retrieve the vip level for the HRAP bin. */
        vip =  get_vip_level ( data->num_levels , data->levels ,
                               data->data_array [ i ] [ j ] ) ;
         } //end switch

        /* Don't bother drawing a vip level of 1.  The MPE forecast area
           has already been colored as this level. Or there is a user-supplied
           background which we don't want to overwrite. */
        if ( user_pixmap == 0 )
        {
           if ( vip == 1 )
           {
              contiguous = False ;
              continue ;
           }
        }
        else
        {
           /* A user supplied pixmap containing data such as
              topography was supplied.  Do not draw missing or no
              data colors. */
           if ( vip == 0 || vip == 1 )
           {
              contiguous = False ;
              continue ;
           }
        }

        ++ count ;

        /* Determine the four points bounding this HRAP bin. 
           Work counter-clockwise around the HRAP bin.  This algorithm
           has been developed to make this operation as efficient as 
           possible.  Note that the mConvertLatLon2XY routine is an
           "inline" function which should speed things up a little 
           bit more. */
        if ( j == 0 || contiguous == False )
        {
           points [ BOTTOM_RIGHT_CORNER ].x = pixel_grid [ i + 1 ] [ j ].x ;
           points [ BOTTOM_RIGHT_CORNER ].y = pixel_grid [ i + 1 ] [ j ].y ;

           points [ BOTTOM_LEFT_CORNER ].x = pixel_grid [ i ] [ j ].x ;
           points [ BOTTOM_LEFT_CORNER ].y = pixel_grid [ i ] [ j ].y ;
        }
        else
        {
           points [ BOTTOM_RIGHT_CORNER ] = points [ TOP_RIGHT_CORNER ] ;
           points [ BOTTOM_LEFT_CORNER ] = points [ TOP_LEFT_CORNER ] ; 
        }

        contiguous = True ;

        points [ TOP_LEFT_CORNER ].x = pixel_grid [ i ] [ j + 1 ].x ; 
        points [ TOP_LEFT_CORNER ].y = pixel_grid [ i ] [ j + 1 ].y ;

        points [ TOP_RIGHT_CORNER ].x = pixel_grid [ i + 1 ] [ j + 1 ].x ;
        points [ TOP_RIGHT_CORNER ].y = pixel_grid [ i + 1 ] [ j + 1 ].y ;

        /* Draw the filled polygon. Need to use the X routine directly
           here because the Graphics Context (GC) of each point differs
           depending on its vip level.  The map library routines would 
           assume that each point has the same GC. */
        XFillPolygon ( display , data->pixbase , data->gc [ vip ] ,
                       points , NUM_CORNERS_PER_HRAP_BIN , 
                       Convex , CoordModeOrigin ) ;
     }

  }

  rectangleWidth = data->maximum_columns + XOR ;

  flogMessage ( stdout , " The number of grid boxes drawn is %d.\n" , count ) ; 
}

/*******************************************************************************
* MODULE NUMBER: 9
* MODULE NAME:   plotContours
* PURPOSE:       Displays the MPE gridded field as a contoured field.
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
static void plotContours ( draw_struct * data,
                           Pixmap user_pixmap ) 
{
   Display * map_display = NULL;
   int h;
   int i, j, k, m, l;
   int di[5], dj[5], dy, dx, pnum, kk, mm;
   float value[5];
   float x[5], y[5];
   int pixelx[5];
   int pixely[5];
   int dup_flag[5];
   const point ** pixel_grid = NULL;
   XGCValues gcvalues;

   map_display = _get_map_display ( );

   /* Retrieve the grid of pixels corresponding to the lat/lon
      lookup array. */
   pixel_grid = getPixelGrid ( );

   if ( pixel_grid == NULL )
   {
      flogMessage ( stderr, "In routine 'plotContours':\n"
                        "Could not retrieve the pixel grid.  Make sure that\n"
                        "the latitude/longitude array has been allocated.\n" );
      return;
   }

   dx = 1;
   dy = 1;

   /* Define the i and j increments to be added to define the corners of
      on grid cell in the map to be contoured. */
   di[0] = 0;
   dj[0] = 0;
   di[1] = 1;
   dj[1] = 0;
   di[2] = 1;
   dj[2] = 1;
   di[3] = 0;
   dj[3] = 1;
   di[4] = 0;
   dj[4] = 0;


   /* Process every hrap grid cell except the last one
      in a row and the last one in a column. */
   for (i = 0; i < data->maximum_columns - 1; i++)
   {
      for (j = 0; j < data->maximum_rows - 1; j++)
      {

         /* Retrieve the values of the first square of four
	    cells. */
	 value[0] = (float) data->data_array[i][j];
	 value[1] = (float) data->data_array[i + 1][j];
	 value[2] = (float) data->data_array[i + 1][j + 1];
	 value[3] = (float) data->data_array[i][j + 1];

         /* If the value of these cells are too small, do not process
            this set of points! */
	 if (value[0] < 0.0 &&
	     value[1] < 0.0 &&
	     value[2] < 0.0 &&
	     value[3] < 0.0)
         {
	    continue;
         }

	 value[4] = value[0];

         /* Loop over each of the potential contour thresholds. */
	 for (l = 0; l < data->num_levels; l++)
	 {
            /* Initialize the count of the number of points found to 0. */
	    pnum = 0;

            /* Loop over the each of the 4 sides of the square */
	    for (m = 0; m < 4; m++)
	    {

               /* Test to determine if a contour line intersects
	          this side of the box. */
	       if ((data->levels[l] >= value[m] &&
		    data->levels[l] < value[m + 1]) ||
		   (data->levels[l] < value[m] &&
		    data->levels[l] >= value[m + 1]))
	       {

                  /* A contour is not drawn between points of 
		     equal value. */
		  if (value[m] == value[m + 1])
                  {
		     continue;
                  }

                  /* Determine the x and y coordinates of the intersection point. */
		  x[pnum] = (float)pixel_grid[i + di[m + 1]][j + dj[m + 1]].x +
		            (float)(pixel_grid[i + di[m]][j + dj[m]].x -
			     pixel_grid[i + di[m + 1]][j + dj[m + 1]].x) /
		            (value[m] - value[m + 1]) * (data->levels[l] -
						  value[m + 1]);

		  y[pnum] = (float)pixel_grid[i + di[m + 1]][j + dj[m + 1]].y +
		           (float)(pixel_grid[i + di[m]][j + dj[m]].y -
			   pixel_grid[i + di[m + 1]][j + dj[m + 1]].y) /
		           (value[m] - value[m + 1]) * (data->levels[l] -
						  value[m + 1]);

                  /* If this point is the same as the one before it,
                     eliminate it ... it is a duplicate. */
		  for (h = 0; h < pnum; h++)
		  {
		     if (x[h] == x[pnum] && y[h] == y[pnum])
                     {
                        continue;
                     }
		  }

                  /* Increment the point count. */
		  pnum++;
	       }

	    }

            /* If there are three points, check for and eliminate duplicate points. */
            if ( pnum == 3 )
            {
               for (kk = 0; kk < pnum - 1; kk++)
               {
	          dup_flag[kk] = 0;

	          for (mm = 1; mm < pnum; mm++)
	          {
                     if ( x[kk] == x[mm] )
                     {
                        dup_flag [kk] = 1;
                     }
                  }
	       }

	       mm = 0;

	       for (kk = 0; kk < pnum; kk++)
	       {
		  if (dup_flag[kk] == 0)
		  {
		     x[mm] = x[kk];
		     y[mm++] = y[kk];
		  }
	       }

	       pnum = mm;
            }

            /* Need either two or 4 points for contouring. */
	    if (pnum != 2 && pnum != 4)
            {
	       continue;
            }

            /* Convert to nearest pixel value. */
            for ( kk = 0; kk < pnum; ++kk )
            {
                pixelx[kk] = (int) (x[kk] + 0.5);
                pixely[kk] = (int) (y[kk] + 0.5);
            }
      
            /* Draw the contour line segment. */
	    if (pnum == 2)
            {
               XDrawLine ( map_display, data->pixbase, data->gc[l],
                           pixelx[0], pixely[0], pixelx[1], pixely[1]);
            }
	    else
	    {

	       if (data->levels[l] < value[1])
	       {
                  XDrawLine ( map_display, data->pixbase, data->gc[l],
		              pixelx[1], pixely[1], pixelx[2], pixely[2]);
                  XDrawLine ( map_display, data->pixbase, data->gc[l],
		              pixelx[3], pixely[3], pixelx[0], pixely[0]);
	       }
	       else
	       {
                  XDrawLine ( map_display, data->pixbase, data->gc[l],
		              pixelx[0], pixely[0], pixelx[1], pixely[1]);
                  XDrawLine ( map_display, data->pixbase, data->gc[l],
		              pixelx[2], pixely[2], pixelx[3], pixely[3]);
	       }
	    }
	 }

      }
   }

   return;

}

/*******************************************************************************
* MODULE NUMBER: 10
* MODULE NAME:   drawHrapArea
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

static void drawHrapArea ( Display * display )
{
   const HRAP ** hrap_lat_lon = NULL ;
   int i , j ;
   int vip_level = 1 ;
   int num_points = ( 2 * ( MAXX + 1 ) ) + ( 2 * ( MAXY + 1 ) ) ;
   int point_index ;
   int xpos ;
   int ypos ;
   static XPoint * pPolyPoints = NULL ;

   /* Retrieve a pointer to the Latitude / Longitude "look up" array. */
   hrap_lat_lon = getLatLonGrid ( ) ;

   if ( hrap_lat_lon == NULL )
   {
      flogMessage ( stderr , "In routine \"drawHrapArea\":\n"
                         "The attempt to draw the HRAP grid overlay\n"
                         "has failed.  The latitude / longitude \"look up\"\n"
                         "array is undefined.  Routine \"createLatLonGrid\"\n"
                         "must first be called to create this array.\n" ) ;
      return ;
   }

   /* Set the color for the HRAP grid. */

   /* Collect all of the points defining the polygon which represents this
      area. */

   /* Allocate memory for the the array of XPoint structures. This
      only needs to be done once since the number of HRAP points 
      defining the boundary of the  */
   if ( pPolyPoints == NULL )
   {
      pPolyPoints = ( XPoint * ) malloc ( num_points * sizeof ( XPoint ) ) ;

      if ( pPolyPoints == NULL )
      {
         flogMessage ( stderr , "\nIn routine '':\n"
                            "Could not allocate %d bytes of memory for\n"
                            "the array of XPoint structures.\n" ,
                            num_points * sizeof ( XPoint ) ) ;
         exit ( 1 ) ;
      }
   }
   
   point_index = 0 ;

   /* Process the points on the left side. */

   for ( j = 0 ; j <= MAXY ; ++ j )
   {
      mConvertLatLon2XY ( hrap_lat_lon [ 0 ] [ j ].y ,
                          hrap_lat_lon [ 0 ] [ j ].x ,
                          & xpos , & ypos ) ;
     
      pPolyPoints [ point_index ].x = ( short ) xpos ;
      pPolyPoints [ point_index ++ ].y = ( short ) ypos ;

   }

   /* Now, draw the top side of the box. */
   j = MAXY ;

   for ( i = 0 ; i <= MAXX ; ++ i )
   {
      mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                          hrap_lat_lon [ i ] [ j ].x ,
                          & xpos , & ypos ) ;

      pPolyPoints [ point_index ].x = ( short ) xpos ;
      pPolyPoints [ point_index ++ ].y = ( short ) ypos ;

   }

   /* Process the points on the right side. */
   i = MAXX ;

   for ( j = MAXY ; j >= 0 ; -- j )
   {
      mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                          hrap_lat_lon [ i ] [ j ].x ,
                          & xpos , & ypos ) ;
     
      pPolyPoints [ point_index ].x = ( short ) xpos ;
      pPolyPoints [ point_index ++ ].y = ( short ) ypos ;

   }

   /* Now, draw the bottom of the box. */
   j = 0 ;

   for ( i = MAXX ; i >= 0 ; -- i )
   {
      mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                          hrap_lat_lon [ i ] [ j ].x ,
                          & xpos , & ypos ) ;

      pPolyPoints [ point_index ].x = ( short ) xpos ;
      pPolyPoints [ point_index ++ ].y = ( short ) ypos ;

   }

   /* Fill the Polygon. */
   XFillPolygon ( display , data->pixbase , data->gc [ vip_level ] ,
                  pPolyPoints , point_index - 1 , Nonconvex , 
                  CoordModeOrigin ) ;
}

/*******************************************************************************
* MODULE NUMBER: 11
* MODULE NAME:   fill_pixmap
* PURPOSE:       Prepares the Hrap pixmap for data plotting.
*
* ARGUMENTS:
*   TYPE   DATA TYPE                     NAME      DESCRIPTION/UNITS
*   Input  Widget                        w         The parent widget.
*   Input  draw_struct *                 data      Data pertaining to the Mpe
*                                                  product to be drawn.
*   Input  XmDrawingAreaCallbackStruct * call_data Not Used.
* RETURNS:
*   Nothing.
*
* APIs UTILIZED:
*   NAME                 HEADER FILE      DESCRIPTION
*   _get_map_width       map.h            Returns the width of the map screen.
*   _get_map_height      map.h            Returns the height of the map screen.
*   get_pixel_by_name    stage3_globals.h Returns the pixel representing a 
*                                         certain color.
*   plotHrapData         post_functions.h Plots the Hrap data on the Hrap
*                                         pixmap contained with the 
*                                         data_struct.
*   mUpdateMap           map_library.h    Caused the map screen to be redrawn.
*                                         This must be done. Otherwise, the 
*                                         Hrap data would not be visible.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME       DESCRIPTION
*   Dimension  height     The height of the map screen.
*   Dimension  width      The width of the map screen.
*   Display  * dpy        The display for X-Motif drawing purposes.
*   int        mask       The mask to use when creating a Graphics 
*                         Context.
*   GC         backgc     The Graphics context used to "clear" the pixmap
*                         before drawing.
*   XGCValues  gcv        A stucture containing the various user-configurable
*                         values of a Graphics Context.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
********************************************************************************
*/

void fill_pixmap ( Pixmap user_pixmap )
{
   Dimension                    width, height;
   XGCValues                    gcv;
   int                          mask = GCForeground;
   GC                           backgc;
   Display                     * dpy = NULL ;

   if ( data == NULL )
   {
     return ;
   }

 /*--------------------------------------------------------------*/
 /*     Get the width and height of the drawing area             */
 /*--------------------------------------------------------------*/
  width = _get_map_width ( 0 ) ;
  height = _get_map_height ( 0 ) ;

  screenWidth = width ;
  mapWidth = width ;
  mapHeight = height ;
  
  dpy = XtDisplay ( data->w ) ;

 /*--------------------------------------------------------------*/
 /*     Free the old pixmap and create one the size of the window*/
 /*--------------------------------------------------------------*/
 
  if ( data->pixbase ) 
  {
     XFreePixmap ( dpy , data->pixbase ) ;
     data->pixbase = 0 ;
  }

  data->pixbase = XCreatePixmap ( dpy ,
			          DefaultRootWindow ( dpy ) ,
			          width , height ,
			          DefaultDepthOfScreen ( XtScreen ( data->w ) )
			        ) ;

  mpe_pixmap = data->pixbase ;
  mpe_GC = data->gc[0] ;

  /* If the user has not supplied a pixmap, then fill the new pixmap 
     to "clear" it out.*/ 
  if ( user_pixmap == 0 )
  {
     gcv.foreground = get_pixel_by_name ( data->w , "dim grey" ) ;

     backgc = XCreateGC ( dpy , DefaultRootWindow ( dpy ) , mask , & gcv ) ;
     XFillRectangle ( dpy , data->pixbase , 
                      backgc , 0 , 0 , width , height ) ;
     XFreeGC ( dpy , backgc ) ;

     /* Draw the area covered by the forecast grid. */
     drawHrapArea ( dpy ) ;
  }
  else
  {
     /* Do not clear the new pixmap out.  Copy the user-supplied pixmap
        to the new pixmap.  Then draw on top of it. */
     XCopyArea ( dpy, user_pixmap, data->pixbase, mpe_GC, 0, 0, width, 
                 height, 0 , 0 );
  }


  /* Plot the HrapData onto the data->pixbase pixmap.  This pixmap
     will later be copied onto the Hmap_mpe viewer window for display. */

  /* Determine if the user wants to display the MPE field
     as an image or as a contoured field. */
  if ( plotDataAsImage == True )
  {
     plotHrapData ( data, user_pixmap ) ;
  }

  if ( plotDataAsContour == True )
  {
     plotContours ( data, user_pixmap );
  }

}

/*******************************************************************************
* MODULE NUMBER: 12
* MODULE NAME:   setDrawStruct
* PURPOSE:       This routine allows the user to set the drawing structure
*                which will provide data to fill the pixmap.
*
* ARGUMENTS:
*   TYPE   DATA TYPE       NAME        DESCRIPTION/UNITS
*   Input  draw_struct *   draw_data   A pointer to the draw_struct object
*                                      containing displayable data.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/

void setDrawStruct ( draw_struct * draw_data )
{
   data = draw_data ;
}

/*******************************************************************************
* MODULE NUMBER: 13
* MODULE NAME:   setPixmapToDraw
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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/

void setPixmapToDraw ( Pixmap * pPixmap , GC * pGraphicsContext )
{
   use_predefined_pixmap = 1 ;
   mpe_pixmap = * pPixmap ;
   mpe_GC = * pGraphicsContext ;
}

/*******************************************************************************
* MODULE NUMBER: 14
* MODULE NAME:   unsetPixmapToDraw
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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/
void unsetPixmapToDraw ( )
{
   use_predefined_pixmap = 0 ;
}

/*******************************************************************************
* MODULE NUMBER: 15
* MODULE NAME:   getDrawStruct
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

draw_struct * getDrawStruct ( )
{
   if ( ( mpe_data_to_draw == 0 ) || ( use_predefined_pixmap == 1) )
   {
      return NULL ;
   }
   else
   {
      return data ;
   }
}

/*******************************************************************************
* MODULE NUMBER: 16
* MODULE NAME:   getPixelGrid
* PURPOSE:       This routine returns the grid of pixel locations which 
*                cooresponds to the latitude/longitude grid.
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

const point ** getPixelGrid ( )
{
    HydroStatus status ;

    if ( pixel_grid == NULL )
    {
       status = createPixelGrid ( ) ;

       if ( status != HydroStatus_OK )
       {
          if ( status == HydroStatus_NoData ) 
          {
             flogMessage ( stderr , "\nIn routine 'getPixelGrid':\n"
                                "The latitude / longitude array has not been\n"
                                "created.  Call routine createLatLongGrid\n"
                                "to create this grid.\n" ) ;
          }
          else
          {
             flogMessage ( stderr , "\nIn routine 'getPixelGrid':\n"
                                "The call to routine createPixelGrid failed.\n"
                                "Memory could not be allocated for the\n"
                                "pixel grid.\n" ) ;
          }

          return NULL ;
        
       }
    }

    return ( const point ** ) pixel_grid ;
}

/*******************************************************************************
* MODULE NUMBER: 17
* MODULE NAME:   createPixelGrid
* PURPOSE:       This routine creates or modifies the grid of pixels which
*                corresponds to the latitude/longitude grid.
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
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   int        i                            A loop indexing variable
*   int        j                            A loop indexing variable
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

HydroStatus createPixelGrid ( )
{
   int i ;
   int j ;

   if ( hrap_to_lat_lon == NULL )
   {
      return HydroStatus_NoData ;
   }

   if ( pixel_grid == NULL )
   {
      /* Allocate memory for the pixel grid. */
      pixel_grid = ( point ** ) malloc ( ( max_hrap_columns + 1 ) *
                                            sizeof ( point * ) ) ;

      if ( pixel_grid == NULL )
      {
         return HydroStatus_BadMalloc ;
      } 

      for ( i = 0 ; i <= max_hrap_columns ; ++i ) 
      {
         /* For each column in the HRAP grid define the rows in it.  This
            is the y axis. */
         pixel_grid [ i ] = ( point * ) malloc ( ( max_hrap_rows + 1 ) * 
                                                  sizeof ( point ) ) ;
         
         if ( pixel_grid [ i ] == NULL )
         {
            for ( j = 0 ; j < i ; ++j )
            {
               free ( pixel_grid [ j ] ) ;
            }

            free ( pixel_grid ) ;
 
            return HydroStatus_BadMalloc ;
           
         }
      } 
   } 

   /* Define the pixels in this grid.  This only needs to be done when
      panning and zooming. */
   for ( i = 0 ; i <= max_hrap_columns ; ++i )
   {
      for ( j = 0 ; j <= max_hrap_rows ; ++j )
      {
         mConvertLatLon2XY ( hrap_to_lat_lon [ i ] [ j ].y ,
                             hrap_to_lat_lon [ i ] [ j ].x ,
                             & ( pixel_grid [ i ] [ j ].x ) ,
                             & ( pixel_grid [ i ] [ j ].y ) ) ;
      }
   }

   return HydroStatus_OK ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:   freePixelGrid
* PURPOSE:       This routine frees the dynamically allocated memory used
*                by the pixel grid.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*    Only system functions are called by this routine.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   int        i                            A loop indexing variable.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   No error handling is required.
*
********************************************************************************
*/
void freePixelGrid ( )
{
   int i ;

   if ( pixel_grid != NULL )
   {
      for ( i = 0 ; i <= max_hrap_columns ; ++ i )
      {
          free ( pixel_grid [ i ] ) ;
      }

      free ( pixel_grid ) ;
      pixel_grid = NULL ;
   }

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
void setToPlotImage ( Boolean state )
{
   plotDataAsImage = state;
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

void setToPlotContour ( Boolean state )
{
   plotDataAsContour = state;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEUtil/RCS/fill_pixmap.c,v $";
 static char rcs_id2[] = "$Id: fill_pixmap.c,v 1.10 2012/05/10 15:26:34 cgobs Exp $";}
/*  ===================================================  */

}
