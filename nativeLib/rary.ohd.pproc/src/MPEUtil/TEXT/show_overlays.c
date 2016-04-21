
/*******************************************************************************
* FILENAME:            show_overlays.c
* NUMBER OF MODULES:   6
* GENERAL INFORMATION:
*   MODULE 1:   _draw_bcdfile_overlay_ssradar
* DESCRIPTION:  Supplies file paths and file names of the bcd files.
*               Reads the information from bcd files for states and counties 
*		overlays. 
*   MODULE 2:   show_states
* DESCRIPTION:  Callback to toggle states overlay on the single radar site 
*               window.
*   MODULE 3:   show_county 
* DESCRIPTION:  Callback to toggle counties overlay on the single radar site 
*               window.
*   MODULE 4:   show_cities_and_towns
* DESCRIPTION:  Callback to toggle cities and towns overlay on the single
*               radar site 
*               window.
*   MODULE 5:   show_basin_boundaries
* DESCRIPTION:  Callback to toggle basin boundaries overlay on the single
*               radar site window.
*   MODULE 6:   show_rivers
* DESCRIPTION:  Callback to toggle rivers overlay on the single radar site 
*               window.
*
* ORIGINAL AUTHOR:
* CREATION DATE:    
* ORGANIZATION:     HSEB / OHD 
* MACHINE:          HP-UX / Redhat Dell Linux
*
* MODIFICATION HISTORY:
*   MODULE #   DATE       PROGRAMMER                         DESCRIPTION/REASON
*   1-6        5/21/2002  Bryon Lawrence/Moria Shebsovich    Revision
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#include "GeneralUtil.h"
#include "overlay.h"
#include "post_functions.h"
#include "stage3_globals.h"

#include "List.h"
#include "map_bcdfile.h"
#include "map_defines.h"
#include "map_menubar.h"
#include "map_menubar_cb.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"

/*** GLOBAL VARIABLES ********************************************************/

/* variables for overlays */

struct _Map_Point _map_point ;

struct _Map_Segment _map_segment ;

extern struct _Overlay overlays [ M_OVERLAYS ] ;

Boolean fillarea = False ;
extern draw_struct * ds_array [ 4 ] ;


/******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   _draw_bcdfile_overlay_ssradar
* PURPOSE:  Provides file paths and file names of the bcd files.
*           Reads the information from bcd files for states and counties 
*           overlays. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  _Overlay * overlays     The structure contains the information needed 
*                                  for drawing the overlay.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE             DESCRIPTION
*   _set_foreground               map_resource.h          Sets the color for the  
*							  item to be drawn.
*   _read_bcdfile                 map_bcdfile.h           Reads in the bcdfile 
*							  into memory.
*                                                      
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE                  NAME                       DESCRIPTION
*
*   int                         i                         Number of the n_files.
*   int                       n_files                     The number of shape 
*							  files for this overlay.
*   struct _Map_Segment *       seg                       If the user wants to
*                                                         keep the geographic
*                                                         information in
*                                                         memory for quicker 
*                                                         access, it's kept here.
* DATA FILES AND/OR DATABASE:
* This routine requires bcd files containing states and counties data for 
* reading them into the memory.  The names of these files are provided
* by the filename member of each the _Overlay structures.
* The paths of these files are provided by the filename variable 
* of the _Overlay structures. 
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if the following
* condition occurs:
* 
* 1) There are no bcd files supplied to this routine.
*
********************************************************************************
*/

static void _draw_bcdfile_overlay_ssradar ( struct _Overlay * overlays )
                                    	                                     
{ 
  int i;
  int n_files = overlays->n_files;
  struct _Map_Segment * seg = NULL;
  
  if(n_files == 0)
  {
     flogMessage ( stderr , "\nIn routine \"_draw_bcdfile_overlay_ssradar\":\n"
                         "\"n_files\" is NULL. No overlay information has\n"
                         "been provided to this routine.\n" ) ;     
     return;
  }
  
  for (i = 0; i < n_files; i++)
  {	
     char name [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;

     memset ( name , '\0' , OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ) ;

     if ( ( overlays->filepath != NULL ) && 
          ( overlays->filepath [ 0 ] != '\0' ) )
     {
        sprintf ( name , "%s/" , overlays->filepath[i] ) ;
     }

     if ( ( overlays->filename != NULL ) && 
          ( overlays->filename [ 0 ] != '\0' ) )
     {
        strcat ( name , overlays->filename[i] ) ;
     }
     else
     {
         seg = NULL ;
         return ;
     }

     _set_foreground ( overlays->color ) ;
   
      if ( _read_bcdfile ( name , & seg ) == 0 )
         return;
	 
      overlays->seg[i] = seg ;
   }
}
/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   show_rfc_boundaries
* PURPOSE:  Callback to toggle rfc_boundaries overlay on the single radar site 
*           window. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE             DESCRIPTION
*   _get_map_display              map_resource.h          Returns the display.
*   _set_first_segment            map_menubar_cb.h        Sets the head of the 
*							  segment for the given 
*    							  overlay to the current 
*                   					  segment.
*   _get_point_coordinate	  map_menubar_cb.h   	  Gets the current 
*							  segment lat lon 
*							  coordinate for the 
*							  given point.
*   _next_segment                 map_menubar_cb.h        Sets current segment  
*                                                         to next item in link 
*							  list.
*   get_pixel_by_name		  globals.h               Returns numeric color
*                                                         value by color name.
*   _get_points_in_segment        map_menubar_cb.h        Gets the number of 
*							  points in the current 
*							  segment.
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE               NAME                   DESCRIPTION
*      int 		      i                    An index for the array of 
*      						   the structures.
*      int		      j	                   A loop index variable for
*                                                  points in a segment.
*      int                    n                    An index for wargs array.
*      Arg                 wargs [ ]               Arg structure, an array of 
*                                                  arguments used for setting 
*                                                  widget resources, 
*						   dimensioned to 5.
*      int                 maxpts                  A number of points in a
*                                                  segment.
*      int                 prev_maxpts             Saved number of points
*                                                  in the previous segment.
*      int                    k                    A loop variable of data
*                                                  files to be read.
*     XPoint *             points                  A pointer to XPoint 
*                                                  structure with the 
*                                                  coordinates of the point.
*     GC                    gc                     A pointer to the Graphic 
*                                                  Context structure.                                            
*     Display *             dpy                    A pointer to the Display
*                                                  structure.
*     int             mask = GCForeground          Mask to set the foreground
*					           component of GC.
*     Dimension            width                   Dimension structure, width
*                                                  of drawing area canvas.
*     Dimension            height                  Dimension structure, height
*                                                  of drawing area canvas.
*       int                  x,y                   A number of pixels per 
*						   hrap unit.
*     XGCValues              gcv                   Specifies the values 
*						   for the fields in 
*                                                  value_mask.
*      float             latit,longt               Lat / lon coordinates
*                                                  of a point.
*      HRAP                 hrap                   Hrap structure, contains
*                                                  hrap coordinates of a
*                    			           point.
* DATA FILES AND/OR DATABASE:
* This routine reads states overlay data from the memory. If there is no data  
* in memory, this routine reads bcd file containing states data.  
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if the following
* condition occurs:
* 
* 1) Could not allocate blocks of memory of needed size 
*     for the points array of XPoint data types.
*
*******************************************************************************
*/

void show_rfc_boundaries ( Widget w, XtPointer client_data ,  
                           XtPointer call_data ) 
{
   int          i, j, n, maxpts, prev_maxpts = 0;
   int 		k = 0 ;
   int          ss_number = ( int ) client_data ;
   XPoint      *points = NULL ;
   GC           gc = NULL ;
   Display     *dpy = NULL ;
   Dimension    width, height;
   draw_struct  * pDrawMsData = NULL ;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   XGCValues    gcv;
   float        latit, longt;
   HRAP         hrap;
   
 dpy = _get_map_display ( ) ; 
 pDrawMsData = & ds_array [ MsData ] [ ss_number ] ;

 /*--------------------------------------------------------------*/
 /*     determine size of display area                           */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues ( pDrawMsData->w , wargs , n ) ;

 if (overlays[M_RFC_BOUNDARY].seg[k] == 0)
 {
    _draw_bcdfile_overlay_ssradar ( & overlays[M_RFC_BOUNDARY]) ;
                            
 }
 
 for ( k = 0 ; k < overlays[ M_RFC_BOUNDARY ].n_files ; k++ )
 {
     _set_first_segment ( overlays[M_RFC_BOUNDARY].seg[k] ) ;
 
    /*--------------------------------------------------------------*/
    /*     determine number of pixels per hrap bin                  */
    /*--------------------------------------------------------------*/

      x = (float)width/(float)pDrawMsData->maximum_columns;
      y = (float)height/(float)pDrawMsData->maximum_rows;

      if (x > y)
	 x = y;
      else if (y > x)
	 y = x;

    /*--------------------------------------------------------------*/
    /*     create graphics context for states overlay               */
    /*--------------------------------------------------------------*/

      gcv.foreground = get_pixel_by_name(w,overlays[M_RFC_BOUNDARY ].color);
      gcv.line_width = overlays[M_RFC_BOUNDARY].line_width ;
      gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

    /*--------------------------------------------------------------*/
    /*     determine number of points to be plotted & allocate space*/
    /*--------------------------------------------------------------*/

      do
      {
	 maxpts = _get_points_in_segment ( ) ; 
	 
    /*---------------------------------------------------------------------*/ 
    /*   allocate memory only in the cases:				   */
    /*			- if it has not been allocated yet; 		   */
    /*			- previously allocated memory space was smaller    */
    /*  		  than needed in the current iteration             */
    /*---------------------------------------------------------------------*/
    
	 if ( points == NULL ) 
 	      points = ( XPoint * ) realloc ( points, maxpts * sizeof ( XPoint ));
	      
         else if( maxpts > prev_maxpts )
	      points = ( XPoint * ) realloc ( points, maxpts * sizeof ( XPoint ));
	      
 	 if ( points == NULL )
 	 {
              flogMessage ( stderr , "\nIn routine \"show_rfc_boundaries\":\n"
                       "Could not allocate %d blocks of memory of size %d\n"
                       "for the \"points\" array of XPoint data types.\n"
                       "Cannot draw the RFC boundaries.\n" ,
                       maxpts , sizeof ( XPoint ) ) ;
    	      return ;
 	 }
	
         for (j = 0; j < maxpts; j++)
         {
	     _get_point_coordinate ( j , & latit , & longt ) ;
	    
	      hrap = LatLongToHrapMpe(latit, (-1) * longt);
	    
	      hrap.x -= pDrawMsData->origin.x ;
              hrap.y -= pDrawMsData->origin.y ; 
    
	      points[j].x = ( hrap.x ) * x;
              points[j].y = ( pDrawMsData->maximum_rows - hrap.y ) * y;
         }
	 
         for ( i = MsData ; i <= St1iiData ; ++ i )
         {
            XDrawLines ( dpy , ds_array [ i ] [ ss_number ].pix , 
                         gc , points , maxpts , CoordModeOrigin ) ;
         }
	 
	 prev_maxpts = maxpts ;
	 
      } while (_next_segment() != NULL);
      
 }
 
 if ( overlays [ M_RFC_BOUNDARY ].store_in_memory == ( int ) M_OFF )
 {
     _deallocate_segments ( & ( overlays [ M_RFC_BOUNDARY ].seg [ 0 ] ) ) ;
     overlays [ M_RFC_BOUNDARY ].seg [ 0 ] = NULL ;
 }
 
 free ( points ) ;
 XFreeGC ( dpy , gc ) ;

}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   show_states
* PURPOSE:  Callback to toggle states overlay on the single radar site window. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE             DESCRIPTION
*   _get_map_display              map_resource.h          Returns the display.
*   _set_first_segment            map_menubar_cb.h        Sets the head of the 
*							  segment for the given 
*    							  overlay to the current 
*                   					  segment.
*   _get_point_coordinate	  map_menubar_cb.h   	  Gets the current 
*							  segment lat lon 
*							  coordinate for the 
*							  given point.
*   _next_segment                 map_menubar_cb.h        Sets current segment  
*                                                         to next item in link 
*							  list.
*   get_pixel_by_name		  globals.h               Returns numeric color
*                                                         value by color name.
*   _get_points_in_segment        map_menubar_cb.h        Gets the number of 
*							  points in the current 
*							  segment.
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE               NAME                   DESCRIPTION
*      int 		      i                    An index for the array of 
*      						   the structures.
*      int		      j	                   A loop index variable for
*                                                  points in a segment.
*      int                    n                    An index for wargs array.
*      Arg                 wargs [ ]               Arg structure, an array of 
*                                                  arguments used for setting 
*                                                  widget resources, 
*						   dimensioned to 5.
*      int                 maxpts                  A number of points in a
*                                                  segment.
*      int                 prev_maxpts             Saved number of points
*                                                  in the previous segment.
*      int                    k                    A loop variable of data
*                                                  files to be read.
*     XPoint *             points                  A pointer to XPoint 
*                                                  structure with the 
*                                                  coordinates of the point.
*     GC                    gc                     A pointer to the Graphic 
*                                                  Context structure.
*     Display *             dpy                    A pointer to the Display
*                                                  structure.
*     int             mask = GCForeground          Mask to set the foreground
*					           component of GC.
*     Dimension            width                   Dimension structure, width
*                                                  of drawing area canvas.
*     Dimension            height                  Dimension structure, height
*                                                  of drawing area canvas.
*       int                  x,y                   A number of pixels per 
*						   hrap unit.
*     XGCValues              gcv                   Specifies the values 
*						   for the fields in 
*                                                  value_mask.
*      float             latit,longt               Lat / lon coordinates
*                                                  of a point.
*      HRAP                 hrap                   Hrap structure, contains
*                                                  hrap coordinates of a
*                    			           point.
* DATA FILES AND/OR DATABASE:
* This routine reads states overlay data from the memory. If there is no data  
* in memory, this routine reads bcd file containing states data.  
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if the following
* condition occurs:
* 
* 1) Could not allocate blocks of memory of needed size 
*     for the points array of XPoint data types.
*
*******************************************************************************
*/

void MPEUtil_show_states ( Widget w, XtPointer client_data , XtPointer call_data ) 
{
   int          i, j, n, maxpts, prev_maxpts = 0;
   int 		k = 0 ;
   int          ss_number = ( int ) client_data ;
   XPoint      *points = NULL ;
   GC           gc = NULL ;
   Display     *dpy = NULL ;
   Dimension    width, height;
   draw_struct  * pDrawMsData = NULL ;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   XGCValues    gcv;
   float        latit, longt;
   HRAP         hrap;
   
 dpy = _get_map_display ( ) ; 
 pDrawMsData = & ds_array [ MsData ] [ ss_number ] ;

 /*--------------------------------------------------------------*/
 /*     determine size of display area                           */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues ( pDrawMsData->w , wargs , n ) ;

 if (overlays[M_STATE].seg[k] == 0)
 {
    _draw_bcdfile_overlay_ssradar ( & overlays[M_STATE]) ;
                            
 }
 
 for ( k = 0 ; k < overlays[ M_STATE ].n_files ; k++ )
 {
     _set_first_segment( overlays[M_STATE].seg[k] ) ;
 
    /*--------------------------------------------------------------*/
    /*     determine number of pixels per hrap bin                  */
    /*--------------------------------------------------------------*/

      x = (float)width/(float) pDrawMsData->maximum_columns;
      y = (float)height/(float)pDrawMsData->maximum_rows;

      if (x > y)
	 x = y;
      else if (y > x)
	 y = x;

    /*--------------------------------------------------------------*/
    /*     create graphics context for states overlay               */
    /*--------------------------------------------------------------*/

      gcv.foreground = get_pixel_by_name(w,overlays[M_STATE].color);
      gcv.line_width = overlays[M_STATE].line_width ;
      gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

    /*--------------------------------------------------------------*/
    /*     determine number of points to be plotted & allocate space*/
    /*--------------------------------------------------------------*/

      do
      {
	 maxpts = _get_points_in_segment ( ) ; 
	 
    /*---------------------------------------------------------------------*/ 
    /*   allocate memory only in the cases:				   */
    /*			- if it has not been allocated yet; 		   */
    /*			- previously allocated memory space was smaller    */
    /*  		  than needed in the current iteration             */
    /*---------------------------------------------------------------------*/
    
	 if ( points == NULL ) 
 	      points = ( XPoint * ) 
		      realloc ( points, maxpts * sizeof ( XPoint ));
	      
         else if( maxpts > prev_maxpts )
	      points = ( XPoint * ) 
		      realloc ( points, maxpts * sizeof ( XPoint ));
	      
 	 if ( points == NULL )
 	 {
              flogMessage ( stderr , "\nIn routine \"show_states\":\n"
                       "Could not allocate %d blocks of memory of size %d\n"
                       "for the \"points\" array of XPoint data types.\n"
                       "Cannot draw the state boundaries.\n" ,
                       maxpts , sizeof ( XPoint ) ) ;
    	      return ;
 	 }
	
         for (j = 0; j < maxpts; j++)
         {
	     _get_point_coordinate ( j , & latit , & longt ) ;
	    
	      hrap = LatLongToHrapMpe(latit, (-1) * longt);
	    
	      hrap.x -= pDrawMsData->origin.x ;
              hrap.y -= pDrawMsData->origin.y ; 
    
	      points[j].x = ( hrap.x ) * x;
              points[j].y = ( pDrawMsData->maximum_rows - hrap.y ) * y;
         }
	 
         for ( i = MsData ; i <= St1iiData ; ++ i )
         {
            XDrawLines ( dpy , ds_array [ i ] [ ss_number ].pix , 
                         gc , points , maxpts , CoordModeOrigin ) ;
         }
	 
	 prev_maxpts = maxpts ;
	 
      } while (_next_segment() != NULL);
      
 }
 
 if ( overlays [ M_STATE ].store_in_memory == ( int ) M_OFF )
 {
     _deallocate_segments ( & ( overlays [ M_STATE ].seg [ 0 ] ) ) ;
     overlays [ M_STATE ].seg [ 0 ] = NULL ;
 }
 
 free ( points ) ;
 XFreeGC ( dpy , gc ) ;

}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   show_county
* PURPOSE:  Callback to toggle counties overlay on the single radar site window. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE             DESCRIPTION
*   _get_map_display              map_resource.h          Returns the display.
*   _set_first_segment            map_menubar_cb.h        Sets the head of the 
*							  segment for the given 
*    							  overlay to the current 
*                   					  segment.
*   _get_point_coordinate	  map_menubar_cb.h   	  Gets the current 
*							  segment lat lon 
*							  coordinate for the 
*							  given point.
*   _next_segment                 map_menubar_cb.h        Sets current segment  
*                                                         to next item in link 
*   get_pixel_by_name		  globals.h               Returns numeric color
*                                                         value by color name.
*   _get_points_in_segment        map_menubar_cb.h        Gets the number of 
*  							  points in the current 
*  							  segment.						
*   HrapToLatLongMpe              stage3.h		  Converts HRAP
*							  coordinates to
*							  lat / lon.
*   LatLongToHrapMpe              stage3.h		  Converts lat / lon
*							  to HRAP coordinates.
*   mSetCursor		          map_resource.h	  Sets the cursor for 
*							  the map window.
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE               NAME              DESCRIPTION
*      int 		      i               An index for the array of 
*      					      the structures.
*      int		      j	              A loop index variable for
*                                             points in a segment.
*      int                    n               An index for wargs array.
*      Arg                 wargs [ ]          Arg structure, an array of 
*                                             arguments used for setting 
*                                             widget resources, dimensioned
*					      to 5.
*      int                 maxpts             A number of points in a
*                                             segment.
*      int                 prev_maxpts        Saved number of points in the
*                                             previous segment.
*      int                    k               A loop variable of data files
*                                             to be read.
*     XPoint *             points             A pointer to XPoint structure
*                                             with the coordinates of the
*                                             point.
*     GC                    gc                A pointer to the Graphic Context 
*                                             structure.                                            
*     Display *             dpy               A pointer to the Display
*                                             structure.
*     int             mask = GCForeground     Mask to set the foreground
*					      component of GC.
*     Dimension            width              Dimension structure, width
*                                             of drawing area canvas.
*     Dimension            height             Dimension structure, height
*                                             of drawing area canvas.
*       int                  x,y              A number of pixels per hrap 
*                                             unit.
*     XGCValues              gcv              Specifies the values for the
*					      fields in value_mask.                                                 
*      float             latit,longt          Lat / lon coordinates of 
*                                             the point. 
*      HRAP            lat_lon_ne_corner      Determines latitude and
*                      lat_lon_nw_corner      longitude bounds of the HRAP
*		       lat_lon_se_corner      area. 
*		       lat_lon_sw_corner      
*      float         lat1 ,lat2 ,lon1 ,lon2   Determines latitude and 
*					      logtitude bounds of the area
*                                             being displayed in each of the
*                                             panes on the site radar window.
*					                   
*      HRAP                 hrap              Hrap structure, contains
*                                             hrap coordinates of a
*                    			      point.
*      point             hrap_point           Structure, contains
*                                             point coordinates.
*       int                  m                Flag indicates that current point
*                                             is out of bounds or not. 
*       int                 pass              Flag indicates that current point
*                                             is not first out of bounds.                                                          			                               
*       int               xtemp, ytemp        Keeps coordinates of the first out 
*                                             of bounds point.
*                      
* DATA FILES AND/OR DATABASE:
* This routine reads counties overlay data from the memory. If there is no data  
* in memory, this routine reads bcd file containing counties data.  
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if the following
* condition occurs:
* 
* 1) Could not allocate blocks of memory of needed size 
*     for the points array of XPoint data types.
*
*******************************************************************************
*/

void MPEUtil_show_county ( Widget w, XtPointer client_data , XtPointer call_data )
{
   int          i, j, n, maxpts, prev_maxpts = 0 ;
   int 		k = 0 ;
   int          ss_number = ( int ) client_data ;
   XPoint      *points = NULL ;
   GC           gc = NULL ;
   Display     *dpy;
   Dimension    width, height;
   draw_struct * pDrawMsData = NULL ;
   Arg          wargs[5];
   HRAP         lat_lon_ne_corner ; 
   HRAP         lat_lon_nw_corner ; 
   HRAP         lat_lon_se_corner ;
   HRAP         lat_lon_sw_corner ; 
   int          m ;
   int          pass ;
   int          x, y;
   int          xtemp = 0 , ytemp = 0;
   int          mask = GCForeground ;
   XGCValues    gcv ;
   float        latit, longt ;
   float        lat1 , lat2 , lon1 , lon2 ;
   HRAP         hrap ;
   point        hrap_point ;

 dpy = _get_map_display ( ) ; 
 pDrawMsData = & ds_array [ MsData ] [ ss_number ] ;

 /*--------------------------------------------------------------*/
 /*     determine size of display area                           */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues ( pDrawMsData->w , wargs , n ) ;

 /*--------------------------------------------------------------*/
 /*     determine number of points to be plotted & allocate space*/
 /*--------------------------------------------------------------*/
 
 /* Set the pointer to indicate that the program is "busy". */
  
 mSetCursor ( M_WATCH ) ;
     
 if (overlays[M_COUNTY].seg[k] == 0)
 {  
    _draw_bcdfile_overlay_ssradar ( & overlays[M_COUNTY]) ;
   
 }
 
 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 /* Set up the scaling factor that will be used to plot the data. */
 
 x = (float)width/(float) pDrawMsData->maximum_columns;
 y = (float)height/(float) pDrawMsData->maximum_rows;

 if (x > y)
    x = y;
 else if (y > x)
    y = x;

 /*--------------------------------------------------------------*/
 /* Determine the latitude and longitude bounds of the 		 */
 /*  of the area being displayed in each of the panes on	 */
 /*   the single site radar window. 			   	 */
 /*--------------------------------------------------------------*/   
 
 hrap_point.x = pDrawMsData->origin.x ;
 hrap_point.y = pDrawMsData->origin.y ; 
 lat_lon_sw_corner = HrapToLatLongMpe ( hrap_point ) ;

 hrap_point.x = pDrawMsData->origin.x ;
 hrap_point.y = pDrawMsData->origin.y + pDrawMsData->maximum_rows  ;
 lat_lon_nw_corner = HrapToLatLongMpe ( hrap_point ) ;

 hrap_point.x = pDrawMsData->origin.x + pDrawMsData->maximum_columns  ;
 hrap_point.y = pDrawMsData->origin.y + pDrawMsData->maximum_rows  ;
 lat_lon_ne_corner = HrapToLatLongMpe ( hrap_point ) ;

 hrap_point.x = pDrawMsData->origin.x + pDrawMsData->maximum_columns  ;
 hrap_point.y = pDrawMsData->origin.y ; 
 lat_lon_se_corner = HrapToLatLongMpe ( hrap_point ) ;

 if ( lat_lon_sw_corner.y < lat_lon_se_corner.y )
 {
    lat2 = lat_lon_sw_corner.y ;
 }
 else
 {
    lat2 = lat_lon_se_corner.y ;
 }

 if ( lat_lon_nw_corner.y > lat_lon_ne_corner.y )
 {
    lat1 = lat_lon_nw_corner.y ;
 }
 else
 {
    lat1 = lat_lon_ne_corner.y ;
 }

 if ( lat_lon_sw_corner.x > lat_lon_nw_corner.x )
 {
    lon1 = lat_lon_sw_corner.x ;
 }
 else
 {
    lon1 = lat_lon_nw_corner.x ;
 }

 if ( lat_lon_se_corner.x < lat_lon_ne_corner.x )
 {
    lon2 = lat_lon_se_corner.x ;
 }
 else
 {
    lon2 = lat_lon_ne_corner.x ;
 }
	 
 for ( k = 0 ; k < overlays[ M_COUNTY ].n_files ; k++ )
 {
    _set_first_segment( overlays[M_COUNTY].seg[k] ) ;

    /*--------------------------------------------------------------*/
    /*     create graphics context for states overlay               */
    /*--------------------------------------------------------------*/

    gcv.foreground = get_pixel_by_name(w,overlays[M_COUNTY].color) ;
    gcv.line_width = overlays[M_COUNTY].line_width ;
    gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

    /*--------------------------------------------------------------*/
    /*     determine number of points to be plotted & allocate space*/
    /*--------------------------------------------------------------*/

    do
    {  
       maxpts = _get_points_in_segment ( ) ; 
         
    /*---------------------------------------------------------------------*/ 
    /*   allocate memory only in the cases:				   */
    /*			- if it has not been allocated yet; 		   */
    /*			- previously allocated memory space was smaller    */
    /*  		  than needed in the current iteration             */
    /*---------------------------------------------------------------------*/
    
       if ( points == NULL ) 
 	      points = ( XPoint * ) 
                       realloc ( points, maxpts * sizeof ( XPoint ) ) ;
	      
       else if( maxpts > prev_maxpts )
	      points = ( XPoint * ) 
                       realloc ( points, maxpts * sizeof ( XPoint ) ) ;

       if ( points == NULL )
       {
              flogMessage ( stderr , "\nIn routine \"show_county\":\n"
                       "Could not allocate %d blocks of memory of size %d\n"
                       "for the \"points\" array of XPoint data types.\n"
                       "Cannot draw the county boundaries.\n" ,
                       maxpts , sizeof ( XPoint ) ) ;
    	      return ;
       }

       m = 0 ;
       pass = 0 ;
	
       for ( j = 0 ; j < maxpts ; ++ j )
       {
          _get_point_coordinate ( j , & latit , & longt ) ;
	  
	  /*--------------------------------------------------------------*/
          /* Enhanced logic to ensure that we draw only what we need to   */
          /*  draw. 							  */
	  /*--------------------------------------------------------------*/
	  
          if ( ( latit >= lat2 && (-1) * longt <= lon1 ) &&
               ( latit <= lat1 && (-1) * longt >= lon2 ) )
          { 
	    
	     hrap = LatLongToHrapMpe ( latit , ( -1 ) * longt ) ;
	    
	     hrap.x -= pDrawMsData->origin.x ;
             hrap.y -= pDrawMsData->origin.y ; 

             if ( ( m == 0 ) && ( pass == 1 ) )
             {
	        points [ m ].x = ( xtemp ) * x ;
                points [ m ].y = ( pDrawMsData->maximum_rows - ytemp ) * y ;
                m ++ ;
             }
    
	     points[m].x = ( hrap.x ) * x;
             points[m].y = ( pDrawMsData->maximum_rows - hrap.y ) * y;
             m ++ ;
          }
          else
          {
	     hrap = LatLongToHrapMpe(latit, (-1) * longt);
	    
	     hrap.x -= pDrawMsData->origin.x ;
             hrap.y -= pDrawMsData->origin.y ; 

             if ( m == 0 )
             {
                xtemp = hrap.x ;
                ytemp = hrap.y ;
                pass = 1 ;
                continue ;
             }

             points[m].x = ( hrap.x ) * x;
             points[m].y = ( pDrawMsData->maximum_rows - hrap.y ) * y;
             m ++ ;

             for ( i = MsData ; i <= St1iiData ; ++ i )
             {
                XDrawLines ( dpy , ds_array [ i ] [ ss_number ].pix , 
                             gc , points , m , CoordModeOrigin ) ;
             }
	 
             m = 0 ;
             pass = 0 ;
          }
       }
	 
       for ( i = MsData ; i <= St1iiData ; ++ i )
       {
          XDrawLines ( dpy , ds_array [ i ] [ ss_number ].pix , 
                       gc , points , m , CoordModeOrigin ) ;
       }

       prev_maxpts = maxpts ;
	 
    } while (_next_segment() != NULL);
      
      /*-----------------------------------------------------------------*/ 
      /* Set the pointer back to "normal" to indicate that the program   */
      /*is "idle". 							 */ 
      /*-----------------------------------------------------------------*/
      
      mSetCursor ( M_NORMAL ) ;                       
 }
 if ( overlays [ M_COUNTY ].store_in_memory == ( int ) M_OFF )
 {
     _deallocate_segments ( & ( overlays [ M_COUNTY ].seg [ 0 ] ) ) ;
     overlays [ M_COUNTY ].seg [ 0 ] = NULL ;
 }
 
 free ( points ) ;
 XFreeGC ( dpy , gc ) ;

}
/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   show_cities_and_towns
* PURPOSE:  Callback to toggle cities and towns overlay on the single radar 
*	    site window. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE             DESCRIPTION
*   _get_map_display              map_resource.h          Returns the display.
*   get_pixel_by_name		  globals.h               Returns numeric color
*                                                         value by color name.
*   pCalculateOverlay             map_menubar_cb.h        Pointer to 
*                                                       OverlayCalculatorRoutine
*                                                         which calculates 
*							  and draws the overlay.
*   LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE               NAME              DESCRIPTION
*      int 		      i               An index for the array of 
*      					      the structures.
*      int                    n               An index for wargs array.
*      Arg                 wargs [ ]          Arg structure, an array of 
*                                             arguments used for setting 
*                                             widget resources, dimensioned
*					      to 5.
*     GC                    gc                A pointer to the Graphic Context 
*                                             structure.
*     Display *             dpy               A pointer to the Display
*                                             structure.
*     int             mask = GCForeground     Mask to set the foreground
*					      component of GC.
*     Dimension            width              Dimension structure, width
*                                             of drawing area canvas.
*     Dimension            height             Dimension structure, height
*                                             of drawing area canvas.
*       int                  x,y              A number of pixels per hrap 
*                                             unit.
*     XGCValues              gcv              Specifies the values for the
*					      fields in value_mask.     
*
* DATA FILES AND/OR DATABASE:
* This routine reads cities and towns overlay data from different sources 
* depends on settings in overlay configuration file. 
* WHFS cities and towns  overlay is read from database,
* FSL and MPE cities and towns overlays are read from the files. 
*
* ERROR HANDLING:
*     None
*******************************************************************************
*/

 void MPEUtil_show_cities_and_towns ( Widget w , XtPointer client_data , 
                              XtPointer call_data )
{
   int          i, n;
   GC           gc;
   Display     *dpy = NULL ;
   Dimension    width, height;
   draw_struct * pDrawMsData = NULL ;
   Arg          wargs[5];
   int          mask = GCForeground;
   int          ss_number = ( int ) client_data ;
   int          x,y;
   XGCValues    gcv;
   
 dpy = _get_map_display ( ) ; 
 pDrawMsData = & ds_array [ MsData ] [ ss_number ] ;

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++ ;
 XtSetArg(wargs[n], XmNheight, &height); n++ ;
 XtGetValues ( pDrawMsData->w , wargs , n ) ;

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/
 x = (float)width/(float)pDrawMsData->maximum_columns;
 y = (float)height/(float)pDrawMsData->maximum_rows;

 if ( x > y )
 {
    x = y ;
 }
 else if ( y > x)
 {
    y = x ;
 }

 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(w,overlays[M_CITY_TOWN].color) ;
 gcv.line_width = overlays[M_CITY_TOWN].line_width ; 
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv) ;

 /*--------------------------------------------------------------*/
 /*     read cities and towns file                               */
 /*--------------------------------------------------------------*/
 
 for ( i = MsData ; i <= St1iiData ; ++ i )
 {
    overlays[M_CITY_TOWN].pCalculateOverlay ( M_CITY_TOWN , 
		                             & overlays [ M_CITY_TOWN ] ,
                                             ds_array [ i ] [ ss_number ].pix ,
                                             & ds_array [ i ] [ ss_number ] ) ;
 }
 
 XFreeGC ( dpy , gc ) ;

}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   show_basin_boundaries
* PURPOSE:  Callback to toggle basin boundaries overlay on the single radar 
*	    site window. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE             DESCRIPTION
*   _get_map_display              map_resource.h          Returns the display.
*   get_pixel_by_name		  globals.h               Returns numeric color
*                                                         value by color name.
*   _set_first_segment            map_menubar_cb.h        Sets the head of the 
*							  segment for the given 
*    							  overlay to the current 
*                   					  segment.
*   _next_segment                 map_menubar_cb.h        Sets current segment  
*                                                         to next item in link 
*							  list.
*   _get_point_coordinate	  map_menubar_cb.h   	  Gets the current 
*							  segment lat lon 
*							  coordinate for the 
*							  given point.
*   pExternalOverlay              map_menubar_cb.h        Pointer to 
*                                                         OverlayExternalRoutine
*                                                         which calculates 
*							  and draws the overlay.
*
*   LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE               NAME              DESCRIPTION
*      int 		      i               An index for the array of 
*      					      the structures.
*      int 		      i               An index for the array of 
*      					      the structures.
*      int		      j	              A loop index variable for
*                                             points in a segment.
*      int                 maxpts             A number of points in a
*                                             segment.
*      int                 prev_maxpts        Saved number of points in the
*                                             previous segment.
*      int                    k               A loop variable of data files
*                                             to be read.
*      int                    n               An index for wargs array.
*      Arg                 wargs [ ]          Arg structure, an array of 
*                                             arguments used for setting 
*                                             widget resources, dimensioned
*					      to 5.
*     XPoint *             points             A pointer to XPoint structure
*                                             with the coordinates of the
*                                             point.
*     GC                    gc                A pointer to the Graphic Context 
*                                             structure.
*     Display *             dpy               A pointer to the Display
*                                             structure.
*     int             mask = GCForeground     Mask to set the foreground
*					      component of GC.
*     Dimension            width              Dimension structure, width
*                                             of drawing area canvas.
*     Dimension            height             Dimension structure, height
*                                             of drawing area canvas.
*       int                  x,y              A number of pixels per hrap 
*                                             unit.
*     XGCValues              gcv              Specifies the values for the
*					      fields in value_mask. 
*      float             latit,longt          Lat / lon coordinates
*                                             of a point.
*      HRAP                 hrap              Hrap structure, contains
*                                             hrap coordinates of a
*                    			      point. 
* DATA FILES AND/OR DATABASE:
* This routine reads basin boundaries overlay data from the database,
* GeoArea table.
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if the following
* condition occurs:
* 
* 1) There is no overlay information has been supplied to this routine.
* 2) Could not allocate blocks of memory of needed size 
*    for the points array of XPoint data types.
*******************************************************************************
*/

void MPEUtil_show_basin_boundaries ( Widget w , XtPointer client_data  , 
                             XtPointer call_data )
{
   int                  i , j , n, maxpts, prev_maxpts = 0;
   int			k = 0 ;
   int          ss_number = ( int ) client_data ;
   XPoint              * points = NULL ;
   GC                   gc = NULL ;
   Display             *dpy = NULL ; 
   Dimension            width, height;
   draw_struct * pDrawMsData = NULL ;
   Arg                  wargs[5];
   int                  x, y;
   int                  mask = GCForeground;
   XGCValues            gcv;       
   float                latit, longt;
   HRAP                 hrap;
  
 dpy = _get_map_display ( ) ; 
 pDrawMsData = & ds_array [ MsData ] [ ss_number ] ;

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(pDrawMsData->w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     determine whether to display map basins or forecast      */
 /*     group basins depending on size of display area           */
 /*--------------------------------------------------------------*/
 
 if (overlays[M_BASINS].seg[k] == NULL )
 {
   
     overlays[M_BASINS].pExternalOverlay ( & overlays [ M_BASINS] ,
                                           M_BASINS ,  
 				           & ( overlays[M_BASINS].seg [ k ]) , 
 				           & overlays[M_BASINS].shape_type , 
        			           & fillarea  ) ;
				       
     if ( overlays[M_BASINS].seg[k] == NULL ) 
     {
         flogMessage ( stderr , "\nIn routine \"show_basin_boundaries\":\n"
                         "\"overlays[M_BASINS].seg[k]\" is NULL.\n"  
			 "No overlay information has\n"
                         "been provided to this routine.  Aborting the\n"
                         "plotting of the basin boundaries.\n" ) ;
         return ;
     }
 }
 
     _set_first_segment( overlays[M_BASINS].seg[k] ) ;
 
    /*--------------------------------------------------------------*/
    /*     determine number of pixels per hrap bin                  */
    /*--------------------------------------------------------------*/

    x = (float)width/(float)pDrawMsData->maximum_columns;
    y = (float)height/(float)pDrawMsData->maximum_rows;

    if (x > y)
        x = y;
    else if (y > x)
	y = x;

    /*--------------------------------------------------------------*/
    /*     create graphics context for basins overlay               */
    /*--------------------------------------------------------------*/

    gcv.foreground = get_pixel_by_name(w,overlays[M_BASINS].color);
    gcv.line_width = overlays[M_BASINS].line_width ;
    gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

    /*--------------------------------------------------------------*/
    /*     determine number of points to be plotted & allocate space*/
    /*--------------------------------------------------------------*/

    do
    {
	maxpts = _get_points_in_segment ( ) ;   
	  
    /*---------------------------------------------------------------------*/ 
    /*   allocate memory only in the cases:				   */
    /*			- if it has not been allocated yet; 		   */
    /*			- previously allocated memory space was smaller    */
    /*  		  than needed in the current iteration             */
    /*---------------------------------------------------------------------*/
    
       if ( points == NULL ) 
 	      points = ( XPoint * ) 
                       realloc ( points, maxpts * sizeof ( XPoint ) ) ;
	      
       else if( maxpts > prev_maxpts )
	      points = ( XPoint * ) 
                       realloc ( points, maxpts * sizeof ( XPoint ) ) ;
	      

 	 if ( points == NULL )
 	 {
              flogMessage ( stderr , "\nIn routine \"show_basin_boundaries\":\n"
                       "Could not allocate %d blocks of memory of size %d\n"
                       "for the \"points\" array of XPoint data types.\n"
                       "Cannot draw the basin boundaries.\n" ,
                       maxpts , sizeof ( XPoint ) ) ;
    	      return ;
 	 }
	
         for (j = 0; j < maxpts; j++)
         {
	     _get_point_coordinate ( j , & latit , & longt ) ;
	    
	      hrap = LatLongToHrapMpe(latit, (-1) * longt);
	    
	      hrap.x -= pDrawMsData->origin.x ;
              hrap.y -= pDrawMsData->origin.y ; 
    
	      points[j].x = ( hrap.x ) * x;
              points[j].y = ( pDrawMsData->maximum_rows - hrap.y ) * y;
         }
	 
         for ( i = MsData ; i <= St1iiData ; ++ i )
         {
            XDrawLines ( dpy , ds_array [ i ] [ ss_number ].pix , 
                         gc , points , maxpts , CoordModeOrigin ) ;
         }
	 
	 prev_maxpts = maxpts ;
	 
    } while (_next_segment() != NULL);
 
      free ( points ) ;
      XFreeGC ( dpy , gc ) ;
}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   show_rivers
* PURPOSE:  Callback to toggle rivers overlay on the single radar 
*	    site window. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE          DESCRIPTION
*   _get_map_display              map_resource.h       Returns the display.
*   get_pixel_by_name		  globals.h            Returns numeric color
*                                                      value by color name.
*   _set_first_segment            map_menubar_cb.h     Sets the head of the 
*						       segment for the given 
*    						       overlay to the current 
*                   				       segment.
*   _next_segment                 map_menubar_cb.h     Sets current segment  
*                                                      to next item in link 
*						       list.
*   _get_point_coordinate	  map_menubar_cb.h     Gets the current 
*						       segment lat lon 
*						       coordinate for the 
*						       given point.
*   LatLongToHrapMpe              stage3.h	       Converts lat / lon
*						       to HRAP coordinates.
*   pExternalOverlay              map_menubar_cb.h     Pointer to 
*                                                      OverlayExternalRoutine
*                                                      which calculates 
*						       and draws the overlay.
*   LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE               NAME              DESCRIPTION
*      int 		      i               An index for the array of 
*      					      the structures.
*      int 		      i               An index for the array of 
*      					      the structures.
*      int		      j	              A loop index variable for
*                                             points in a segment.
*      int                 maxpts             A number of points in a
*                                             segment.
*      int                 prev_maxpts        Saved number of points in the
*                                             previous segment.
*      int                    k               A loop variable of data files
*                                             to be read.
*      int                    n               An index for wargs array.
*      Arg                 wargs [ ]          Arg structure, an array of 
*                                             arguments used for setting 
*                                             widget resources, dimensioned
*					      to 5.
*     XPoint *             points             A pointer to XPoint structure
*                                             with the coordinates of the
*                                             point.
*     GC                    gc                A pointer to the Graphic Context 
*                                             structure.                                            
*     Display *             dpy               A pointer to the Display
*                                             structure.
*     int             mask = GCForeground     Mask to set the foreground
*					      component of GC.
*     Dimension            width              Dimension structure, width
*                                             of drawing area canvas.
*     Dimension            height             Dimension structure, height
*                                             of drawing area canvas.
*       int                  x,y              A number of pixels per hrap 
*                                             unit.
*     XGCValues              gcv              Specifies the values for the
*					      fields in value_mask. 
*      float             latit,longt          Lat / lon coordinates
*                                             of a point.
*      HRAP                 hrap              Hrap structure, contains
*                                             hrap coordinates of a
*                    			      point. 
* DATA FILES AND/OR DATABASE:
* This routine reads rivers overlay data from the database,
* GeoLine table.
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if the following
* condition occurs:
* 
* 1) There is no overlay information has been supplied to this routine.
* 2) Could not allocate blocks of memory of needed size 
*    for the points array of XPoint data types.
*******************************************************************************
*/

void MPEUtil_show_rivers ( Widget w , XtPointer client_data , XtPointer call_data )
{
   int          i , j, n, maxpts, prev_maxpts = 0;
   int          k = 0;
   int          ss_number = ( int ) client_data ;
   XPoint      *points = NULL ;
   GC           gc;
   Display     *dpy = NULL ;
   Dimension    width, height;
   draw_struct * pDrawMsData = NULL ;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   XGCValues    gcv;
   
   float                latit, longt;
   HRAP                 hrap;

 dpy = _get_map_display ( ) ; 
 pDrawMsData = & ds_array [ MsData ] [ ss_number ] ;

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues( ds_array [MsData] [ss_number].w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     determine whether to display map basins or forecast      */
 /*     group basins depending on size of display area           */
 /*--------------------------------------------------------------*/
 
 if (overlays[M_STREAMS].seg[k] == 0)
 {
   
     overlays[M_STREAMS].pExternalOverlay ( & overlays [ M_STREAMS ] ,
                                            M_STREAMS ,  
 				            &( overlays[M_STREAMS].seg [ k ]) , 
 				            & overlays[M_STREAMS].shape_type , 
 				            & fillarea  ) ;
				       
     if ( overlays[M_STREAMS].seg[k] == NULL ) 
     {
         flogMessage ( stderr , "\nIn routine \"show_rivers\":\n"
                         "\"overlays[M_STREAMS].seg[k]\" is NULL.\n"  
                         "No overlay information has\n" 
			 "been provided to this routine.  Aborting the\n"
                         "plotting of the rivers.\n" ) ;
         return ;
     }
 }
 
     _set_first_segment( overlays[M_STREAMS].seg[k] ) ;
 
    /*--------------------------------------------------------------*/
    /*     determine number of pixels per hrap bin                  */
    /*--------------------------------------------------------------*/


    x = (float)width/(float)pDrawMsData->maximum_columns;
    y = (float)height/(float)pDrawMsData->maximum_rows;

    if (x > y)
	 x = y;
      else if (y > x)
	 y = x;
	 
    /*--------------------------------------------------------------*/
    /*     create graphics context for states overlay               */
    /*--------------------------------------------------------------*/

      gcv.foreground = get_pixel_by_name(w,overlays[M_STREAMS].color);
      gcv.line_width = overlays[M_STREAMS].line_width;
      gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);
    
    /*--------------------------------------------------------------*/
    /*     determine number of points to be plotted & allocate space*/
    /*--------------------------------------------------------------*/

     do
     {
	 maxpts = _get_points_in_segment ( ) ;   
	  
 	 if ( points == NULL ) 
 	      points = ( XPoint * ) 
                       realloc ( points, maxpts * sizeof ( XPoint ) ) ;
	      
         else if( maxpts > prev_maxpts )
	      points = ( XPoint * ) 
                       realloc ( points, maxpts * sizeof ( XPoint ) ) ;
	      
 	 if ( points == NULL )
 	 {
              flogMessage ( stderr , "\nIn routine \"show_rivers\":\n"
                       "Could not allocate %d blocks of memory of size %d\n"
                       "for the \"points\" array of XPoint data types.\n"
                       "Cannot draw the rivers.\n" ,
                       maxpts , sizeof ( XPoint ) ) ;
    	      return ;
 	 }
	
         for (j = 0; j < maxpts; j++)
         {
	     _get_point_coordinate ( j , & latit , & longt ) ;
	    
	      hrap = LatLongToHrapMpe(latit, (-1) * longt);
	    
	      hrap.x -= pDrawMsData->origin.x ;
              hrap.y -= pDrawMsData->origin.y ; 
    
	      points[j].x = ( hrap.x ) * x;
              points[j].y = ( pDrawMsData->maximum_rows - hrap.y ) * y;
         }
	 
         for ( i = MsData ; i <= St1iiData ; ++ i )
         {
            XDrawLines ( dpy , ds_array [ i ] [ ss_number ].pix , 
                         gc , points , maxpts , CoordModeOrigin ) ;
         }
	 
	 prev_maxpts = maxpts ;
	 
     } while (_next_segment() != NULL);

 free ( points ) ;
 XFreeGC ( dpy , gc ) ;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


