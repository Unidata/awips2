
/*******************************************************************************
* FILENAME:            map.c
* NUMBER OF MODULES:   49
* GENERAL INFORMATION:
*    MODULE 1:         _get_map_pixmap
* DESCRIPTION:         Returns the pixmap upon which the map or maps are drawn.
*    MODULE 2:         _clear_map
* DESCRIPTION:         Clears the map's pixmap
*    MODULE 3:         _legend_expose
* DESCRIPTION:         Calls a user-specified routine to expose the map's 
*                      legend. 
*    MODULE 4:         _map_expose
* DESCRIPTION:         This routine calls the user specified routine that
*                      draws the map, its symbols, and all of its overlays.
*    MODULE 5:         mQuickUpdateMap 
* DESCRIPTION:         This routine forces the specified map to be redrawn.
*                      It's function is similar to mUpdateMap, and it may
*                      be removed in a future release.
*    MODULE 6:         mUpdateMap
* DESCRIPTION:         This routine forces the specified map to be redrawn.
*    MODULE 7:         mUpdateLegend
* DESCRIPTION:         This routine causes the map's legend to be redrawn.
*    MODULE 8:         _get_make_map
* DESCRIPTION:         This routine returns the state of the "make map" flag.
*    MODULE 9:         _set_make_map
* DESCRIPTION:         This routine sets the state of the "make map" flag.
*    MODULE 10:        _get_screen_width
* DESCRIPTION:         This routine returns the width of the screen in 
*                      pixels
*    MODULE 11:        _get_toolbar
* DESCRIPTION:         This routine returns the state of the toolbar.  That is,
*                      it reports whether the toolbar is visible and active or
*                      not.
*    MODULE 12:        _set_map_change
* DESCRIPTION:         This routine sets the flag indicating whether or not
*                      a map has been altered.  This is used to communicate
*                      to other map library routines
*    MODULE 13:        _get_map_width
* DESCRIPTION:         This routine returns the map width in pixels.
*    MODULE 14:        _get_map_height
* DESCRIPTION:         This routine returns the map height in pixels.
*    MODULE 15:        _get_num_of_maps
* DESCRIPTION:         This routine returns the numbder of maps being displayed
*                      on the screen.  This currently may be 1, 2, or 4.
*    MODULE 16:        _get_map_widget
* DESCRIPTION:         This routine returns the widget representing the map.
*    MODULE 17:        _get_maps_pixmap
* DESCRIPTION:         Returns the pixmap used to draw an individual map being
*                      displayed.
*    MODULE 18:        _get_states_pixmap
* DESCRIPTION:         Returns the pixmap used to draw the state and political
*                      boundary information.
*    MODULE 19:        _get_legend_pixmap
* DESCRIPTION:         This routine returns the pixmap used to draw the legend
*                      textual and symbolic information. 
*    MODULE 20:        _get_map_shell
* DESCRIPTION:         This routine returns the widget identifier of the map's
*                      shell.
*    MODULE 21:        _create_pixmaps
* DESCRIPTION:         Creates the pixmaps associated with each of the 1, 2, 
*                      or 4 maps that the user is attempting to display. 
*    MODULE 22:        _focus_change 
* DESCRIPTION:         The event handler for handling focus changes on the
*                      Main GUI.
*    MODULE 23:        _create_single_map
* DESCRIPTION:         Creates a single map.  This routine is called in the 
*                      instance the user only wants to display one map on the
*                      screen.
*    MODULE 24:        _create_two_maps
* DESCRIPTION:         Creates a GUI displaying two maps.  This is not
*                      implemented at this time.
*    MODULE 25:        _create_four_maps
* DESCRIPTION:         Creates a GUI displaying four maps. This is not
*                      implemented at this time.
*    MODULE 26:        _create_map
* DESCRIPTION:         This routine creates a legend and draw area for each
*                      map being displayed.
*    MODULE 27:        _turn_toolbar_on
* DESCRIPTION:         This routine toggles the state of the toolbar to on.
*    MODULE 28:        _turn_toolbar_off
* DESCRIPTION:         This routine toggles the state of the toolbar to off.
*    MODULE 29:        _turn_legend_on
* DESCRIPTION:         This routine turns the legend on for all of the maps
*                      being displayed. 
*    MODULE 30:        _turn_legend_off
* DESCRIPTION:         This routine turns the legend off for all of the maps
*                      being displayed.
*    MODULE 31:        mCloseMapScreen
* DESCRIPTION:         This routine closes or "pops down" the shell used to
*                      contain the map(s).
*    MODULE 32:        mGetLegendStatus
* DESCRIPTION:         This routine returns the status of the legend indicating
*                      if it is on or off.
*    MODULE 33:        mAddLegendDrawCb
* DESCRIPTION:         This routine sets the callcack routine that will be
*                      used to draw the legend.  The callback routine is
*                      written by the user specifically for his application.
*    MODULE 34:        mAddLegendExpose
* DESCRIPTION:         This routine adds the legend drawing routine as a 
*                      callback.
*    MODULE 35:        mAddLeaveCallback
* DESCRIPTION:         This routine allows the user to specify an action 
*                      when the mouse pointer leaves the boundaries of the
*                      specified map.
*    MODULE 36:        mAddAboutAppCallback
* DESCRIPTION:         This routine allows the user to specify a routine
*                      to be called when the user clicks on the "About" menu
*                      item under the "Help" menu.  If the user does not
*                      supply this routine, then the "About" menu item will
*                      have no effect.
*    MODULE 37:        _get_about_app_cb
* DESCRIPTION:         This routine returns a pointer to the function supplied
*                      by the developer that provides information "about" the
*                      program to the user.
*    MODULE 38:        mAddMapExpose
* DESCRIPTION:         This routine adds a routine as an expose callback for
*                      the specified map.
*    MODULE 39:        mAddMapSelectRoutine
* DESCRIPTION:         This routine allows for application-specific
*                      functionality to be assigned to the ButtonPress Event
*                      for the specified map.
*    MODULE 40:        mAddFocusChangeRoutine
* DESCRIPTION:         Allows the user to add a routine which can be called
*                      when a FocusIn or FocusOut event occurs.  A separate
*                      routine can be created for each of the maps being
*                      managed. 
*    MODULE 41:        mOpenMapScreen
* DESCRIPTION:         This routine "pops up" or realizes the main application
*                      depending on if it is a pop up shell or a standalone
*                      application. For standalone mapping applications,
*                      program control does not return from the call to this
*                      routine.
*    MODULE 42:        mSetMapSize
* DESCRIPTION:         This routine sets the size(s) of the map(s).
*    MODULE 43:        mChangeTitle
* DESCRIPTION:         This routine gives the user the ability to alter the
*                      the title of the map.
*    MODULE 44:        mInitMapScreen
* DESCRIPTION:         This routine initializes the height, width, screen x,
*                      screen y, toolbar state, and toolbar position of
*                      the map. 
*    MODULE 45:        mInitLegend
* DESCRIPTION:         This routine initializes the state (on/off), location,
*                      height, and width of the legend.
*    MODULE 46:        mInitMap
* DESCRIPTION:         This routine initializes the number of maps to be
*                      displayed, whether or not an overview map is desired,
*                      the background color, and the border color.
*    MODULE 47:        mCreateMapScreen
* DESCRIPTION:         Creates the actual map shell.  Initializes all of the
*                      widgets that go into making the map screen.
*    MODULE 48:        mCreateOverviewMap
* DESCRIPTION:         Creates an overview screen.  This is not implemented
*                      at this time.
*    MODULE 49:        mCreateLocateCoordinateMap
* DESCRIPTION:         I'm not sure what this was supposed to do.  Anyway, it
*                      has not been implemented by the original developer.
*    MODULE 50:        mSetMapBackgroundColor
* DESCRIPTION:         This routine sets the background color.
*    MODULE 51:        mAddMapCleanUp
* DESCRIPTION:         Allows the user to specify a routine that is called
*                      when the map is closed.  This could be useful for 
*                      freeing resources such as dynamically allocated 
*                      memory and closing files.
*    MODULE 52:        _get_legend_widget
* DESCRIPTION:         Returns the widget the represents the legend.
*                      In this case, the widget is from the xmDrawingAreaClass.
*
* ORIGINAL AUTHOR:     Heather Friedeman with modifications and updates by 
*                      Bryon Lawrence
* CREATION DATE:       Unknown
* ORGANIZATION:        HSEB / OHD
* MACHINES:            HP UNIX / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          
********************************************************************************
*/
/*****************************************************************************
 *
 * Module: map.c
 *
 * Description: contains routines to create map displays
 *
 *****************************************************************************/

/* include files */

/* C includes */

#include "stdio.h"
#include "stdlib.h"
#include "math.h"

/* x includes */

#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>
#include <Xm/Protocols.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>

/* map library includes */

#include "map.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_resource.h"
#include "map_menubar.h"
#include "map_menubar_cb.h"
#include "map_toolbar.h"
#include "map_draw.h"
#include "map_convert.h"

/* data types */
struct _Map map [ MAX_NUM_MAPS ] ;

/* global variables */

Widget parent_window;
Widget map_shell = NULL;
Widget main_window ;
Widget map_form;
Widget scrolled_window ;
Widget tool_form;
Widget tool_form_off;
Widget pane [ MAX_NUM_MAPS ] ;

extern Widget map_menubar;

Pixmap states,map_pixmap;
Pixmap user_pixmap ;

int legend_width , legend_height ;
int make_map;
int screen_width , screen_height;
int num_of_maps;
int screen_x,screen_y;
int stand_alone = 0 ;

int overview,legend,toolbar;
int legend_loc,toolbar_loc;

char background[15];
char screen_title[100];
char state_border_color[15];

static enum MapState expose_watch_pointer_flag = M_ON ;

static float width_in_nmi ;

MotionCallback about_app_cb = NULL ;
static CleanCallback clean_routine_cb = NULL ; 
static MotifCallback recenter_routine = NULL ;

XtAppContext app ;

/*****************************************************************************
 *
 * Routine: _get_map_pixmap
 *
 * Description: this routine returns the map pixmap
 *
 ****************************************************************************/
Pixmap _get_map_pixmap()
{
  return(map_pixmap);
}

/*****************************************************************************
 *
 * Routine: _clear_map
 *
 * Description: this routine clear pixmap for given map and draws states on
 * map
 *
 ****************************************************************************/

void _clear_map(int index)
{
  _set_foreground(background);
  _area_draw_fill_box(map_pixmap,0,0,map[index].width,map[index].height);
}

/*****************************************************************************
 *
 * Routine: _legend_expose
 *
 * Description: this routine is the expose callback for the legend
 *
 ****************************************************************************/

void _legend_expose(Widget wid,XtPointer client_data,XtPointer call_data)
{
  int index = ( int ) client_data;

  /* Check if the legend pixmap exists. */
  if ( map [ index ].lmap == 0 )
  {
     return;
  }
  
  mSetCursor(M_WATCH);
  _set_foreground(background);

  if ( map [ index ].legend_change == 1 )
  {
     _area_draw_fill_box(map[index].lmap,0,0,map[index].width,legend_height);
     if (map[index].lroutine != NULL )(*map[index].lroutine)( & index);
     map [ index ].legend_change = 0 ;
  }

  XCopyArea(_get_map_display(),map[index].lmap,XtWindow(wid),_get_map_gc(),
	    0,0,map[index].width,legend_height,0,0);
  mSetCursor(M_NORMAL);
}

/*****************************************************************************
 *
 * Routine: _map_expose
 *
 * Description: this routine is the expose callback for the map
 *
 ****************************************************************************/

void _map_expose(Widget wid,XtPointer client_data,XtPointer call_data)
{
  int index = ( int ) client_data;

  if ( _get_recenter_flag() == M_OFF && expose_watch_pointer_flag == M_ON )
  {
     mSetCursor ( M_WATCH ) ;
  }

  if ( map[index].map_change == 1 )
  {

    if ( map[index].routine != NULL )
    {

       /* Call the user-supplied map expose routine. */
       ( *map[index].routine ) ( & index ) ;

    }
    else
    {
       /* Clear the map. */
       _clear_map ( index ) ;

       /* The user hasn't supplied a map expose routine.
          Call the default map expose routine. */
       _draw_map_overlays ( index ) ;

    }
    
    XCopyArea(_get_map_display(),map_pixmap,map[index].pixmap,_get_map_gc(),
              0,0,map[index].width,map[index].height,0,0) ;

    map[index].map_change = 0 ;

  }

  XCopyArea(_get_map_display(),map[index].pixmap,XtWindow(wid),_get_map_gc(),
	    0,0,map[index].width,map[index].height,0,0);

  if ( ( _get_recenter_flag() == M_OFF ) && 
       ( expose_watch_pointer_flag == M_ON ) )
  {
     mSetCursor(M_NORMAL);
  }
}

/*****************************************************************************
 *
 * Routine: mQuickUpdateMap
 *
 * Description: this routine calls the copies the map and calls the users 
 *   call back.
 *
 ****************************************************************************/

void mQuickUpdateMap(int index)
{
  if (_get_recenter_flag() == M_OFF)
    mSetCursor(M_WATCH);

  XCopyArea(_get_map_display(),map_pixmap,map[index].pixmap,_get_map_gc(),
	    0,0,map[index].width,map[index].height,0,0);
  
  if (map[index].routine != NULL )
    (*map[index].routine)( & index );
  
  XCopyArea(_get_map_display(),map[index].pixmap,XtWindow(map[index].map),
	    _get_map_gc(),0,0,map[index].width,map[index].height,0,0);
  
  if (_get_recenter_flag() == M_OFF)
    mSetCursor(M_NORMAL);
}
/*****************************************************************************
 *
 * Routine: mUpdateMap
 *
 * Description: this routine calls the expose routine
 *
 ****************************************************************************/

void mUpdateMap(int number)
{

   Boolean is_managed;

   /* Check to make sure that the referenced map is managed. If it is not
      do not try to refresh it. */
   if ( map [ number ].map == NULL )
   {
      return;
   }

   is_managed = XtIsManaged ( map [ number ].map );

   if ( is_managed == False )
   {
      return;
   }
  
   map[number].map_change = 1;
  _map_expose(map[number].map,(XtPointer)number,NULL);
}

/*****************************************************************************
 *
 * Routine: _select_routine
 *
 * Description: this routine creates a draws the states on the states pixmap
 *
 ****************************************************************************/

void _select_routine(Widget wid,XtPointer client_data,XEvent *event, 
		     Boolean *flag)
{
  int event_type ;
  int i , index = ( int ) client_data ;
  float lat , lon ;
  
  /* Test the type of mouse button event this is. */
  if ( event->type == ButtonPress )
  {
     event_type = ButtonPress ;
  }
  else
  {
     event_type = ButtonRelease ;
  }

  mSetCursor(M_WATCH);
  if (_get_recenter_flag() == M_ON && event_type == ButtonPress ){
    mConvertXY2LatLon(event->xbutton.x,event->xbutton.y,&lat,&lon);
    mSetCenterLatLon(lat,lon);
    _set_recenter_flag(M_OFF);
    make_map = M_ON;

    /* Check to see if the user has supplied a recenter routine.
       if so, perform its operation. */ 
    if ( recenter_routine != NULL )
    {
       /* Recompute the polygon. */
       recenter_routine ( wid , NULL , NULL ) ;
    }

    for (i=0; i < num_of_maps;i++)
      mUpdateMap(i);
  }
  else{
    if (map[index].sroutine != NULL)
      (*map[index].sroutine)(wid,client_data,event,flag);
  }

  mSetCursor(M_NORMAL);
}
/*****************************************************************************
 *
 * Routine: _legend_select_routine
 *
 * Description: This routine processes a mouse click on the legend
 *              drawing area.
 *
 ****************************************************************************/

void _legend_select_routine(Widget wid,XtPointer client_data,XEvent *event, 
		            Boolean *flag)
{
  int index = ( int ) client_data ;
  
  mSetCursor(M_WATCH);

  if (map[index].lsroutine != NULL)
  {
      (*map[index].lsroutine)(wid,client_data,event,flag);
  }

  mSetCursor(M_NORMAL);
}

/*****************************************************************************
 *
 * Routine: mUpdateLegend
 *
 * Description: this routine calls the expose routine
 *
 ****************************************************************************/

void mUpdateLegend (int number)
{
   map[number].legend_change = 1;
   _legend_expose(map[number].legend,(XtPointer)number,NULL);
}

/*****************************************************************************
 *
 * Routine: _get_make_map
 *
 * Description: This routine returns the state of the make_map flag. 
 *
 ****************************************************************************/

int _get_make_map ( )
{
   return make_map ;
}

/*****************************************************************************
 *
 * Routine: _set_make_map
 *
 * Description: this routine sets the map_change item to true
 *
 ****************************************************************************/

void _set_make_map(int status)
{
  make_map = status;
}

/*****************************************************************************
 *
 * Routine: _get_screen_width
 *
 * Description: this routine returns the screen width
 *
 ****************************************************************************/

int _get_screen_width()
{
  int width; /* Temporarily override the global screen width 
                definition. */
  
  /* Retrieve the screen width value of the main window. */
  XtVaGetValues ( main_window ,
                  XmNwidth , &width , NULL ) ;
  
  return screen_width;
}

/*****************************************************************************
 *
 * Routine: _get_toolbar
 *
 * Description: this routine returns the state of the toolbar.
 *
 ****************************************************************************/

int _get_toolbar()
{
  return(toolbar);
}

/*****************************************************************************
 *
 * Routine: _set_map_change
 *
 * Description: this routine sets the map_change item to true
 *
 ****************************************************************************/

void _set_map_change(int index)
{
  map[index].map_change = 1;
}

/*****************************************************************************
 *
 * Routine: _get_map_width
 *
 * Description: this routine returns the map width
 *
 ****************************************************************************/

int _get_map_width(int index)
{
  return(map[index].width);
}

/*****************************************************************************
 *
 * Routine: _get_map_height
 *
 * Description: this routine returns the map height
 *
 ****************************************************************************/

int _get_map_height(int index)
{
  return(map[index].height);
}

/*****************************************************************************
 *
 * Routine: _get_num_of_maps
 *
 * Description: this routine returns the status of the legend
 *
 ****************************************************************************/

int _get_num_of_maps()
{
  return(num_of_maps);
}

/*****************************************************************************
 *
 * Routine: _set_num_of_maps
 *
 * Description: this routine sets the number of maps. 
 *
 ****************************************************************************/

void _set_num_of_maps( int number )
{
  num_of_maps = number;
}


/*****************************************************************************
 *
 * Routine: _get_map_widget
 *
 * Description: this routine returns the status of the legend
 *
 ****************************************************************************/

Widget _get_map_widget(int index)
{
  return(map[index].map);
}

/*****************************************************************************
 *
 * Routine: _get_maps_pixmap
 *
 * Description: this routine returns the pixmap for the given map
 *
 ****************************************************************************/

Pixmap _get_maps_pixmap(int index)
{
  return(map[index].pixmap);
}

/*****************************************************************************
 *
 * Routine: _get_states_pixmap
 *
 * Description: this routine returns the pixmap used to store the 
 *              state boundaries, the Canadian Provinces, and 
 *              Mexico. 
 *
 ****************************************************************************/

Pixmap _get_states_pixmap ( int index )
{
   return states ;
}

/*****************************************************************************
 *
 * Routine: _get_legend_pixmap
 *
 * Description: this routine returns the pixmap for the given map
 *
 ****************************************************************************/

Pixmap _get_legend_pixmap(int index)
{
  return(map[index].lmap);
}

/*****************************************************************************
 *
 * Routine: _get_map_shell
 *
 * Description: this routine returns the map_shell widget
 *
 ****************************************************************************/
Widget _get_map_shell()
{
  return(map_shell);
}


void _create_pixmaps(int index,int width,int height)
{
  int w , h ;

  if (legend_loc == M_BOTTOM)
  {
     w = width;
     h = legend_height ;
  }
  else if ( legend_loc == M_TOP )  
  {
     w = 50;
     h = height;
  } 
  else
  {
     w = legend_width ;
     h = legend_height ;
  }

  map[index].lmap = XCreatePixmap(_get_map_display(),
				  RootWindowOfScreen(XtScreen(map_form)),
				  w,
				  h,
	 			  DefaultDepthOfScreen(XtScreen(map_form)));
    
  map[index].pixmap = XCreatePixmap(_get_map_display(),
				    RootWindowOfScreen(XtScreen(map_form)),
				    width,
				    height,
				    DefaultDepthOfScreen(XtScreen(map_form)));
  map[index].lheight = h;
}  
/*****************************************************************************
 *
 * Routine: _focus_change
 *
 * Description: This routine performs special processing based on
 *              focus changes on the main window.
 *
 ****************************************************************************/

static void _focus_change ( Widget wid , XtPointer client_data , 
                            XEvent * event , Boolean * flag )
{
   int index = ( int ) client_data ;

   if ( map [ index ].froutine != NULL )
   {
      map [ index ].froutine ( ) ;
   }
}


/*****************************************************************************
 *
 * Routine: _create_single_map()
 *
 * Description: this routine creates a single map draw area and legend for map.
 *
 ****************************************************************************/

void _create_single_map(Widget form,int index,int x,int y,int width,int height)
{
  int x1,y1,w1,h1,h;
  pane[index] = XtVaCreateWidget("pane",xmFormWidgetClass,form,
				 XmNx,x,
				 XmNy,y,
				 XmNwidth,width,
                                 XmNheight,height,
				 NULL);
  if (legend_loc == M_BOTTOM)
  {
    x1 = x;
    w1 = width;
    h1 = height - legend_height ;
    h = legend_height ;
    y1 =  h1;
    y = 0;

    /*if (legend == M_ON)
    {
       height = h1;
    }*/

  }
  else if ( legend_loc == M_TOP ) 
  {
    x1 = width - 120;
    w1 = 120;
    h1 = height;
    h = h1;
    y1 = 0;
    y = 0;
    if (legend == M_ON)
      width = x1;
  }
  else
  {
     x1 = 0 ;
     y1 = 0 ;
     w1 = legend_width ;
     h = legend_height ;
  }
  
  map[index].lx = x1;
  map[index].ly = y1;
  map[index].legend_change = 1;
  map[index].legend = XtVaCreateWidget("legend",xmDrawingAreaWidgetClass,
				       pane[index],
				       XmNx,x1,
				       XmNy,y1,
				       XmNwidth,w1,
				       XmNheight,h,
				       XmNbackground,_get_color(background),
				       XmNborderColor,_get_color("Black"),
				       XmNborderWidth,2,
				       XmNbottomAttachment,XmATTACH_FORM,
                                       XmNleftAttachment,XmATTACH_FORM,
                                       XmNrightAttachment,XmATTACH_FORM,
				       NULL);

  map[index].map =  XtVaCreateWidget("map",xmDrawingAreaWidgetClass,
				     pane[index],
				     XmNx,x,
				     XmNy,y,
				     XmNwidth,width,
				     XmNheight,height,
				     XmNbackground,_get_color(background),
				     XmNborderColor,_get_color("Black"),
				     XmNborderWidth,2,
				     NULL);

  map[index].y = y;
  map[index].width = width;
  map[index].height = height;    
  map[index].map_change = 1;

  /* Create the pixmaps to accompany the map and legend. */
  _create_pixmaps(index,width,height);


  XtAddCallback(map[index].map,XmNexposeCallback,_map_expose,(XtPointer)index);
  XtAddCallback(map[index].map,XmNresizeCallback,_map_expose,(XtPointer)index);
  XtAddCallback(map[index].legend,XmNexposeCallback,_legend_expose,
		(XtPointer)index);
  XtAddEventHandler(map[index].map,ButtonPressMask,FALSE,_select_routine,
		    (XtPointer)index);
  XtAddEventHandler(map[index].map,ButtonReleaseMask,FALSE,_select_routine,
		    (XtPointer)index);
  XtAddEventHandler(map[index].legend,ButtonPressMask,FALSE,
                    _legend_select_routine, (XtPointer)index);
  XtAddEventHandler(map[index].legend,ButtonReleaseMask,FALSE,
                    _legend_select_routine, (XtPointer)index);
  XtAddEventHandler(map[index].map,PointerMotionMask,FALSE,_cursor_location,
		    (XtPointer)index);
  XtAddEventHandler ( _get_map_shell ( ) , FocusChangeMask , FALSE ,
                      _focus_change ,
                      ( XtPointer ) index ); 
  XtAddEventHandler(map[index].map,LeaveWindowMask,FALSE,_leave_window,
		    (XtPointer)index);
}

/*****************************************************************************
 *
 * Routine: _create_two_maps()
 *
 * Description: this routine creates a two map draw areas and legends for 
 *  each map.
 *
 ****************************************************************************/

void _create_2nd_map ()
{
   int height1;
   int height2;
   int nx1;
   int nx2;
   int ny1;
   int ny2;
   int width1;
   int width2;

   /* Determine the geometry of the two windows. */
   width1 = screen_width;
   width2 = screen_width;
   height1 = screen_height/2;
   height2 = height1;

   /* Determine the locations to start drawing the maps on 
      the main form. */
   nx1 = 0;
   ny1 = 1;
   nx2 = 0;
   ny2 = height1 + 1;
   
   /* Update the existing map to make it smaller. */
   map [ 0 ].height = height1;
   XtVaSetValues ( map[0].map, XmNheight, height1, NULL );

   /* Update the position of the existing map's legend. */
   map [ 0 ].ly = map[0].height - legend_height;
   XtVaSetValues ( map[0].legend, XmNy, map [ 0 ].ly, NULL );

   /* Free the pixmap. */ 
   XFreePixmap ( _get_map_display ( ), map[0].pixmap );
   XFreePixmap ( _get_map_display ( ), map[0].lmap );
   _create_pixmaps ( 0, screen_width, map [ 0 ].height );

   /* Create the second (bottom) map, index 1. */
   _create_single_map(map_form,1,nx2,ny2,width2,height2);

   /* Manage the form and the drawing area of the second map. */
   XtManageChild ( pane [ 1 ] );
   XtManageChild ( map [ 1 ].map );
   XtManageChild ( map [ 1 ].legend );
}


/*****************************************************************************
 *
 * Routine: _create_two_maps()
 *
 * Description: this routine creates a two map draw areas and legends for 
 *  each map.
 *
 ****************************************************************************/

void  _create_two_maps(Widget form,int width,int height)
{
   int height1;
   int height2;
   int nx1;
   int nx2;
   int ny1;
   int ny2;
   int width1;
   int width2;

   /* Determine the geometry of the two windows. */
   width1 = width;
   width2 = width;
   height1 = height/2;
   height2 = height1;

   /* Determine the locations to start drawing the maps on 
      the main form. */
   nx1 = 0;
   ny1 = 1;
   nx2 = 0;
   ny2 = height1 + 1;
   
   /* Create single map, index 0. */
   _create_single_map(form,0,nx1,ny1,width1,height1);

   /* Create single map, index 1. */
   _create_single_map(form,1,nx2,ny2,width2,height2);
}

/*****************************************************************************
 *
 * Routine: _create_four_maps()
 *
 * Description: this routine creates a 4 map draw areas and legends for each
 * map.
 *
 ****************************************************************************/

void _create_four_maps(Widget form,int width,int height)
{
}
/*****************************************************************************
 *
 * Routine: _create_map()
 *
 * Description: this routine creates a map draw area and legend for each map.
 *
 ****************************************************************************/

void _create_map(Widget form,int distance,int width,int height)
{
  int w = width ;
  int h = height ;

  if (num_of_maps == 1){
    _create_single_map(form,0,1,distance,w,h);
  }
  else if (num_of_maps == 2){
    h = height - distance;
    _create_two_maps(form,width-5,h);
  }
  else if (num_of_maps == 4){
    w = (width - 5)/2;
    h = (height - distance)/2;
    _create_four_maps(form,width-5,height-distance);
  }

  states = XCreatePixmap(_get_map_display(),
			 RootWindowOfScreen(XtScreen(form)),
			 w,
			 h,
			 DefaultDepthOfScreen(XtScreen(form)));
  
  map_pixmap = XCreatePixmap(_get_map_display(),
			     RootWindowOfScreen(XtScreen(form)),
			     w,
			     h,
			     DefaultDepthOfScreen(XtScreen(form)));
  
}

/*****************************************************************************
 *
 * Routine: _turn_toolbar_on
 *
 * Description: this routine displays the map toolbar.
 *
 ****************************************************************************/

void _turn_toolbar_on()
{

  _manage_toolbar();
  XtVaSetValues ( scrolled_window ,
                  XmNtopAttachment , XmATTACH_WIDGET ,
                  XmNtopWidget , tool_form ,
                  NULL ) ;
  toolbar = M_ON;
  XtUnmanageChild ( scrolled_window ) ;
  XtManageChild ( scrolled_window );
}

/*****************************************************************************
 *
 * Routine: _turn_toolbar_off
 *
 * Description: this routine closes the map toolbar.
 *
 ****************************************************************************/

void _turn_toolbar_off()
{
  XtVaSetValues ( scrolled_window ,
                  XmNtopAttachment , XmATTACH_WIDGET ,
                  XmNtopWidget , map_menubar ,
                  NULL ) ;
  _unmanage_toolbar ( ) ;
  toolbar = M_OFF;
  XtManageChild ( scrolled_window );
}

/*****************************************************************************
 *
 * Routine: _turn_legend_on
 *
 * Description: this routine closes the map screen.
 *
 ****************************************************************************/

void _turn_legend_on()
{
  int i ;

  legend = M_ON ;

  for ( i = 0 ; i < num_of_maps ; i++ )
  {
    if ( legend_loc == M_BOTTOM )
    {
      XtManageChild ( map[i].legend ) ;
      mUpdateLegend ( i ) ;
    }
    else
    {
       /* Other menu locations, such as across the top of the screen
          will need to be handled as the need arises. */
    }
  }
}
                       
/*****************************************************************************
 *
 * Routine: _turn_legend_off
 *
 * Description: this routine closes the map screen.
 *
 ****************************************************************************/

void _turn_legend_off()
{
  int i ;

  legend = M_OFF ;

  for ( i = 0 ; i < num_of_maps ; i++ )
  {
    if ( legend_loc == M_BOTTOM )
    {
      XtUnmanageChild ( map[i].legend ) ;
    }
    else
    {
       /* Other menu locations, such as across the top of the screen, will 
          need to be handled here as the need arises. */
    }
  }
}

/*****************************************************************************
 *
 * Routine: mCloseMapScreen
 *
 * Description: this routine closes the map screen.
 *
 ****************************************************************************/

void mCloseMapScreen()
{
  XtPopdown(map_shell);
}
/*****************************************************************************
 *
 * Routine: mGetLegendStatus
 *
 * Description: this routine returns where the legend is on or off
 *
 ****************************************************************************/

int mGetLegendStatus()
{
  return(legend);
}

/*****************************************************************************
 *
 * Routine: mAddLegendDrawCb
 *
 * Description: This routine specifies the call back routine that will create
 *              and manage the map legend when the user specifies that it 
 *              should be placed outside of the actual map gui. 
 *
 ****************************************************************************/
int mAddLegendDrawCb ( int index , MotionCallback legend_routine )
{
   int status = M_OK ;

   if ( index >= MAX_NUM_MAPS || index < 0 )
   {
      status = M_ERROR ;
   }

   map [ index ].legend_draw = legend_routine ;

   return status ;
}   

/*****************************************************************************
 *
 * Routine: mAddLegendExpose
 *
 * Description: this routine sets routine for given legend as expose callback
 *
 ****************************************************************************/

void mAddLegendExpose(int index,void (*routine)())
{
  if (index >= MAX_NUM_MAPS)
    printf("Error: There is no map for the given reference(%d). "
           "There are only %d maps define.",index,MAX_NUM_MAPS);
  else
    map[index].lroutine = routine;
}

/*****************************************************************************
 *
 * Routine: mAddMotionCallback
 *
 * Description: This routine sets the a user-supplied routine to be called 
 *              when a mouse motion event is sensed.  This gives the user
 *              the option to supply functionality in addition to that 
 *              already provided by the map library.
 *
 ****************************************************************************/

void mAddMotionCallback ( int index ,
                          MotionCallback routine )
{
  if (index >= MAX_NUM_MAPS)
    printf("Error: There is no map for the given reference(%d). "
           "There are only %d maps define.",index,MAX_NUM_MAPS);
  else
    map[index].mroutine = routine;
}

/*****************************************************************************
 *
 * Routine: mAddLeaveCallback
 *
 * Description: This routine sets the user-supplied routine to be called 
 *              when the mouse pointer leaves the specified map's drawing
 *              area.  This gives the user the option to supply functionality
 *              in addition to that already provided by the map library.
 *
 ****************************************************************************/
void mAddLeaveCallback ( int index ,
                         MotionCallback routine )
{
  if (index >= MAX_NUM_MAPS)
    printf("Error: There is no map for the given reference(%d). "
           "There are only %d maps define.",index,MAX_NUM_MAPS);
  else
    map[index].wroutine = routine;
}

/*****************************************************************************
 *
 * Routine: mAddAboutAppCallback
 *
 * Description: This function sets the user-supplied routine to be called 
 *              when the "About" item on the "Help" menu is selected.
 *              The user-supplied routine should perform some operation
 *              or launch a GUI that describes the Hydromap application, 
 *              its version, and the date the version was released.
 *
 ****************************************************************************/
void mAddAboutAppCallback ( MotionCallback routine ) 
{
   if ( routine != NULL )
   {
      about_app_cb = routine ;
   }
} 

/*****************************************************************************
 *
 * Routine: _get_about_app_cb
 *
 * Description: This function returns the user-supplied routine to be called 
 *              when the "About" item on the "Help" menu is selected.
 *              Note that the user must first call mAddAboutAppCallback in
 *              order to set the callback routine.
 *
 *              If no callback routine has been set, then this routine will
 *              return a NULL value.
 *
 ****************************************************************************/

MotionCallback _get_about_app_cb ( ) 
{
   return about_app_cb ;
}

/*****************************************************************************
 *
 * Routine: mAddMapExpose
 *
 * Description: this routine sets routine for given map as expose callback
 *
 ****************************************************************************/

void mAddMapExpose(int index,void (*routine)())
{
  if (index >= MAX_NUM_MAPS)
    printf("Error: There is no map for the given reference(%d). "
           "There are only %d maps define.",index,MAX_NUM_MAPS);
  else
    map[index].routine = routine;
}

/*****************************************************************************
 *
 * Routine: mAddMapSelectRoutine
 *
 * Description: this routine sets the routine to be called when the mouse click
 *    occurs in the given map.
 *
 * History:   Bryon Lawrence    June 11, 2001
 *            Reveresed the calling arguments to this function to 
 *            reflect the ordering as shown in the prototype.
 *
 ****************************************************************************/

void mAddMapSelectRoutine( int index , void (*routine) () )
{
 
  if (index >= MAX_NUM_MAPS)
    printf("Error: There is no map for the given reference(%d).  There are only %d maps define.",index,MAX_NUM_MAPS);
  else
    map[index].sroutine = routine;
 
}

/*****************************************************************************
 *
 * Routine: mAddLegendSelectRoutine
 *
 * Description: this routine sets the routine to be called when the mouse click
 *    occurs in the given map legend.
 *
 * History:   Bryon Lawrence     April 11, 2006   Created.
 ****************************************************************************/

void mAddLegendSelectRoutine( int index , MouseClickEventCallback routine )
{
 
  if (index >= MAX_NUM_MAPS)
  {
    printf("Error: There is no map for the given reference(%d). "
           "There are only %d maps define.",index,MAX_NUM_MAPS);
  }
  else
  {
    map[index].lsroutine = routine;
  }
 
}

/*****************************************************************************
 *
 * Routine: mAddFocusChangeRoutine
 *
 * Description: This routine allows the user to add a routine which is called
 *              whenever a FocusIn or a FocusOut event is captured.  This
 *              routine can be set individually for each of the maps 
 *              created.
 *
 ****************************************************************************/
void mAddFocusChangeRoutine ( int index , GeneralCallback routine )
{
   map [ index ].froutine = routine ;
} 

/*****************************************************************************
 *
 * Routine: mOpenMapScreen
 *
 * Description: this routine displays the map screen
 *
 ****************************************************************************/

void mOpenMapScreen ( )
{
   if (map_shell == NULL)
   {
      printf("Error: Must create the map screen before opening it.\n");
   }
   else
   {
      XtManageChild ( main_window ) ;

      if ( stand_alone == 0 )
      {
         XtPopup(map_shell,XtGrabNone) ;
      }
      else
      {
         XtRealizeWidget ( map_shell ) ;
         XtAppMainLoop ( app ) ;
      }
   }

}

/*****************************************************************************
 *
 * Routine: mSetMapSize
 *
 * Description: this routine set the size of the maps
 *
 ****************************************************************************/

void mSetMapSize(int width,int height)
{
  int i;

  for (i=0; i < num_of_maps; i++){
    map[i].width = width;
    map[i].height = height;
    map[i].map_change = M_ON;
    if (legend_loc == M_BOTTOM){
      map[i].ly = height - legend_height;
      XtVaSetValues(map[i].legend,XmNy,map[i].ly,XmNx,1,XmNwidth,width,NULL);
    }

    XtVaSetValues(pane[i],XmNwidth,width,XmNheight,height,NULL);
    if (legend == M_ON && legend_loc == M_BOTTOM)
      XtVaSetValues(map[i].map,XmNwidth,width,XmNheight,map[i].ly,NULL);
    else if (legend == M_OFF)
      XtVaSetValues(map[i].map,XmNwidth,width,XmNheight,height,NULL);
  }

  make_map = M_ON;
  _update_map_variables();
}

/**************************************************
 *
 * Routine: mChangeTitle
 *
 * Description: this routine changes the map screen title
 *
 ****************************************************************************/
void mChangeTitle(char *text)
{
  if (map_shell == NULL)
    printf("Error: map screen MUST be created first.\n");
  else
    XtVaSetValues(map_shell,XmNtitle,text,NULL);
}

void mInitMapScreen ( int height , int width , int x , int y , 
                      float nmi , int tool , int toolbar_location ,
                      char * title )
{
  int i ;

  for ( i = 0 ; i < MAX_NUM_MAPS ; ++ i )
  {
     map [ i ].pixmap = 0 ;
     map [ i ].map = NULL ;
     map [ i ].legend = NULL ;
     map [ i ].lmap = 0 ;
     map [ i ].x = 0 ;
     map [ i ].y = 0 ;
     map [ i ].width = 0 ;
     map [ i ].height = 0 ;
     map [ i ].lx = 0 ;
     map [ i ].ly = 0 ;
     map [ i ].map_change = 0 ;
     map [ i ].legend_change = 0 ;
     map [ i ].routine = NULL ;
     map [ i ].lroutine = NULL ;
     map [ i ].mroutine = NULL ;
     map [ i ].wroutine = NULL ;
     map [ i ].legend_draw = NULL ;
     map [ i ].sroutine = NULL ;
     map [ i ].lsroutine = NULL;
  }
     
  width_in_nmi = nmi ;
  screen_width = width;
  screen_height = height;
  toolbar = tool;
  toolbar_loc = toolbar_location;
  screen_x = x;
  screen_y = y;
  strcpy(screen_title,title);
}

int mInitLegend ( int leg , int legend_location , int width , int height )
{
   int status = M_OK ;

   legend = leg ;
   legend_loc = legend_location ;

   /* Determine the legend location.  If the legend is located at the 
      bottom or the top of the map screen, then its width will default to
      the map's width.  If the legend is located outside of the application,
      then the user-supplied width and height are used. */
   if ( legend_loc == M_TOP || legend_loc == M_BOTTOM )
   {
      legend_width = screen_width ;
      legend_height = height ;
   }
   else if ( legend_loc == M_OUTSIDE )
   {
      legend_width = width ;
      legend_height = height ;
   }
   else 
   {
      status = M_ERROR ;
   }

   return status ;
}

void mInitMap ( int num_maps , int overv ,char * bgcolor ,
                char * border_color)
{
  overview = overv;
  num_of_maps = num_maps;
  strcpy(background,bgcolor);
  strcpy(state_border_color,border_color);
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
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
#define TOOL_BAR_FORM_HEIGHT 30 /* The initial height of the toolbar. */

void mCreateMapScreen ( Widget parent , char res_title[] )
{
  Atom WM_DELETE_WINDOW ;
  int i ;
  int argc = 0 ;
  char ** argv = NULL ;
  int distance_down = 0 ;
  unsigned long pixel ;

  /* If the map has been already created, then write an error message. */
  if ( map_shell != NULL )
  {
     printf ( "Error: map screen already exists.\n" ) ;
     return ;
  }
 
  /* Determine if the user wants this to be a stand-alone 
     application, or if it is being launched from an 
     existing X/Motif application.  This is indicated by whether or not
     the widget id passed into this routine is NULL. */
  if ( parent == NULL )
  {
     /* This is a stand alone application. */
     stand_alone = 1 ;
     XtSetLanguageProc ( NULL , NULL , NULL ) ;
     map_shell = XtVaAppInitialize ( & app , res_title , NULL , 0 ,
                                     & argc , argv , NULL , XmNwidth ,
                                     screen_width , XmNheight , screen_height ,
                                     XmNmaxWidth , screen_width ,
                                     XmNmaxHeight , screen_height ,
                                     XmNtitle , screen_title ,
				     XmNdeleteResponse, XmDO_NOTHING ,
                                     NULL ) ;
     _get_map_resources( map_shell ) ;
  }   
  else
  {
     /* This application is being launched as a "pop up" shell from
        an existing X/Motif program. */

     stand_alone = 0 ;
     _get_map_resources(parent);

     /* Create the "pop up" shell that will be used to contain the 
        graphical application. */
     map_shell = XtVaCreatePopupShell(screen_title,topLevelShellWidgetClass,
    		                      parent,
		                      XmNwidth,screen_width + 11 ,
				      XmNheight,screen_height + 41 ,
				      XmNx,screen_x,
				      XmNy,screen_y,
                                      XmNmaxWidth , screen_width  + 11,
                                      XmNmaxHeight , screen_height + 41 ,
                                      XmNtitle , screen_title ,
				      XmNdeleteResponse, XmDO_NOTHING ,
				      NULL);
  }

  /* Create a form geometry management window. */ 
  main_window = XtVaCreateManagedWidget ( "main_window" ,
                                          xmFormWidgetClass ,
                                          map_shell ,
                                          XmNwidth , screen_width ,
                                          XmNheight , screen_height ,
                                          XmNbackground ,
                                          _get_color ( "LightSteelBlue3" ) ,
                                          NULL ) ;
  /* Create the menubar. */
  _create_map_menubar ( main_window ) ;

  /* Create a Form Geometry management window.  This will become
     the toolbar for the Main Window, which will be configured to
     reside in either the command or message portions of the main
     window widget. */
  tool_form = XtVaCreateWidget( "tool_form" ,
                                xmDrawingAreaWidgetClass ,
 	                        main_window ,
		        	XmNwidth , screen_width ,
				XmNheight , TOOL_BAR_FORM_HEIGHT ,
                                XmNleftAttachment , XmATTACH_FORM ,
                                XmNrightAttachment , XmATTACH_FORM ,
                                XmNtopAttachment , XmATTACH_WIDGET ,
                                XmNtopWidget , map_menubar ,
				XmNbackground ,
                                _get_color ( "LightSteelBlue3" ) ,
				NULL );

  XtAddCallback ( tool_form , XmNresizeCallback , _resize_map_toolbar ,
                  ( XtPointer ) 1 ) ;

  /* Create the toolbar. */
  _create_map_toolbar ( tool_form , toolbar_loc , overview ) ;

  /* Create a Scrolled Window geometry management widget. */
  if ( toolbar == M_ON )
  {
     scrolled_window = XtVaCreateManagedWidget ( "map_window" ,
                                                 xmScrolledWindowWidgetClass ,
                                                 main_window ,
                                                 XmNwidth , screen_width ,
                                                 XmNheight , screen_height ,
                                                 XmNscrollingPolicy ,
                                                 XmAUTOMATIC ,
                                                 XmNtopAttachment ,
                                                 XmATTACH_WIDGET ,
                                                 XmNtopWidget ,
                                                 tool_form ,
                                                 XmNleftAttachment , 
                                                 XmATTACH_FORM ,
                                                 XmNrightAttachment , 
                                                 XmATTACH_FORM ,
                                                 XmNbottomAttachment ,
                                                 XmATTACH_FORM ,
                                                 NULL ) ;
  }
  else
  {
     scrolled_window = XtVaCreateManagedWidget ( "map_window" ,
                                                 xmScrolledWindowWidgetClass ,
                                                 main_window ,
                                                 XmNwidth , screen_width ,
                                                 XmNheight , screen_height ,
                                                 XmNscrollingPolicy ,
                                                 XmAUTOMATIC ,
                                                 XmNtopAttachment ,
                                                 XmATTACH_WIDGET ,
                                                 XmNtopWidget ,
                                                 map_menubar ,
                                                 XmNleftAttachment , 
                                                 XmATTACH_FORM ,
                                                 XmNrightAttachment , 
                                                 XmATTACH_FORM ,
                                                 XmNbottomAttachment ,
                                                 XmATTACH_FORM ,
                                                 NULL ) ;
  }
                              
  /* Create a Form geometry management window.
     This will become the work area of the Main Window. */
  map_form = XtVaCreateManagedWidget( "map_form" , xmFormWidgetClass ,
				      scrolled_window ,
		                      XmNwidth , screen_width ,
				      XmNheight , screen_height ,
				      XmNbackground ,
                                      _get_color ( "LightSteelBlue3" ) ,
				      NULL ) ;
                              
  XtVaSetValues ( scrolled_window ,
                  XmNworkWindow , map_form ,
                  NULL ) ;

  _create_map(map_form,distance_down,screen_width,screen_height);
  _create_gc(map[0].map);
  _init_map_variables ( width_in_nmi ) ;

  /* Set the background color in the graphics context.  This will
     be needed for the XOR graphics context. */
  pixel = _get_color ( background ) ;
  XSetBackground ( _get_map_display ( ) , _get_map_gc ( ) , pixel ) ;

  /* Set the initial font size. */
  mSetFont ( M_SMALL ) ;

  /* manage the widgets */
  _manage_menubar();

  /* If the user initially wants the toolbar to be displayed,
     then manage it. */ 
  if ( toolbar == M_ON )
  {
     XtManageChild ( tool_form ) ;
  }

  for (i=0;i < num_of_maps; i++){
    XtManageChild(pane[i]);
    XtManageChild(map[i].map);
    
    if (legend == M_ON) XtManageChild(map[i].legend);
  }

WM_DELETE_WINDOW = XmInternAtom ( _get_map_display ( ) , "WM_DELETE_WINDOW" ,
		                    False ) ;


  XmAddWMProtocolCallback ( map_shell , WM_DELETE_WINDOW , _close_map_screen ,
			    (XtPointer) & stand_alone ) ;
  
}

/*****************************************************************************
 *
 * Routine: mCreateOverviewMap
 *
 * Description: this routine creates a overview map
 *
 ****************************************************************************/

/*****************************************************************************
 *
 * Routine: mCreateLocateCoordinateMap
 *
 * Description: this routine creates a map that allow the user to define
 *  a region
 *
 ****************************************************************************/

/*****************************************************************************
 *
 * Routine: mSetMapBackgroundColor
 *
 * Description: this routine sets the color for the map backgrounds.
 *
 ****************************************************************************/
void mSetMapBackgroundColor(char *color)
{
  int i;

  strcpy(background,color);
  for (i=0;i<num_of_maps;i++){
    map[i].map_change = 1;
    _map_expose(map[i].map,(XtPointer)i,NULL);
  }
}

/*****************************************************************************
 *
 * Routine: mAddMapCleanUp
 *
 * Description: This routine sets the callback to free system resources used
 *              by the application such as dynamically allocated memory or
 *              open files.  This routine is called when the application is
 *              is being shut down.
 *
 ****************************************************************************/
void mAddMapCleanUp (  CleanCallback clean_routine ) 
{
   /* Test for a NULL input value. */
   if ( clean_routine == NULL )
   {
      clean_routine_cb = NULL ;
   }
   else
   {
      /* Set the clean up routine pointer that is global to this file
         to the user-supplied routine name. */
      clean_routine_cb = clean_routine ;
   }
}

/*****************************************************************************
 *
 * Routine:     _get_clean_routine 
 *
 * Description: This routine gives the world outside of this file access
 *              to the "clean routine".  This accessor function is being
 *              created to prevent the overuse of global variables, a 
 *              practice which, unfortunately, has been taken to an 
 *              extreme in the "map library" already.
 *
 ****************************************************************************/

CleanCallback _get_clean_routine ( )
{
   return clean_routine_cb ;
}

/*****************************************************************************
* Routine:     _set_legend_height
*
* Description: This routine resets the height of the legend.  It allows the
*              legend to contain more or less information without wasting
*              any space in the geographic viewing area.
*****************************************************************************/

void _set_legend_height ( int map_index , int new_legend_height ,
                          int resize_pixmap )
{

   Display * display = NULL ;
   int new_legend_offset ;

   /* Recompute the key legend positioning attributes. */
   if ( legend_loc == M_BOTTOM )
   {
      new_legend_offset = map [ map_index ].height - new_legend_height ;
   }
   else
   {
      new_legend_offset = 0 ;
   }

   /* Test to determine if the pixmap needs to be resized. */
   if ( resize_pixmap == 1 )
   {
      /* Reset the variable that contains the legend height value. */
      legend_height = new_legend_height ;
      map [ map_index ].lheight = new_legend_height;

      /* Destroy the old legend pixmap.  Create a new one with the correct
         dimensions. */
      display = _get_map_display ( ) ;
      XFreePixmap ( display , map [ map_index ].lmap ) ;
      map [ map_index ].lmap = XCreatePixmap ( display ,
                             RootWindowOfScreen ( XtScreen ( map_form ) ) ,
                             map [ map_index ].width ,
                             new_legend_height ,
                             DefaultDepthOfScreen ( XtScreen ( map_form ) ) ) ;

      /* Fill the pixmap.  This is necessary because the legend has changed
         size. */
      _area_draw_fill_box(map[map_index].lmap,0,0,map[map_index].width,
                          legend_height);
   }

   /* Reset the attributes of the legend that define its size. */
     XtVaSetValues ( map [ map_index ].legend ,
                     XmNheight, new_legend_height ,
                     XmNy , new_legend_offset , NULL ) ;

   map [ map_index ].legend_change = 1 ;


}

/*****************************************************************************
* Routine:     _get_legend_widget
*
* Description: This routine returns the legend widget for the specified
*              map number.
*****************************************************************************/
Widget _get_legend_widget ( int map_index )
{
   return map [ map_index ].legend ;
}

/*****************************************************************************
* Routine:     mDisableExposeWatch
*
* Description: This routine disables the watch cursor pointer which is 
*              normally displayed when the map is redrawn (as during an
*              expose event). 
*****************************************************************************/
void mDisableExposeWatch ( ) 
{
   expose_watch_pointer_flag = M_OFF ;
}

/*****************************************************************************
* Routine:     mEnableExposeWatch
*
* Description: This routine enables the watch cursor pointer which is 
*              normally displayed when the map is redrawn (as during an
*              expose event). 
*****************************************************************************/
void mEnableExposeWatch ( )
{
   expose_watch_pointer_flag = M_ON ;
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
void mAddRecenterRoutine ( MotifCallback recenter_function )
{
   recenter_routine = recenter_function ;
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
void mSetUserPixmap ( Pixmap u_pixmap )
{
   user_pixmap = u_pixmap ;
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
Pixmap mGetUserPixmap ( )
{
   return user_pixmap ;
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
void set_map_height ( int index, int height )
{
   map [ index ].height = height;
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

int _get_legend_height ( )
{
   return legend_height;
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
int _get_legend_height_index ( int index )
{
   return map [ index ].lheight;
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
int _get_legend_width ( )
{
   return legend_width;
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
int _get_screen_height ( )
{
   return screen_height;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/whfs_lib/src/MapUtil/RCS/map.c,v $";
 static char rcs_id2[] = "$Id: map.c,v 1.27 2007/02/14 20:05:31 lawrence Exp lawrence $";}
/*  ===================================================  */

}
