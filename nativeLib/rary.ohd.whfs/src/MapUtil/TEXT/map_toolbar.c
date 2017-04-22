/*****************************************************************************
 *
 * Module: map_toolbar.c
 *
 * Description: contains routines to create map toolbar
 *
 *****************************************************************************/

/* include files */

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Separator.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include <Xm/PushB.h>

#include "GeneralUtil.h"
#include "map.h"
#include "map_defines.h"
#include "map_convert.h"
#include "map_menubar_cb.h"
#include "map_resource.h"

/* global variables */

Widget map_toolbar_up;
Widget map_toolbar_down;
Widget map_toolbar_left;
Widget map_toolbar_right;
Widget map_toolbar_recenter;
Widget map_toolbar_in;
Widget map_toolbar_out;
Widget map_lat_label;
Widget map_lon_label;
Widget map_lat_text;
Widget map_lon_text;

extern Widget tool_form;
extern Widget tool_form_off;
extern struct _Map map[4] ;

/* Definitions */

static const int MINIMUM_TOOLBAR_WIDTH = 540 ;

/*****************************************************************************
 *
 * Routine: _cursor_location
 *
 * Description: routine displays the cursor location on map in the toolbar
 *
 *****************************************************************************/

void _cursor_location(Widget wid,XtPointer clientdata,XEvent *event,
		      Boolean *flag)
{
  float lat,lon;
  char str[10];
  int index = (int) clientdata;

  /* Check to determine if the user has supplied a callback routine
     for the mouse-motion event. This will add functionality 
     in addition to what the map library provides. */
  if ( map[index].mroutine != NULL )
  {
     map[index].mroutine ( wid , clientdata , event ) ; 
  }

  mConvertXY2LatLon(event->xmotion.x,event->xmotion.y,&lat,&lon);

  sprintf(str,"%7.2f",lat);
  XmTextSetString(map_lat_text,str);
  
  sprintf(str,"%7.2f",lon);
  XmTextSetString(map_lon_text,str);
  
}

/*****************************************************************************
 *
 * Routine: _leave_window
 *
 * Description: Routine calls a user-specified callback routine
 *
 *****************************************************************************/
void _leave_window ( Widget wid , XtPointer clientdata , XEvent *event ,
	             Boolean *flag )
{
  int index = ( int ) clientdata ;

  /* Check to determine if the user has supplied a callback routine
     for the mouse-motion event. This will add functionality 
     in addition to what the map library provides. */
  if ( map[index].wroutine != NULL )
  {
     map[index].wroutine ( wid , clientdata , event ) ; 
  }

}


/*****************************************************************************
 *
 * Routine: _manage_toolbar
 *
 * Description: routine manages all the toolbar widgets
 *
 *****************************************************************************/

void _manage_toolbar()
{
 Widget parent = XtParent ( map_toolbar_up ) ;
 XtManageChild(map_toolbar_up);
 XtManageChild(map_toolbar_down);
 XtManageChild(map_toolbar_left);
 XtManageChild(map_toolbar_right);
 XtManageChild(map_toolbar_recenter);
 XtManageChild(map_toolbar_in);
 XtManageChild(map_toolbar_out);
 XtManageChild(map_lat_label);
 XtManageChild(map_lat_text);
 XtManageChild(map_lon_label);
 XtManageChild(map_lon_text);
 XtManageChild(parent);
}

/*****************************************************************************
 *
 * Routine: _unmanage_toolbar
 *
 * Description: routine unmanges the toolbar widgets
 *
 *****************************************************************************/

void _unmanage_toolbar()
{
 Widget parent = XtParent ( map_toolbar_up );
 XtUnmanageChild(map_toolbar_up);
 XtUnmanageChild(map_toolbar_down);
 XtUnmanageChild(map_toolbar_left);
 XtUnmanageChild(map_toolbar_right);
 XtUnmanageChild(map_toolbar_recenter);
 XtUnmanageChild(map_toolbar_in);
 XtUnmanageChild(map_toolbar_out);
 XtUnmanageChild(map_lat_label);
 XtUnmanageChild(map_lat_text);
 XtUnmanageChild(map_lon_label);
 XtUnmanageChild(map_lon_text);
 XtUnmanageChild( parent );
}

/*****************************************************************************
 *
 * Routine: _create_toolbar(Widget form)
 *
 * Description: routine creates the toolbar for the map screen
 *
 *****************************************************************************/

void _create_toolbar(Widget form)
{
  int x, istatus, path_len= 120, rpath_len = 120;
  char * hv_config_dir = "hv_config_dir" ;
  char path[120], buffer[150];
  XmString str;
  Pixmap pix;
  
  path_len = strlen ( hv_config_dir ) ;
  istatus = get_apps_defaults ( hv_config_dir , & path_len , path , 
                                & rpath_len);

  if ( istatus == 0 )
  {
     sprintf(buffer,"%s/%s",path,"north.xpm");
  }
  else
  {
     sprintf(buffer,"%s","north.xpm");
  }

  pix = XmGetPixmap(XtScreen(form),buffer,
		    _get_color("Black"),_get_color("White"));

  map_toolbar_up = XtVaCreateWidget("map_toolbox_up",
					   xmPushButtonWidgetClass,
					   form,
					   XmNy,0,
					   XmNx,5,
					   XmNwidth,32,
					   XmNheight,32,
					   XmNlabelType,XmPIXMAP,
					   XmNlabelPixmap,pix,
					   XmNforeground,_get_color("Black"),
					   XmNbackground,_get_color("HotPink4"),
					   NULL);
  
  if ( istatus == 0 )
  {
     sprintf(buffer,"%s/%s",path,"south.xpm");
  }
  else
  {
     sprintf(buffer,"%s","south.xpm");
  }

  pix = XmGetPixmap(XtScreen(form), buffer,
		    _get_color("Black"),_get_color("White"));

  map_toolbar_down = XtVaCreateWidget("map_toolbox_down",
					     xmPushButtonWidgetClass,
					     form,
					     XmNy,0,
					     XmNx,40,
					     XmNwidth,32,
					     XmNheight,32,
					     XmNlabelType,XmPIXMAP,
					     XmNlabelPixmap,pix,
					     XmNforeground,_get_color("Black"),
					     XmNbackground,_get_color("HotPink4"),
					     NULL);
  
  if ( istatus == 0 )
  {
     sprintf ( buffer,"%s/%s",path,"west.xpm" ) ;
  }
  else
  {
     sprintf ( buffer,"%s","west.xpm" ) ;
  }

  pix = XmGetPixmap(XtScreen(form), buffer,
		    _get_color("Black"),_get_color("White"));

  map_toolbar_left = XtVaCreateWidget("map_toolbar_left",
					     xmPushButtonWidgetClass,
					     form,
					     XmNy,0,
					     XmNx,75,
					     XmNwidth,32,
					     XmNheight,32,
					   XmNlabelType,XmPIXMAP,
					   XmNlabelPixmap,pix,
					     XmNforeground,_get_color("Black"),
					     XmNbackground,_get_color("HotPink4"),
					     NULL);
  
  if ( istatus == 0 )
  {
     sprintf(buffer,"%s/%s",path,"east.xpm");
  }
  else
  {
     sprintf(buffer,"%s","east.xpm");
  }
  
  pix = XmGetPixmap(XtScreen(form), buffer,
		    _get_color("Black"),_get_color("White"));

  map_toolbar_right = XtVaCreateWidget("map_toolbar_right",
					      xmPushButtonWidgetClass,
					      form,
					      XmNy,0,
					      XmNx,110,
					      XmNwidth,32,
					      XmNheight,32,
					   XmNlabelType,XmPIXMAP,
					   XmNlabelPixmap,pix,
					      XmNforeground,_get_color("Black"),
					      XmNbackground,_get_color("HotPink4"),
					      NULL);
  
  if ( istatus == 0 )
  {
     sprintf(buffer,"%s/%s",path,"center.xpm");
  }
  else
  {
     sprintf(buffer,"%s","center.xpm");
  }

  pix = XmGetPixmap(XtScreen(form), buffer, 
		    _get_color("Black"),_get_color("White"));
  map_toolbar_recenter = XtVaCreateWidget("map_toolbar_recenter",
						 xmPushButtonWidgetClass,
						 form,
						 XmNy,0,
						 XmNx,145,
						 XmNwidth,32,
						 XmNheight,32,
					   XmNlabelType,XmPIXMAP,
					   XmNlabelPixmap,pix,
						 XmNforeground,_get_color("Black"),
						 XmNbackground,_get_color("HotPink4"),
						 NULL);
  
  if ( istatus == 0 )
  {
     sprintf(buffer,"%s/%s",path,"zoomin.xpm");
  }
  else
  {
     sprintf(buffer,"%s","zoomin.xpm");
  }

  pix = XmGetPixmap(XtScreen(form), buffer, 
		    _get_color("Black"),_get_color("White"));
  map_toolbar_in = XtVaCreateWidget("map_toolbar_in",
					   xmPushButtonWidgetClass,
					   form,
					   XmNy,0,
					   XmNx,180,
					   XmNwidth,32,
					   XmNheight,32,
					   XmNforeground,_get_color("Black"),
					   XmNlabelType,XmPIXMAP,
					   XmNlabelPixmap,pix,
					   XmNbackground,_get_color("HotPink4"),
					   NULL);
  
  if ( istatus == 0 )
  {
     sprintf(buffer,"%s/%s",path,"zoomout.xpm");
  }
  else
  {
     sprintf(buffer,"%s","zoomout.xpm");
  }

  pix = XmGetPixmap(XtScreen(form), buffer, 
		    _get_color("Black"),_get_color("White"));
  map_toolbar_out = XtVaCreateWidget("map_toolbar_out",
					    xmPushButtonWidgetClass,
					    form,
					    XmNy,0,
					    XmNx,215,
					    XmNwidth,32,
					    XmNheight,32,
					   XmNlabelType,XmPIXMAP,
					   XmNlabelPixmap,pix,
					    XmNforeground,_get_color("Black"),
					    XmNbackground,_get_color("HotPink4"),
					    NULL);
  
  x = _get_screen_width();
  printf ( "Inside create_toolbar ... screen_width = %d\n" , x ) ;
  map_lon_text = XtVaCreateWidget("map_lon_text",xmTextWidgetClass,
					 form,
					 XmNwidth,80,
					 XmNheight,30,
					 XmNy,0,
					 XmNx,x - 90,
					 XmNeditable,False,
					 XmNforeground,_get_color("Black"),
					 XmNbackground,_get_color("LightSteelBlue3"),
					 NULL);
  
  
  x = x - 90;
  str = XmStringCreateLocalized("Long:");
  map_lon_label = XtVaCreateWidget("map_lon_label",xmLabelWidgetClass,
					  form,
					  XmNwidth,60,
					  XmNheight,30,
					  XmNy,0,
					  XmNx,x - 65,
					  XmNforeground,_get_color("Black"),
					  XmNbackground,_get_color("LightSteelBlue3"),
					  XmNlabelString,str,
					  NULL);
  XmStringFree(str);
  x = x - 65;
  
  map_lat_text = XtVaCreateWidget("map_lat_text",xmTextWidgetClass,
					 form,
					 XmNwidth,80,
					 XmNheight,30,
					 XmNy,0,
					 XmNx,x - 85,
					 XmNeditable,False,
					 XmNforeground,_get_color("Black"),
					 XmNbackground,_get_color("LightSteelBlue3"),
					 NULL);
  
  x = x - 85;
  str = XmStringCreateLocalized("Lat:");
  map_lat_label = XtVaCreateWidget("map_lat_label",xmLabelWidgetClass,
			            form,
				    XmNwidth,50,
				    XmNheight,30,
					  XmNy,0,
					  XmNx,x - 55,
					  XmNforeground,_get_color("Black"),
					  XmNbackground,_get_color("LightSteelBlue3"),
					  XmNlabelString,str,
					  NULL);
  
  XmStringFree(str);
}

/*****************************************************************************
 *
 * Routine: _create_toolarea()
 *
 * Description: routine creates the tool area for the map screen
 *
 *****************************************************************************/

void _create_toolarea(Widget form,int overv)
{
}

/*****************************************************************************
 *
 * Routine: _set_toolbar_callbacks()
 *
 * Description: routine sets the toolbar callbackas
 *
 *****************************************************************************/

void _set_toolbar_callbacks()
{
  XtAddCallback(map_toolbar_in,XmNactivateCallback,_zoom_map_in,NULL);
  XtAddCallback(map_toolbar_out,XmNactivateCallback,_zoom_map_out,NULL);
  XtAddCallback(map_toolbar_up,XmNactivateCallback,_pan_map_up,NULL);
  XtAddCallback(map_toolbar_down,XmNactivateCallback,_pan_map_down,NULL);
  XtAddCallback(map_toolbar_right,XmNactivateCallback,_pan_map_right,NULL);
  XtAddCallback(map_toolbar_left,XmNactivateCallback,_pan_map_left,NULL);
  XtAddCallback(map_toolbar_recenter,XmNactivateCallback,_recenter_cb,NULL);
}

/*****************************************************************************
 *
 * Routine: _create_map_toolbar()
 *
 * Description: routine creates the toolbar for the map screen
 *
 *****************************************************************************/

void _create_map_toolbar(Widget form,int location,int overview)
{
  location = M_TOP;

  if (location == M_TOP){
    _create_toolbar(form);
  }
  else{
    _create_toolarea(form,overview);
  }

  _set_toolbar_callbacks();

}

/*****************************************************************************
 *
 * Routine: _resize_map_toolbar()
 *
 * Description: This routine ensures that if the window is resized, all of the
 *              toolbar information remains visible. 
 *
 *****************************************************************************/
void _resize_map_toolbar ( Widget wid , XtPointer ClientData ,
                           XtPointer CallData )
{
   Dimension width ;
   int x ;

   /* Retrieve the width of the parent. */
   XtVaGetValues ( tool_form ,
                   XmNwidth , &width ,
                   NULL ) ;
   x = ( int ) width;

   /* Check to see if the minimum size has been reached. */
   if ( x <= MINIMUM_TOOLBAR_WIDTH )
   {
      x = MINIMUM_TOOLBAR_WIDTH ;
   }
   

   /* Unmanage the map_lon_text, map_lon_label, map_lat_text, and
      map_lat_label widgets. */

   /* Reposition these widgets. */
   x -= 90 ;
   XtVaSetValues ( map_lon_text ,
                   XmNx , x ,
                   NULL ) ;
   x -= 65 ;
   XtVaSetValues ( map_lon_label ,
                   XmNx , x ,
                   NULL ) ;
   x -= 85 ;
   XtVaSetValues ( map_lat_text ,
                   XmNx , x ,
                   NULL ) ;
   x -= 55 ;
   XtVaSetValues ( map_lat_label ,
                   XmNx , x ,
                   NULL ) ;
                   
   /* Manage the widgets. */
   
   /* Done. */
}
