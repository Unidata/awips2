/*****************************************************************************
 *
 * Module: map_resource.c
 *
 * Description: contains routines to create the required resources.
 * Modifcation History: March 2006  Add mSetPDSize() to define the parameters
 *                                  used to draw point data icon in drawStation.c
 *
 *****************************************************************************/

/* include files */

#include "stdio.h"
#include "stdlib.h"
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <X11/cursorfont.h>
#include "map.h"
#include "map_defines.h"
#include "map_resource.h"
#include "strutil.h"

/* data types */

struct _color{
  char name[20];
  unsigned long value;
}map_color[525];

/* global variables */

Display *display = NULL ;
GC gc ;

Widget top_level_widget ;
XtAppContext app_context ;
int num_of_colors = 0 ;

Colormap colormap ;

XFontStruct *fonts[M_NUM_OF_FONTS];
static int font_type;

Cursor watch,normal,selection,group_select,help;

int line_style=LineSolid,cap_style=CapButt,join_style=JoinRound;

Screen *screen = NULL;
unsigned long foreground_pixel, background_pixel, overlay_pixel1, overlay_pixel2;
unsigned long overlay_plane_mask;

/*****************************************************************************
 *
 * Routine: _get_map_gc
 *
 * Description: This routine returns the graphics context of the 
 *              application.
 *
 ****************************************************************************/
GC _get_map_gc()
{
  return gc ;
}

/*****************************************************************************
 *
 * Routine: _get_map_display
 *
 * Description: this routine returns the display
 *
 ****************************************************************************/
Display *_get_map_display()
{
  return(display);
}

/*****************************************************************************
 *
 * Routine: _get_map_context
 *
 * Description: this routine returns the application context
 *
 ****************************************************************************/
XtAppContext _get_map_context ( )
{
   return ( app_context ) ;
}

/*****************************************************************************
 *
 * Routine: _get_top_level
 *
 * Description: this routine returns the application context
 *
 ****************************************************************************/
Widget _get_top_level ( )
{
   return ( top_level_widget ) ;
}

/*****************************************************************************
 *
 * Routine: get_color
 *
 * Description: this routine returns the color value for the given color name
 *
 * History: Updated this routine to handle the "no color" situation in a 
 *          better fashion. 
 *
 ****************************************************************************/

unsigned long _get_color(char *name)
{
  int i , ret ;
  XColor exact , color ;

  for ( i = 0 ; i < num_of_colors ; i++ )
  {

    if ( strncmp ( map_color [ i ].name , name , strlen ( name ) ) == 0 )
    {
      return ( map_color [ i ].value ) ;
    }

  }

  strcpy ( map_color[num_of_colors].name , name ) ;
  ret = XAllocNamedColor(display,colormap,map_color[num_of_colors].name,
			 &color,&exact);

  if ( ret == 0 )
  {
     printf( "Warning: Unable to allocate color %s.\n",
              map_color[num_of_colors].name ) ;
     map_color [ num_of_colors ].value = 0 ;
  }
  else
  {
     map_color [ num_of_colors ].value = color.pixel;
     num_of_colors++;
     return ( color.pixel ) ;
  }

  return ( 0 ) ;
}

/*****************************************************************************
 *
 * Routine: mSetCursor
 *
 * Description: this routine set the cursor for the map window
 *
 ****************************************************************************/

void mSetCursor(int cursor)
{
  switch(cursor){
  case M_NORMAL: 
    XDefineCursor(display,XtWindow(_get_map_shell()),normal);
    break;

  case M_WATCH: 
    XDefineCursor(display,XtWindow(_get_map_shell()),watch);
    break;

  case M_SELECT:
    XDefineCursor(display,XtWindow(_get_map_shell()),selection);
    break;

  case M_GROUP_SELECT: 
    XDefineCursor(display,XtWindow(_get_map_shell()),group_select);
    break;

  case M_HELP: 
    XDefineCursor(display,XtWindow(_get_map_shell()),help);
    break;

  default: 
    printf("Error: Invalid cursor requested.\n");
    break;

  }
}

/*****************************************************************************
 *
 * Routine: mSetFont
 *
 * Description: this routine sets the font type (not the font directly) to be drawn
 *
 ****************************************************************************/

void mSetFont(int type)
{
  if (type >= M_NUM_OF_FONTS)
  {
    printf("ERROR:Invalid font type (%d). There are %d font types\n",type,
	   M_NUM_OF_FONTS);
  }
  else
  {
  	font_type = type;
    XSetFont(display,gc,fonts[type]->fid);
  }
}

/*****************************************************************************
 *
 * Routine: mGetFont
 *
 * Description: this routine returns current font type (not really the font) to be drawn
 ****************************************************************************/
 
int mGetFont()
{ 
   return font_type;	
}

/*****************************************************************************
 *
 * Routine: mGetStringWidth
 *
 * Description: this routine tell how wide the string would be if drawn in the
 * currently selected font.
 *
 ****************************************************************************/
 
 int mGetStringWidth(char * string)
 {
 	
 	 int string_width = XTextWidth(fonts[font_type], string, strlen(string));
 	
 	 return string_width;
 }
 
 /*****************************************************************************
 *
 * Routine: mGetStringHeight
 *
 * Description: this routine tell how tall the string would be if drawn in the
 * currently selected font.
 *
 ****************************************************************************/
 
 int mGetStringHeight(char * string)
 {
 	
 	 int string_height = fonts[font_type]->ascent + fonts[font_type]->descent;
 	
 	 return string_height;
 }
 

/*****************************************************************************
 *
 * Routine: _get_fonts
 *
 * Description: this routine allocates fonts
 *
 ****************************************************************************/

void _get_fonts()
{

  fonts[M_MINISCULE] = XLoadQueryFont(display,"-adobe-helvetica-bold-r-normal--8-80-75-75-p-50-iso8859-1");

  fonts[M_TINY] = XLoadQueryFont(display,"-adobe-helvetica-bold-r-normal--10-100-75-75-p-60-iso8859-1");

  fonts[M_SMALL] = XLoadQueryFont(display,"-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1");

  fonts[M_MEDIUM] = XLoadQueryFont(display,"-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1");


  fonts[M_LARGE] = XLoadQueryFont(display,"-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1");

  fonts[M_HUGE] = XLoadQueryFont(display,"-adobe-helvetica-bold-r-normal--24-240-75-75-p-138-iso8859-1");

}

/*****************************************************************************
 *
 * Routine: _get_cursors
 *
 * Description: this routine allocates cursors 
 *
 ****************************************************************************/

void _get_cursors()
{
  normal = XCreateFontCursor(display,XC_arrow);
  watch = XCreateFontCursor(display,XC_watch);
  selection = XCreateFontCursor(display,XC_hand2);
  group_select = XCreateFontCursor(display,XC_pencil);
  help = XCreateFontCursor(display,XC_question_arrow);
}

/*****************************************************************************
 *
 * Routine: _create_gc
 *
 * Description: this routine creates a GC
 *
 ****************************************************************************/

void _create_gc ( Widget drawarea )
{
  gc = XCreateGC(display,RootWindowOfScreen(XtScreen(drawarea)),0,NULL);
}

/*****************************************************************************
 *
 * Routine: _get_map_resources
 *
 * Description: this routine creates the resources needed by the library
 *
 ****************************************************************************/

void _get_map_resources(Widget parent)
{
  /* Retrieve a pointer to the parent widget's display. */
  display = XtDisplay ( parent ) ;

  /* Retrieve the application context of the parent widget. */
  app_context = XtWidgetToApplicationContext ( parent ) ;

  /* Set the parent as the top level widget. */
  top_level_widget = parent ;

  /* create colormap */
  colormap = DefaultColormap(display,DefaultScreen(display));

  _get_fonts();
  _get_cursors();
}
/*****************************************************************************
 *
 * Routine: mSetLineWidth
 *
 * Description: this routine sets the width for the lines to be drawn.
 *
 ****************************************************************************/

void mSetLineWidth(int width)
{
  XSetLineAttributes(display,gc,width,line_style,cap_style,join_style);
}

/*****************************************************************************
 *
 * Routine: mSetColor
 *
 * Description: this routine sets the color for the item to be drawn.
 *
 ****************************************************************************/

void mSetColor ( char * color )
{
   _set_foreground ( color ) ;
}

/*****************************************************************************
 *
 * Routine: _set_foreground
 *
 * Description: this routine sets the color for the item to be drawn.
 *
 ****************************************************************************/

void _set_foreground(char *color)
{
  XSetForeground(display,gc,_get_color(color));
}

Screen *_get_screen()
{
  return(screen);
}

void mSetGC ( enum MapGCtype gc_type , char * color )
{
   int function ;
   unsigned long foreground_pixel ;
   Widget map_widget ;
   XGCValues gc_values ;

   map_widget = _get_map_widget ( 0 ) ;

   switch ( gc_type )
   {
      case M_DEFAULT_GC :

         XSetFunction ( display , gc , GXcopy ) ;
         mSetColor ( color ) ;
         break ;

      case M_XOR_GC :

         /*------------------------------------------------------------------*/
         /* Set the fg to the XOR of the fg and bg, so if it is              */
         /* XOR'ed with the bg, the result will be fg and vice-versa.        */
         /* This effectively achieves inverse video for the line.            */
         /*------------------------------------------------------------------*/

         XGetGCValues ( display , gc , GCBackground , & gc_values ) ;
         
         foreground_pixel = _get_color ( color ) ;
         foreground_pixel ^= gc_values.background ;
         function = GXxor ;
         XSetForeground ( display , gc , foreground_pixel ) ;
         XSetFunction ( display , gc , function ) ;
         break ;

      default :

         break ;
   }
}

/*****************************************************************************
 *
 * Routine: mSetPDSize
 *
 * Description: this routine sets point data size to be drawn
 *
 ****************************************************************************/

void mSetPDSize(int type)
{
  if (type >= M_NUM_OF_PDSIZES)
  {
     printf("ERROR:Invalid point data size type (%d). There are %d point data size types\n",type,
	   M_NUM_OF_PDSIZES);
  }
  else
  {
     if (type == M_S_VSMALL )     
        set_pdsize_flag = M_S_VSMALL;            
     else if (type == M_S_SMALL )     
        set_pdsize_flag = M_S_SMALL;             
     else if (type == M_S_MEDIUM )     
        set_pdsize_flag = M_S_MEDIUM;        
     else if (type == M_S_LARGE )     
        set_pdsize_flag = M_S_LARGE;        
     
     define_pdsize(set_pdsize_flag);     
        
  }
}

/*****************************************************************************
 *
 * Routine: define_pdsize()
 *
 * Description: define the parameters for the point data icon size
 *
 ****************************************************************************/
void define_pdsize(int set_pdsize_flag)
{
    if (set_pdsize_flag == M_S_VSMALL)
    {
        FCST_POINT_SIZE = 3;
	FCST_POINT_OFFSET_X = -2;
	FCST_POINT_OFFSET_Y = -7;
	RIVER_GAGE_SIZE = 4;
	GENERAL_STATION_SIZE = 3;
	RESERVOIR_WIDTH = 7; 
	RESERVOIR_OFFSET_X = -3;
	RESERVOIR_OFFSET_Y = 6;
	X_OTHER_OFFSET = -2;    
    	Y_OTHER_OFFSET = -1;
    }
    else if (set_pdsize_flag == M_S_SMALL)
    {
        FCST_POINT_SIZE = 4;
	FCST_POINT_OFFSET_X = -2;
	FCST_POINT_OFFSET_Y = -7;
	RIVER_GAGE_SIZE = 5;
	GENERAL_STATION_SIZE = 4;
	RESERVOIR_WIDTH = 9;
	RESERVOIR_OFFSET_X = -4;
	RESERVOIR_OFFSET_Y = 6;
	X_OTHER_OFFSET = -2;
	Y_OTHER_OFFSET = -1;
    }
    else if (set_pdsize_flag == M_S_MEDIUM)
    {
       
        FCST_POINT_SIZE = 5;
	FCST_POINT_OFFSET_X = -2.15;
	FCST_POINT_OFFSET_Y = -8;
	RIVER_GAGE_SIZE = 6;
	GENERAL_STATION_SIZE = 5;
	RESERVOIR_WIDTH = 11;
	RESERVOIR_OFFSET_X = -5;
	RESERVOIR_OFFSET_Y = 7;
	X_OTHER_OFFSET = -2.7;
	Y_OTHER_OFFSET = -1;
    }
    else if (set_pdsize_flag == M_S_LARGE)
    {
        FCST_POINT_SIZE = 6;
	FCST_POINT_OFFSET_X = -2.85;
	FCST_POINT_OFFSET_Y = -9;
	RIVER_GAGE_SIZE = 7;
	GENERAL_STATION_SIZE = 6;
	RESERVOIR_WIDTH = 13;
	RESERVOIR_OFFSET_X = -6;
	RESERVOIR_OFFSET_Y = 8;
	X_OTHER_OFFSET = -3;
	Y_OTHER_OFFSET = -1;
    }	

}


