/* ************************************************************************************************

	tool.c

	Purpose:        provide tools support for the IFP mapping functions in the form of a
			floating toolbox with pushbutton widgets

	Coded by:       Tom Adams
	Affiliation:    NOAA/NWS/Office of Hydrology/HRL
	Date:           09/10/92

	Major changes:


   ************************************************************************************************ */



#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "drawa.h"
#include "libXs.h"
#include "help.h"


/* Bitmap Files...      */
#include "arrow_tool.h"
#include "circle_tool.h"
#include "locator_tool.h"
#include "marquee_tool.h"
#include "query_tool.h"
#include "scale_tool.h"
#include "zoomIn_tool.h"
#include "zoomOut_tool.h"

#include "arrow_tool_invert.h"
#include "circle_tool_invert.h"
#include "locator_tool_invert.h"
#include "marquee_tool_invert.h"
#include "query_tool_invert.h"
#include "scale_tool_invert.h"
#include "zoomIn_tool_invert.h"
#include "zoomOut_tool_invert.h"


void    arrow_tool_selected();
void    marquee_tool_selected();
void    locator_tool_selected();
void    query_tool_selected();
void    circle_tool_selected();
void    scale_tool_selected();
void    zoom_in_tool_selected();
void    zoom_out_tool_selected();


void    invert_widget();
void    create_Tools_pixmaps();
void    set_tool_inactive();


static int              Previous_Tool;
static Pixmap           pixmap[16];
static Cursor           cursor;

enum  which_tool_active {
		           ARROW_OFF = 0 ,     ARROW_ON,
				   CIRCLE_OFF,     CIRCLE_ON,
				   LOCATOR_OFF,    LOCATOR_ON,
				   MARQUEE_OFF,    MARQUEE_ON,
				   QUERY_OFF,      QUERY_ON,
				   SCALE_OFF,      SCALE_ON,
				   ZOOM_IN_OFF,    ZOOM_IN_ON,
				   ZOOM_OUT_OFF,   ZOOM_OUT_ON};




Widget  create_toolbox(parent, widget_struct)
	Widget                  parent;
	the_widget_struct       *widget_struct;
{

	Widget          shell;
	Widget          rc;
	Widget          arrow;
	Widget          marquee;
	Widget          locator;
	Widget          query;
	Widget          circle;
	Widget          scale;
	Widget          zoomIn;
	Widget          zoomOut;

	int             n;
	Arg             wargs[3];



 n = 0;
 XtSetArg(wargs[0], XmNtitle, "Tools");
 shell = XtCreatePopupShell("tools_shell", transientShellWidgetClass, parent, wargs, n);

 rc = XtVaCreateManagedWidget("tools_rc", xmRowColumnWidgetClass, shell,
					 NULL);

 Previous_Tool = ARROW_ON;
 create_Tools_pixmaps(rc);


 /*---------------------------------------------*/
 /*     arrow_tool                              */
 /*---------------------------------------------*/
 arrow = XtVaCreateManagedWidget("arrow_tool", xmPushButtonWidgetClass, rc,
					 XmNlabelType, XmPIXMAP,
					 XmNlabelPixmap, pixmap[ARROW_ON],
					 NULL);
 widget_struct->mappingStruct->arrow_tool = arrow;
 XtAddCallback(arrow, XmNactivateCallback, arrow_tool_selected, widget_struct);
 XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);

 /*---------------------------------------------*/
 /*     marquee_tool                            */
 /*---------------------------------------------*/
 marquee =  XtVaCreateManagedWidget("marquee_tool", xmPushButtonWidgetClass, rc,
					 XmNlabelType, XmPIXMAP,
					 XmNlabelPixmap, pixmap[MARQUEE_OFF],
					 NULL);
 widget_struct->mappingStruct->marquee_tool = marquee;
 XtAddCallback(marquee, XmNactivateCallback, marquee_tool_selected, widget_struct);

 /*---------------------------------------------*/
 /*     locator_tool                            */
 /*---------------------------------------------*/
 locator =  XtVaCreateManagedWidget("locator_tool", xmPushButtonWidgetClass, rc,
					 XmNlabelType, XmPIXMAP,
					 XmNlabelPixmap, pixmap[LOCATOR_OFF],
					 NULL);
 widget_struct->mappingStruct->locator_tool = locator;
 XtAddCallback(locator, XmNactivateCallback, locator_tool_selected, widget_struct);

 /*---------------------------------------------*/
 /*     query_tool                              */
 /*---------------------------------------------*/
 query =    XtVaCreateManagedWidget("query_tool", xmPushButtonWidgetClass, rc,
					 XmNlabelType, XmPIXMAP,
					 XmNlabelPixmap, pixmap[QUERY_OFF],
					 NULL);
 widget_struct->mappingStruct->query_tool = query;
 XtAddCallback(query, XmNactivateCallback, query_tool_selected, widget_struct);

 /*---------------------------------------------*/
 /*     circle_tool                             */
 /*---------------------------------------------*/
 circle =   XtVaCreateManagedWidget("circle_tool", xmPushButtonWidgetClass, rc,
					 XmNlabelType, XmPIXMAP,
					 XmNlabelPixmap, pixmap[CIRCLE_OFF],
					 NULL);
 widget_struct->mappingStruct->circle_tool = circle;
 XtAddCallback(circle, XmNactivateCallback, circle_tool_selected, widget_struct);

 /*---------------------------------------------*/
 /*     scale_tool                              */
 /*---------------------------------------------*/
 scale =    XtVaCreateManagedWidget("scale_tool", xmPushButtonWidgetClass, rc,
					 XmNlabelType, XmPIXMAP,
					 XmNlabelPixmap, pixmap[SCALE_OFF],
					 NULL);
 widget_struct->mappingStruct->scale_tool = scale;
 XtAddCallback(scale, XmNactivateCallback, scale_tool_selected, widget_struct);

 /*---------------------------------------------*/
 /*     zoomOut_tool                            */
 /*---------------------------------------------*/
 zoomOut =  XtVaCreateManagedWidget("zoomOut_tool", xmPushButtonWidgetClass, rc,
					 XmNlabelType, XmPIXMAP,
					 XmNlabelPixmap, pixmap[ZOOM_OUT_OFF],
					 NULL);
 widget_struct->mappingStruct->zoomOut_tool = zoomOut;
 widget_struct->zoom = (ZoomStruct *) NULL;
 XtAddCallback(zoomOut, XmNactivateCallback, zoom_out_tool_selected, widget_struct);

 /*---------------------------------------------*/
 /*     zoomIn_tool                             */
 /*---------------------------------------------*/
 zoomIn =   XtVaCreateManagedWidget("zoomIn_tool", xmPushButtonWidgetClass, rc,
					 XmNlabelType, XmPIXMAP,
					 XmNlabelPixmap, pixmap[ZOOM_IN_OFF],
					 NULL);
 widget_struct->mappingStruct->zoomIn_tool = zoomIn;
 XtAddCallback(zoomIn, XmNactivateCallback, zoom_in_tool_selected, widget_struct);
 XtAddCallback(zoomIn, XmNactivateCallback, do_zoom, widget_struct);

 widget_struct->CircleToolData = (circle_rubber_band_data *) malloc(sizeof(circle_rubber_band_data));
 widget_struct->ScaleToolData = (rubber_line_data *) malloc(sizeof(rubber_line_data));

 widget_struct->CircleToolData->gc =
			IFP_Map_xs_create_xor_gc(widget_struct->main_canvas, "yellow", LineSolid);
 widget_struct->ScaleToolData->gc  =
			IFP_Map_xs_create_xor_gc(widget_struct->main_canvas, "yellow", LineSolid);

 Zoom_In_Enabled = FALSE;

 return(shell);

}





/* **********************************************************************

	arrow_tool_selected()



   ********************************************************************** */

void arrow_tool_selected(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{


/* ------------------------------------------------------------------
 if(Previous_Tool == ARROW_ON)
	{
	XtVaSetValues(w, XmNlabelPixmap, pixmap[ARROW_OFF], NULL);
	Previous_Tool = ARROW_OFF;
	}
 else   {
   ------------------------------------------------------------------ */

 if(Previous_Tool != ARROW_ON)
	{
	if(Previous_Tool != ARROW_OFF) set_tool_inactive(widget_struct);
	XtVaSetValues(w, XmNlabelPixmap, pixmap[ARROW_ON], NULL);
	Previous_Tool = ARROW_ON;

	if(widget_struct->tree_shell == NULL)
	       XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
	else   XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);
	}

}




/* **********************************************************************

	marquee_tool_selected()



   ********************************************************************** */

void marquee_tool_selected(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{


 if(Previous_Tool == MARQUEE_ON)
	{
	XtVaSetValues(w, XmNlabelPixmap, pixmap[MARQUEE_OFF], NULL);
	XtVaSetValues(widget_struct->mappingStruct->arrow_tool, XmNlabelPixmap, pixmap[ARROW_ON], NULL);
	Previous_Tool = ARROW_ON;

	XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask,   FALSE, IFP_Map_start_rubber_band, &rbdata);
	XtRemoveEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, IFP_Map_track_rubber_band, &rbdata);
	XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, IFP_Map_end_rubber_band, widget_struct);

	if(widget_struct->tree_shell == NULL)
	       XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
	else   XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);

	}
 else   {
	if(Previous_Tool != MARQUEE_OFF) set_tool_inactive(widget_struct);
	XtVaSetValues(w, XmNlabelPixmap, pixmap[MARQUEE_ON], NULL);
	Previous_Tool = MARQUEE_ON;

	XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask,   FALSE, IFP_Map_start_rubber_band, &rbdata);
	XtAddEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, IFP_Map_track_rubber_band, &rbdata);
	XtAddEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, IFP_Map_end_rubber_band, widget_struct);

	}


}




/* **********************************************************************

	locator_tool_selected()



   ********************************************************************** */

void locator_tool_selected(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{

	XSetWindowAttributes    attrs;
	Display                 *display;



 display = XtDisplay(w);


 if(Previous_Tool == LOCATOR_ON)
	{       /* The tool is on so, turn it off...    */
	XtVaSetValues(w, XmNlabelPixmap, pixmap[LOCATOR_OFF], NULL);
	XtVaSetValues(widget_struct->mappingStruct->arrow_tool, XmNlabelPixmap, pixmap[ARROW_ON], NULL);
	Previous_Tool = ARROW_ON;

	if(widget_struct->tree_shell == NULL)
	       XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
	else   XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);

	XtRemoveEventHandler(widget_struct->main_canvas, ButtonMotionMask, FALSE, track_locator, widget_struct);
	XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask,  FALSE, show_location, widget_struct);
	XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask,  FALSE, clear_location, widget_struct);

	attrs.cursor = None;
	XChangeWindowAttributes(display, XtWindow(widget_struct->main_canvas), CWCursor, &attrs);
	XFreeCursor(display, cursor);
	}
 else   {       /* The tool is off so, turn it on...    */
	if(Previous_Tool != LOCATOR_OFF) set_tool_inactive(widget_struct);
	XtVaSetValues(w, XmNlabelPixmap, pixmap[LOCATOR_ON], NULL);
	Previous_Tool = LOCATOR_ON;

	XtAddEventHandler(widget_struct->main_canvas, ButtonMotionMask, FALSE, track_locator, widget_struct);
	XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask,  FALSE, show_location, widget_struct);
	XtAddEventHandler(widget_struct->main_canvas, ButtonReleaseMask,  FALSE, clear_location, widget_struct);

	cursor = XCreateFontCursor(display, XC_tcross);
	attrs.cursor = cursor;
	XChangeWindowAttributes(display, XtWindow(widget_struct->main_canvas), CWCursor, &attrs);

	}


}




/* **********************************************************************

	query_tool_selected()



   ********************************************************************** */

void query_tool_selected(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{

	XSetWindowAttributes    attrs;
	Display                 *display;
	Font                    srcFont;
	XColor                  foreground;
	XColor                  background;



 display = XtDisplay(w);


 if(Previous_Tool == QUERY_ON)
	{
	XtVaSetValues(w, XmNlabelPixmap, pixmap[QUERY_OFF], NULL);
	XtVaSetValues(widget_struct->mappingStruct->arrow_tool, XmNlabelPixmap, pixmap[ARROW_ON], NULL);
	Previous_Tool = ARROW_ON;

	if(widget_struct->tree_shell == NULL)
	       XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
	else   XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);

	XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, locate, widget_struct);
	XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);

	attrs.cursor = None;
	XChangeWindowAttributes(display, XtWindow(widget_struct->main_canvas), CWCursor, &attrs);
	XFreeCursor(display, cursor);
	}
 else   {
	if(Previous_Tool != QUERY_OFF) set_tool_inactive(widget_struct);
	XtVaSetValues(w, XmNlabelPixmap, pixmap[QUERY_ON], NULL);
	Previous_Tool = QUERY_ON;

	XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, locate, widget_struct);

	background.red   = 0;
	background.green = 0;
	background.blue  = 0;

	foreground.red   = 65535;
	foreground.green = 65535;
	foreground.blue  = 65535;

	srcFont = XLoadFont(display, "-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1");
	cursor = XCreateGlyphCursor(display, srcFont, srcFont, '?', '?', &foreground, &background);
	attrs.cursor = cursor;
	XChangeWindowAttributes(display, XtWindow(widget_struct->main_canvas), CWCursor, &attrs);
	}


}




/* **********************************************************************

	circle_tool_selected()



   ********************************************************************** */

void circle_tool_selected(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{


 if(Previous_Tool == CIRCLE_ON)
	{
	XtVaSetValues(w, XmNlabelPixmap, pixmap[CIRCLE_OFF], NULL);
	XtVaSetValues(widget_struct->mappingStruct->arrow_tool, XmNlabelPixmap, pixmap[ARROW_ON], NULL);
	Previous_Tool = ARROW_ON;

	XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask,   FALSE, start_CircleTool_rb, widget_struct);
	XtRemoveEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, track_CircleTool_rb, widget_struct);
	XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, end_CircleTool_rb,   widget_struct);

	if(widget_struct->tree_shell == NULL)
	       XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
	else   XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);
	}
 else   {
	if(Previous_Tool != CIRCLE_OFF) set_tool_inactive(widget_struct);
	XtVaSetValues(w, XmNlabelPixmap, pixmap[CIRCLE_ON], NULL);
	Previous_Tool = CIRCLE_ON;

	XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask,   FALSE, start_CircleTool_rb, widget_struct);
	XtAddEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, track_CircleTool_rb, widget_struct);
	XtAddEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, end_CircleTool_rb,   widget_struct);

	}


}




/* **********************************************************************

	scale_tool_selected()



   ********************************************************************** */

void scale_tool_selected(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{


 if(Previous_Tool == SCALE_ON)
	{
	XtVaSetValues(w, XmNlabelPixmap, pixmap[SCALE_OFF], NULL);
	XtVaSetValues(widget_struct->mappingStruct->arrow_tool, XmNlabelPixmap, pixmap[ARROW_ON], NULL);
	Previous_Tool = ARROW_ON;

	XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask,   FALSE, start_ScaleTool_rb, widget_struct);
	XtRemoveEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, track_ScaleTool_rb, widget_struct);
	XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, end_ScaleTool_rb,   widget_struct);

	if(widget_struct->tree_shell == NULL)
	       XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
	else   XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);
	}
 else   {
	if(Previous_Tool != SCALE_OFF) set_tool_inactive(widget_struct);
	XtVaSetValues(w, XmNlabelPixmap, pixmap[SCALE_ON], NULL);
	Previous_Tool = SCALE_ON;

	XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask,   FALSE, start_ScaleTool_rb, widget_struct);
	XtAddEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, track_ScaleTool_rb, widget_struct);
	XtAddEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, end_ScaleTool_rb,   widget_struct);

	}


}




/* **********************************************************************

	zoom_out_tool_selected()



   ********************************************************************** */

void zoom_out_tool_selected(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{

 /* puts("Inside 'zoom_out_tool_selected()'..."); */

 if(widget_struct->zoom == (ZoomStruct *) NULL)
	{
	XBell(XtDisplay(w), 100);
	return;
	}

 zoom_out(widget_struct);

 if(widget_struct->tree_shell == NULL)
	XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
 else   XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);

}




/* **********************************************************************

	zoom_in_tool_selected()



   ********************************************************************** */

void zoom_in_tool_selected(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{


 if(!Zoom_In_Enabled)
	{
	XBell(XtDisplay(w), 100);
	return;
	}

 /* Set the Marquee Tool to inactive...                                 */
 set_tool_inactive(widget_struct);

 /* Set the Arrow Tool to active & add appropriate Event Handlers...    */
 XtVaSetValues(widget_struct->mappingStruct->arrow_tool, XmNlabelPixmap, pixmap[ARROW_ON], NULL);
 Previous_Tool = ARROW_ON;

 if(widget_struct->tree_shell == NULL)
	XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
 else   XtAddEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);

 Zoom_In_Enabled = FALSE;

}




/* **********************************************************************

	()



   **********************************************************************

void (w, widget_struct, call_data)
	Widget                  w;
	MappingWidgetStruct     *widget_struct;
	XmAnyCallbackStruct     *call_data;
{


}

*/


/* **********************************************************************

	create_Tools_pixmaps()


   ********************************************************************** */

void create_Tools_pixmaps(w)
	Widget  w;
{

	Display         *display;
	Window          root;
	int             screen, background;



 display = XtDisplay(w);
 screen  = DefaultScreen(display);
 root    = DefaultRootWindow(display);


 XtVaGetValues(w, XtNbackground, &background, NULL);


 pixmap[ARROW_OFF] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			arrow_tool_bits,
			arrow_tool_width,
			arrow_tool_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[ARROW_ON] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			arrow_tool_invert_bits,
			arrow_tool_invert_width,
			arrow_tool_invert_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[MARQUEE_OFF] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			marquee_tool_bits,
			marquee_tool_width,
			marquee_tool_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[MARQUEE_ON] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			marquee_tool_invert_bits,
			marquee_tool_invert_width,
			marquee_tool_invert_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);

 pixmap[LOCATOR_OFF] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			locator_tool_bits,
			locator_tool_width,
			locator_tool_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[LOCATOR_ON] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			locator_tool_invert_bits,
			locator_tool_invert_width,
			locator_tool_invert_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);

 pixmap[QUERY_OFF] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			query_tool_bits,
			query_tool_width,
			query_tool_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[QUERY_ON] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			query_tool_invert_bits,
			query_tool_invert_width,
			query_tool_invert_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);

 pixmap[CIRCLE_OFF] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			circle_tool_bits,
			circle_tool_width,
			circle_tool_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[CIRCLE_ON] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			circle_tool_invert_bits,
			circle_tool_invert_width,
			circle_tool_invert_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);

 pixmap[SCALE_OFF] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			scale_tool_bits,
			scale_tool_width,
			scale_tool_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[SCALE_ON] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			scale_tool_invert_bits,
			scale_tool_invert_width,
			scale_tool_invert_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);

 pixmap[ZOOM_OUT_OFF] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			zoomOut_tool_bits,
			zoomOut_tool_width,
			zoomOut_tool_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[ZOOM_OUT_ON] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			zoomOut_tool_invert_bits,
			zoomOut_tool_invert_width,
			zoomOut_tool_invert_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);

 pixmap[ZOOM_IN_OFF] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			zoomIn_tool_bits,
			zoomIn_tool_width,
			zoomIn_tool_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


 pixmap[ZOOM_IN_ON] = XCreatePixmapFromBitmapData
			(
			display,
			root,
			zoomIn_tool_invert_bits,
			zoomIn_tool_invert_width,
			zoomIn_tool_invert_height,
			BlackPixel(display, screen),
			background,
			DefaultDepth(display, screen)
			);


}



/* **********************************************************************

	set_tool_inactive()


   ********************************************************************** */

void set_tool_inactive(widget_struct)
	the_widget_struct       *widget_struct;
{

	Display                 *display;
	XSetWindowAttributes    attrs;




 display = XtDisplay(widget_struct->main_canvas);


 switch(Previous_Tool)
	{
	case ARROW_ON:
		XtVaSetValues(widget_struct->mappingStruct->arrow_tool, XmNlabelPixmap, pixmap[ARROW_OFF], NULL);

	if(widget_struct->tree_shell == NULL)
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_ForecastGroup, widget_struct);
	else    XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, select_basin, widget_struct);

		break;

	case CIRCLE_ON:
		XtVaSetValues(widget_struct->mappingStruct->circle_tool, XmNlabelPixmap, pixmap[CIRCLE_OFF], NULL);

		XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE,
				     start_CircleTool_rb, widget_struct);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonMotionMask, FALSE,
				     track_CircleTool_rb, widget_struct);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE,
				     end_CircleTool_rb, widget_struct);

		break;

	case LOCATOR_ON:
		XtVaSetValues(widget_struct->mappingStruct->locator_tool, XmNlabelPixmap, pixmap[LOCATOR_OFF], NULL);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, track_locator, widget_struct);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask,  FALSE, show_location, widget_struct);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask,  FALSE, clear_location, widget_struct);

		attrs.cursor = None;
		XChangeWindowAttributes(display, XtWindow(widget_struct->main_canvas), CWCursor, &attrs);
		XFreeCursor(display, cursor);
		break;

	case MARQUEE_ON:
		XtVaSetValues(widget_struct->mappingStruct->marquee_tool, XmNlabelPixmap, pixmap[MARQUEE_OFF], NULL);

		XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask,   FALSE, IFP_Map_start_rubber_band, &rbdata);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, IFP_Map_track_rubber_band, &rbdata);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, IFP_Map_end_rubber_band, widget_struct);

		break;

	case QUERY_ON:
		XtVaSetValues(widget_struct->mappingStruct->query_tool, XmNlabelPixmap, pixmap[QUERY_OFF], NULL);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask, FALSE, locate, widget_struct);

		attrs.cursor = None;
		XChangeWindowAttributes(display, XtWindow(widget_struct->main_canvas), CWCursor, &attrs);
		XFreeCursor(display, cursor);
		break;

	case SCALE_ON:
		XtVaSetValues(widget_struct->mappingStruct->scale_tool, XmNlabelPixmap, pixmap[SCALE_OFF], NULL);

		XtRemoveEventHandler(widget_struct->main_canvas, ButtonPressMask,   FALSE, start_ScaleTool_rb, widget_struct);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonMotionMask,  FALSE, track_ScaleTool_rb, widget_struct);
		XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, end_ScaleTool_rb,   widget_struct);

		break;

	case ZOOM_IN_ON:
		XtVaSetValues(widget_struct->mappingStruct->zoomIn_tool, XmNlabelPixmap, pixmap[ZOOM_IN_OFF], NULL);
		break;

	case ZOOM_OUT_ON:
		XtVaSetValues(widget_struct->mappingStruct->zoomOut_tool, XmNlabelPixmap, pixmap[ZOOM_OUT_OFF], NULL);
		break;

	default:
		break;
	}




/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/tools.c,v $";
 static char rcs_id2[] = "$Id: tools.c,v 1.2 2006/04/07 13:30:44 aivo Exp $";}
/*  ===================================================  */

}


