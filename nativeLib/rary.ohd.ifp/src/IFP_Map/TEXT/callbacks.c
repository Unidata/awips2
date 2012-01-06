/*=========================================================================*/
/*                    FILE PATH/NAME:   /home/lef/s3/stage3_callbacks.c    */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:                                      */
/*                                      choose_color()                     */
/*                                      do_zoom()                          */
/*                                      draw_circle()                      */
/*                                      draw_rectangle()                   */
/*                                      draw_polygon()                     */
/*                                      draw_line()                        */
/*                                                                         */
/*=========================================================================*/


#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "read_write_data.h"
#include "libXs.h"
#include "drawa.h"
#include "math.h"
#include <sys/stat.h>








/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/stage3_callbacks.c                       */
/*  FUNCTION NAME:   choose_color()                                        */
/*       FUNCTION:   callback that pops up widget with color preferences   */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) preferences (color) button

Functions called:
   display_colors


******************************************** BEGIN choose_color ************/

void choose_color(w, widget_struct, call_data)
   Widget               w;
   the_widget_struct    *widget_struct;
   caddr_t              *call_data;
{

 display_colors(widget_struct);

}






/********************************************************************************/
/*                                                                              */
/*  FUNCTION NAME:      do_zoom()                                               */
/*                                                                              */
/*       FUNCTION:      callback to zoom into the selected area of the          */
/*                      DrawingArea widget                                      */
/*                                                                              */
/*      Important:      The HRAP coordinate system is used to keep track        */
/*                      of the scaling and origin of the region drawn to        */
/*                      the DrawingArea widget; the upper left-hand corner      */
/*                      is the origin in terms of the DrawingArea widget,       */
/*                      with pixel coordinates (0,0); but in terms of the       */
/*                      HRAP coordinate system, it is:                          */
/*                                                                              */
/*                      x: prev_origin_x, where                                 */
/*                                                                              */
/*                                prev_origin_x = rad_data->origin.x;           */
/*                                                                              */
/*                      y: prev_origin_y + rad_data->maximum_rows, where        */
/*                                                                              */
/*                                prev_origin_y = rad_data->origin.y;           */
/*                                                                              */
/*                                                                              */
/*                      Initially, before any zooming:                          */
/*                                                                              */
/*                              rad_data->origin.x = XOR                        */
/*                              rad_data->origin.y = YOR                        */
/*                                                                              */
/*                      (the bottom left-hand corner of the DrawingArea         */
/*                       in HRAP bin units)                                     */
/*                                                                              */
/*                      and:                                                    */
/*                                                                              */
/*                              rad_data->maximum_columns = MAXX                */
/*                              rad_data->maximum_rows    = MAXY                */
/*                                                                              */
/*                      (the width & height of the DrawingArea in HRAP bins)    */
/*                                                                              */
/*                                                                              */
/********************************************************************************

Function type:
   void

Called by function:
   (callback) zoom button

Functions called:
   (callback) fill_pixmap
   (callback) copy_area
   (callback) show_states
   (callback) show_county
   (callback) show_basin_boundaries
   (callback) show_rivers
   (callback) show_radar_rings
   (callback) show_cities_and_towns
   get_pixel_by_name
   fill_pixmap

Static variables:
   zoom_on - integer;


******************************************** BEGIN do_zoom *****************/

void do_zoom(w, widget_struct, call_data)
	Widget               w;
	the_widget_struct    *widget_struct;
	caddr_t              *call_data;
{
	Widget          SWinHorzScrollBar;
	Widget          SWinVertScrollBar;

	int             HorzScrollBar_valueReturn;
	int             HorzScrollBar_sliderSizeReturn;
	int             HorzScrollBar_incrementReturn;
	int             HorzScrollBar_pageIncrementReturn;
	int             HorzScrollBar_maximum;

	int             VertScrollBar_valueReturn;
	int             VertScrollBar_sliderSizeReturn;
	int             VertScrollBar_incrementReturn;
	int             VertScrollBar_pageIncrementReturn;
	int             VertScrollBar_maximum;

	int             x_pixels_per_bin;
	int             y_pixels_per_bin;
	int             pixels_per_bin;
	int             pixels_per_bin_Y;
	int             x;
	int             y;
	int             prev_origin_x;
	int             prev_origin_y;
	int             delta_x;
	int             delta_y;

	point           center;                 /* (x,y) center of the zoom area in HRAP units...       */

	Dimension       width, height;          /* Width & Height of the DrawingArea widget             */
	Dimension       sw_width, sw_height;    /* Width & Height of the ScrolledWindow widget,         */
						/* which is the parent of the DrawingArea widget...     */





 /*-----------------------------------------------------------------------------*/
 /*     if no zoom window selected, do nothing                                  */
 /*-----------------------------------------------------------------------------*/

 if (rbdata.start_x == rbdata.last_x) return;

 /*-----------------------------------------------------------------------------*/
 /*     Save the previous origin in case we're zooming inside a zoom area;      */
 /*     if this is the 1st time zooming, x & y are XOR & YOR, respectively...   */
 /*-----------------------------------------------------------------------------*/
 prev_origin_x = rad_data->origin.x;
 prev_origin_y = rad_data->origin.y;


 /*-----------------------------------------------------------------------------*/
 /*     get size of pixels in original display                                  */
 /*-----------------------------------------------------------------------------*/

 XtVaGetValues(widget_struct->drawArea_SWindow,
	      XmNwidth,               &sw_width,
	      XmNheight,              &sw_height,
	      XmNhorizontalScrollBar, &SWinHorzScrollBar,
	      XmNverticalScrollBar,   &SWinVertScrollBar,
	      NULL);

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 XtVaGetValues(SWinHorzScrollBar, XmNmaximum, &HorzScrollBar_maximum, NULL);
 XmScrollBarGetValues(SWinHorzScrollBar,
		     &HorzScrollBar_valueReturn,
		     &HorzScrollBar_sliderSizeReturn,
		     &HorzScrollBar_incrementReturn,
		     &HorzScrollBar_pageIncrementReturn);

 XtVaGetValues(SWinVertScrollBar, XmNmaximum, &VertScrollBar_maximum, NULL);
 XmScrollBarGetValues(SWinVertScrollBar,
		     &VertScrollBar_valueReturn,
		     &VertScrollBar_sliderSizeReturn,
		     &VertScrollBar_incrementReturn,
		     &VertScrollBar_pageIncrementReturn);


 x_pixels_per_bin = (float)width/(float) rad_data->maximum_columns;
 y_pixels_per_bin = (float)height/(float) rad_data->maximum_rows;

 if (x_pixels_per_bin > y_pixels_per_bin) x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin) y_pixels_per_bin = x_pixels_per_bin;


 /*-----------------------------------------------------------------------------*/
 /*                                                                             */
 /*     set zoom origin                                                         */
 /*                                                                             */
 /*     (x,y) is the upper left-hand corner of the selected region...           */
 /*                                                                             */
 /*-----------------------------------------------------------------------------*/

 x = rbdata.last_x;
 if (rbdata.start_x < x) x = rbdata.start_x;
 y = rbdata.last_y;
 if (rbdata.start_y < y) y = rbdata.start_y;

 delta_x = abs(rbdata.last_x-rbdata.start_x);
 delta_y = abs(rbdata.last_y-rbdata.start_y);


 /*-----------------------------------------------------------------------------*/
 /*     The center of the selected area...                                      */
 /*-----------------------------------------------------------------------------*/

 /* In old Pixel coordinates..          */
 x = x + delta_x/2;
 y = y + delta_y/2;

 /* In HRAP coordinates...              */
 center.x = x/x_pixels_per_bin + prev_origin_x;
 center.y = prev_origin_y + rad_data->maximum_rows - y/y_pixels_per_bin;

 zoom_in(&center, widget_struct, rad_data);

}





/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/stage3_callbacks.c                       */
/*  FUNCTION NAME:   draw_circle()                                         */
/*       FUNCTION:   callback to draw circle area overlay (DOES NOTHING)   */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) area (circle) button

Functions called:
   none


******************************************** BEGIN draw_circle *************/

void draw_circle(w, widget_struct, call_data)
	Widget               w;
	the_widget_struct    *widget_struct;
	caddr_t              *call_data;
{

}



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/stage3_callbacks.c                       */
/*  FUNCTION NAME:   draw_rectangle()                                      */
/*       FUNCTION:   callback to draw rectangle area overlay (DOES NOTHING)*/
/***************************************************************************

Function type:
   void

Called by function:
   (callback) area (rectangle) button

Functions called:
   none

******************************************** BEGIN draw_rectangle **********/

void draw_rectangle(w, widget_struct, call_data)
	Widget               w;
	the_widget_struct    *widget_struct;
	caddr_t              *call_data;
{

}




/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/stage3_callbacks.c                       */
/*  FUNCTION NAME:   draw_polygon()                                        */
/*       FUNCTION:   callback to draw polygon area overlay (DOES NOTHING)  */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) area (polygon) button

Functions called:
   none

******************************************** BEGIN draw_polygon ************/

void draw_polygon(w, widget_struct, call_data)
	Widget               w;
	the_widget_struct    *widget_struct;
	caddr_t              *call_data;
{

}




/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/stage3_callbacks.c                       */
/*  FUNCTION NAME:   draw_line()                                           */
/*       FUNCTION:   callback to draw line area overlay (DOES NOTHING)     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) area (line) button

Functions called:
   none

******************************************** BEGIN draw_line ***************/

void draw_line(w, widget_struct, call_data)
	Widget               w;
	the_widget_struct    *widget_struct;
	caddr_t              *call_data;
{

}









/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/stage3_callbacks.c                       */
/*  FUNCTION NAME:   popdown_shell()                                       */
/*       FUNCTION:   callback to popdown shell                             */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) close button

Functions called:
   none

******************************************** BEGIN popdown_shell ***********/
/*
void popdown_shell(w, shell, call_data)
   Widget               w;
   Widget               shell;
   XmAnyCallbackStruct *call_data;
{

 XtPopdown(shell);

}

*/



/* **********************************************************************

	show_forecast_points()
		main meun item callback function;


   ********************************************************************** */

void show_forecast_points(widget_struct)
	the_widget_struct       *widget_struct;
{

	int             i, j, n, maxpts;
	int             x, y;
	int             xloc, yloc;
	int             mask = GCForeground;

	char            str[35];

	XPoint          points[4];
	Display         *dpy;
	Dimension       width, height;
	GC              gc;
	XGCValues       gcv;

	HRAP            hpto;

	draw_struct     *data;


 data = widget_struct->overlays;

 dpy = XtDisplay(widget_struct->main_canvas);

 /*     Determine dimension of display area...                          */
 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 /*--------------------------------------------------------------*/
 /*     if the forecast points are on, copy the base pixmap      */
 /*     without overlays onto active pixmap, add other           */
 /*     overlays and create an expose event to re-display        */
 /*--------------------------------------------------------------*/

/* --------------------------------------------------------------
 if (data->forecastPoints_on)
    {
    data->forecastPoints_on = FALSE;
    XCopyArea(dpy, data->pixbase, data->pix, data->gc[0], 0, 0, width, height, 0, 0);
    add_overlays(data);
    if (XtIsRealized(widget_struct->main_canvas))
		XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);

    return;
    }

 data->forecastPoints_on = TRUE;
   -------------------------------------------------------------- */

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;

 if (x > y) x = y;
 else if (y > x) y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(widget_struct->main_canvas, color_list[18]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 XSetForeground(dpy, gc, get_pixel_by_name(widget_struct->main_canvas,color_list[22]));

 /*--------------------------------------------------------------*/
 /* Display fcstpoints as small triangles at the actual location */
 /*--------------------------------------------------------------*/

 for (i = 0; i < NumForecastPoints; i++)
	{
	hpto = LatLongToHrap(forecastpoints[i].Lat, forecastpoints[i].Long);
	xloc = (hpto.x - data->origin.x) * x;
	yloc = (data->maximum_rows - (hpto.y - data->origin.y))*y;

	points[0].x = points[3].x = xloc;
	points[0].y = points[3].y = yloc - 5;
	points[1].x = xloc - 5;
	points[1].y = yloc + 5;
	points[2].x = xloc + 5;
	points[2].y = yloc + 5;

	XFillPolygon(dpy, data->pix, gc, points, 4, Convex, CoordModeOrigin);

	if(zoom_factor > 2.5)
		{
		memset(str, '\0', 35);
		strcpy(str, " ");
		strcat(str, forecastpoints[i].id);
		XDrawString(dpy, data->pix, gc, xloc+4, yloc, str, strlen(str));
		}

	}



 /*--------------------------------------------------------------*/
 /*     create expose event if display widget has already been   */
 /*     realized                                                 */
 /*--------------------------------------------------------------*/
 if (XtIsRealized(widget_struct->main_canvas))
		XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);


}




/* **********************************************************************

	show_tools()
		main meun item callback function;


   ********************************************************************** */

void show_tools(w, widget_struct, call_data)
	Widget               w;
	the_widget_struct    *widget_struct;
	XmAnyCallbackStruct  *call_data;
{

	XWindowAttributes       attributes;


 if(XtIsRealized(widget_struct->mappingStruct->tools_shell))
	{
	XGetWindowAttributes(XtDisplay(w), XtWindow(widget_struct->mappingStruct->tools_shell), &attributes);

	if(attributes.map_state == IsUnmapped) XtPopup(widget_struct->mappingStruct->tools_shell, XtGrabNone);
	else XtPopdown(widget_struct->mappingStruct->tools_shell);
	}
 else XtPopup(widget_struct->mappingStruct->tools_shell, XtGrabNone);

}




/* **********************************************************************

	do_Draw_to_scale()
		main meun item callback function;


   ********************************************************************** */

void do_Draw_to_scale(w, widget_struct, call_data)
	Widget               w;
	the_widget_struct    *widget_struct;
	XmAnyCallbackStruct  *call_data;
{


}



/* **********************************************************************

	void show_forecastGroup_boundaries()


   ********************************************************************** */

void show_forecastGroup_boundaries(widget_struct)
	the_widget_struct       *widget_struct;
{

	int                  i, j, n, maxpts;
	XPoint               *points;
	GC                   gc;
	Display              *dpy;
	Dimension            width, height;
	Arg                  wargs[5];
	int                  x, y;
	int                  mask = GCForeground;
	XGCValues            gcv;
	int                  numbasin;
	overlay_struct       **basin;

	draw_struct          *data;


 data = widget_struct->overlays;

 dpy = XtDisplay(widget_struct->main_canvas);

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 /*--------------------------------------------------------------*/
 /*     if basins are on, copy the base pixmap without overlays  */
 /*     onto active pixmap, add other overlays and create        */
 /*     an expose event to re-display                            */
 /*--------------------------------------------------------------*/

/* --------------------------------------------------------------
 if (data->fcstGroup_bounds_on)
	{
	data->fcstGroup_bounds_on = FALSE;
	XCopyArea(dpy, data->pixbase, data->pix, data->gc[0], 0, 0, width, height, 0, 0);
	add_overlays(data);

	if (XtIsRealized(widget_struct->main_canvas))
		XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);

	return;
	}

 data->fcstGroup_bounds_on = TRUE;
   -------------------------------------------------------------- */

 /*--------------------------------------------------------------*/
 /*     determine whether to display map basins or forecast      */
 /*     group basins depending on size of display area           */
 /*--------------------------------------------------------------*/

 basin = fgbasin;

 /*--------------------------------------------------------------*/
 /*     determine maximum number of points to display & allocate */
 /*     space                                                    */
 /*--------------------------------------------------------------*/

 maxpts = 0;
 numbasin = numfg;

 for(i = 0; i < numbasin; i++)
    if (basin[i]->npts > maxpts) maxpts = basin[i]->npts;


 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;

 if (x > y) x = y;
 else if (y > x) y = x;


 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(widget_struct->main_canvas, color_list[23]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 XSetLineAttributes(XtDisplay(widget_struct->main_canvas),
		    gc,
		    (unsigned int) 2,   /* width      */
		    LineSolid,          /* line_style */
		    0,               /* cap_style  */
		    0);              /* join_stype */



 /*--------------------------------------------------------------*/
 /*     display basin boundaries                                 */
 /*--------------------------------------------------------------*/

 for (i = 0; i < numbasin; i++)
	{
	for (j = 0; j < basin[i]->npts; j++)
		{
		points[j].x = (basin[i]->hrap[j].x - data->origin.x) * x;
		points[j].y = (data->maximum_rows - (basin[i]->hrap[j].y - data->origin.y))*y;
		}
	XDrawLines(dpy, data->pix, gc, points, basin[i]->npts, CoordModeOrigin);
	}

 /*--------------------------------------------------------------*/
 /*     create expose event if display widget has already been   */
 /*     realized                                                 */
 /*--------------------------------------------------------------*/

 if (XtIsRealized(widget_struct->main_canvas))
		XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);

}






/* **********************************************************************

	change_thresholds()
		main meun item callback function;


   ********************************************************************** */

void change_thresholds(w, widget_struct, call_data)
	Widget               w;
	the_widget_struct    *widget_struct;
	XmAnyCallbackStruct  *call_data;
{


}




void show_current_mods(Widget w, caddr_t *client_data, XmAnyCallbackStruct *call_data)
{

	printf("Inside 'show_current_mods()'...\n");


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/callbacks.c,v $";
 static char rcs_id2[] = "$Id: callbacks.c,v 1.2 2006/04/07 13:29:26 aivo Exp $";}
/*  ===================================================  */

}


