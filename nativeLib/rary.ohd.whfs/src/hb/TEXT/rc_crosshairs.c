/*
	File:		rc_crosshairs.c
	Date:		12/19/1994
	Author:		Chip Gobs
	
	Purpose:	Provide support for the Rating Curve DS.
	
*/

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include "rc_crosshairs.h"
#include "rateds.h"
#include "rate_cbs.h"
#include "rate_draw.h"
#include "Xtools.h"

int	rc_firstx, 
	rc_firsty;

/*
int	rc_get_pixel_name(Widget widget, char *name)
{
	Display     *dpy = XtDisplay(widget);
	int         scr  = DefaultScreen(dpy);
	Colormap    cmap = DefaultColormap(dpy,scr);
	XColor      color, ignore;

	if (XAllocNamedColor(dpy, cmap, name, &color, &ignore))
    		return(color.pixel);
  	else 
  	{
  		printf("warning: color not defined\n");
    		return(BlackPixel(dpy,scr));
  	}
}
*/

GC	rc_setup_crosshairs(Widget widget)
{
	Display		*display;
	XGCValues	vals;
	GC		gc;
	Arg		arg[1];
	int		ac;
	
	
	display = XtDisplay(widget);
	
	rc_ch_data.startx = 0;
	rc_ch_data.starty = 0;
	rc_ch_data.endx = 0;
	rc_ch_data.endy = 0;
	
	ac = 0;
	XtSetArg(arg[ac], XmNbackground, &vals.background); ac++;
	XtGetValues(widget, arg, ac);
	
	vals.function = GXxor;
	vals.foreground = GetNamedColor(widget, "white");
	vals.foreground = vals.foreground ^ vals.background;
	vals.line_style = LineSolid;
	
	gc = XtGetGC(widget, 
		GCForeground | GCBackground | GCFunction | GCLineStyle, 
		&vals);	
	return(gc);
}


void	rc_start_crosshairs(Widget widget, RcCrossHairs *rc_ch_data, XEvent *event)
{
	Dimension	width,
			height;
			
	
	width  = getWidgetWidth(widget);
	height = getWidgetHeight(widget);
	
	rc_firstx = event->xbutton.x;
	rc_firsty = event->xbutton.y;
			
	rc_ch_data->startx = event->xbutton.x;
	rc_ch_data->starty = event->xbutton.y;
	

		
	/*
		Check bounds for crosshairs, must be
		inside of the hydrograph work area.
	*/
	if (rc_ch_data->startx <= R_BORDER)
		rc_ch_data->startx = R_BORDER;
	if (rc_ch_data->startx >= (width - BORDER))
		rc_ch_data->startx = (width - (BORDER - 1));
	if (rc_ch_data->starty <= BORDER)
		rc_ch_data->starty = BORDER;
	if (rc_ch_data->starty >= height - BORDER)
		rc_ch_data->starty = height - BORDER;
	
	/*
		Draw the crosshairs.
	*/	
	XDrawLine(XtDisplay(widget), XtWindow(widget), rc_ch_data->gc,
		  rc_ch_data->startx, BORDER,
		  rc_ch_data->startx, height - BORDER);
	XDrawLine(XtDisplay(widget), XtWindow(widget), rc_ch_data->gc,
		  R_BORDER, rc_ch_data->starty,
		  width - BORDER, rc_ch_data->starty);
	return;
}


void	rc_track_crosshairs(Widget widget, RcCrossHairs *rc_ch_data, XEvent *event)
{
	Display		*display;
	Window		window;
	Dimension	width,
			height;	
	char 		buf[20];
	float		y_data_range;
	
	display = XtDisplay(widget);
	window  = XtWindow(widget);
	width   = getWidgetWidth(widget);
	height  = getWidgetHeight(widget);

	
	XSetLineAttributes(display, rc_ch_data->gc, 0, 0, 0, 0);
	XDrawLine(display, window, rc_ch_data->gc, 
		  rc_ch_data->startx, BORDER,
		  rc_ch_data->startx, height - BORDER);
	XDrawLine(display, window, rc_ch_data->gc,
		  R_BORDER, rc_ch_data->starty,
		  width - BORDER, rc_ch_data->starty);
		
	rc_ch_data->startx = event->xbutton.x;
	rc_ch_data->starty = event->xbutton.y;
	

	/*
		Check bounds for crosshairs, must be
		inside of the hydrograph work area.
	*/
	if (rc_ch_data->startx <= R_BORDER)
		rc_ch_data->startx = R_BORDER + 1;
	if (rc_ch_data->startx >= (width - BORDER))
		rc_ch_data->startx = width - BORDER;
	if (rc_ch_data->starty <= BORDER)
		rc_ch_data->starty = BORDER;
	if (rc_ch_data->starty >= height - BORDER)
		rc_ch_data->starty = height - (BORDER + 1);

	XDrawLine(display, window, rc_ch_data->gc, 
		  rc_ch_data->startx, BORDER,
		  rc_ch_data->startx, height - BORDER);
	XDrawLine(display, window, rc_ch_data->gc,
		  R_BORDER, rc_ch_data->starty,
		  width - BORDER, rc_ch_data->starty);
		  
	y_data_range = maxstage - minstage;
	
	sprintf(buf,"%6.1f",((((height - BORDER) - (float) rc_ch_data->starty) 
		/ (height - 2*BORDER)) * y_data_range) + minstage);
	
	SetLabel(rtstageInfo, buf);	
		
	sprintf(buf,"%4.1f",((((float) rc_ch_data->startx - R_BORDER) /
	        (width - 130)) * _max_flow) / 1000); 
	SetLabel(rtflowInfo, buf);
	
	return;
}


void	rc_stop_crosshairs(Widget widget, RcCrossHairs *rc_ch_data, XEvent *event)
{
	Display		*display;
	Window		window;
	Dimension	width,
			height;
	
	
	/*
		Get the window attributes.
	*/
	display = XtDisplay(widget);
	window  = XtWindow(widget);		
	width   = getWidgetWidth(widget);
	height  = getWidgetHeight(widget);
	
	
	/*
		Cleanup display window.
	*/
	
	SetLabel(rtstageInfo,"");
	SetLabel(rtflowInfo,"");
	
	XDrawLine(display, window, rc_ch_data->gc,
		  rc_ch_data->startx, BORDER,
		  rc_ch_data->startx, height - BORDER);
	XDrawLine(display, window, rc_ch_data->gc,
		  R_BORDER, rc_ch_data->starty,
		  width - BORDER, rc_ch_data->starty);	
	return;
}
