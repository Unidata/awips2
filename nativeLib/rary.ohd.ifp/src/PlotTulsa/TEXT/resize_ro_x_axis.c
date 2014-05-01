/* File: resize_ro_x_axis.c
 *
 * Resizes events for Tulsa plot drawing areas.
 *
 * Creates a new image to proper scale and stores it in
 * the pixmap.
 *
 * Gets the width and height of the drawing area.
 *
 * Gets the foreground and background colors to make
 * the graphics context to draw into the pixmap.
 *
 * Sets the origin and end pixel values of the x and
 * y axes.
 *
 * Stores origin_y in plot_cb_struct part of data structure so
 * that it is available for the mousetracker functions.
 *
 * Clears the window and generates an Expose event for the
 * entire window.
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void resize_ro_x_axis(w, data, call_data)

  Widget                        w;             /* widget data structure */
  combined_struct                *data;        /* tables and plot data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                                  /* counters */
  Arg   wargs[10];                             /* window resource data structure array */
  Dimension     width, height;                 /* width and height of the drawing area */
  Dimension     pix_width;                     /* pixmap width */
  Pixel         foreground, background;        /* foreground, background colors */
  int           mask;                          /* mask to declare which fields are valid */
  XGCValues     gcv;                           /* graphics context data structure */
  XRectangle    *rectangles;                   /* rectangle data structure pointer */
  short         bar_width;

  /* printf("in resize_ro_x_axis\n"); */

/*
 * Resize events for Tulsa plot drawing areas.
 * Create new image to proper scale and store in pixmap.
 *
 * Get the width and height of the drawing area.
 * Also, get foreground and background colors to make
 *  graphics context to draw into pixmap.
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtSetArg(wargs[n], XmNforeground, &foreground); n++;
  XtSetArg(wargs[n], XmNbackground, &background); n++;
  XtGetValues(w, wargs, n);

  pix_width = data->plot->width_scale * width;

/* Set the origin and end pixel values of the x and y axes. */
  data->plot->origin_x = 0.0;
  data->plot->origin_y = 0.9*height;
  data->plot->end_x = pix_width;
  data->plot->end_y = 0.0;
/*
 * Store origin_y in plot_cb_struct part of data structure so
 *  it is available for the mousetracker functions - gfs, 11/12/91
 */
  data->plot->ro_origin_y = data->plot->origin_y;
/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->plot->pix[3])
     XFreePixmap(XtDisplay(w), data->plot->pix[3]);

  data->plot->pix[3] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     pix_width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->plot->pix[3] == 0)
    {
     printf("No space to create pixmap for ro_x axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->plot->gc[3] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->plot->gc[3] = XCreateGC(XtDisplay(w), data->plot->pix[3], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->plot->gc[3], background);
  XFillRectangle(XtDisplay(w), data->plot->pix[3], data->plot->gc[3],
		  0, 0, pix_width, height);

  XSetForeground(XtDisplay(w), data->plot->gc[3], foreground);

  bar_width = (short)((float)pix_width /
	      (float)data->plot->num_rr_pts * .75 /
	      (float)data->plot->num_rr_oper);
  if(bar_width < 1) bar_width = 1;

  draw_rectangles(w, data, RO, bar_width);

  gcv.foreground = get_pixel_by_name(w, "gray");
  XChangeGC(XtDisplay(data->plot->drawing_area_widget[3]),
	    data->plot->gc[3], GCForeground, &gcv);
  XFillRectangle(XtDisplay(w), data->plot->pix[3], data->plot->gc[3],
		 0, data->plot->origin_y, pix_width,
		 height - data->plot->origin_y);
/*
 * Temporarily - just make up a time series to plot

  rectangles = (XRectangle *)malloc(data->plot->max_points * sizeof(XRectangle));

  bar_width = (short)((float)pix_width/(float)data->plot->max_points/3.0);
  if(bar_width < 1) bar_width = 1;

  for(i=0; i<data->plot->max_points; i++)
     {
      rectangles[i].x = (short)((i * pix_width) /
				 (data->plot->max_points-1) - bar_width/2);
      rectangles[i].height = (short)(height/2
			      +
			      (sin((double)((float)i/(float)(data->plot->max_points-1) *
					    2*PI * 3))
			       *
			       sin((double)((float)i/(float)(data->plot->max_points-1) *
					    2*PI * 2))
			       *
			       sin((double)((float)i/(float)(data->plot->max_points-1) *
					    2*PI))
			       *
			       sin((double)((float)i/(float)(data->plot->max_points-1) *
					    PI))
			       * height/2)
			     );
      rectangles[i].y = height - rectangles[i].height;
      rectangles[i].width = bar_width;
     }
  XFillRectangles(XtDisplay(w), data->plot->pix[3], data->plot->gc[3],
		  rectangles, data->plot->max_points);

  free(rectangles);
*/

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_ro_x_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_ro_x_axis.c,v 1.1 1995/09/08 14:58:17 page Exp $";}
/*  ===================================================  */

}
