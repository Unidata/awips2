/* File: resize_px_x_axis.c
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
 * Sets the origin and end pixel values of the x and y axes.
 *
 * Stores origin_y in plot_cb_struct part of data structure so
 * that it is available for the mousetracker functions, also
 * stores the height in the plot_cb_struct for mousetracker.
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void resize_px_x_axis(w, data, call_data)

  Widget                        w;           /* widget data structure */
  combined_struct               *data;       /* tables and plot data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                                /* counters */
  Arg   wargs[10];                           /* window resource data structure array */
  Dimension     width, height;               /* width and height of the drawing area */
  Dimension     pix_width;                   /* pixmap width */
  int           current_scrollbar_maximum, current_slider_size;
  int           current_scrollbar_value, new_scrollbar_value;
  float         float_current_location, float_current_maximum;
  Pixel         foreground, background;      /* foreground, background colors */
  int           mask;                        /* mask to declare which fields are valid */
  XGCValues     gcv;                         /* graphics context data structure */
  XRectangle    *rectangles;                 /* rectangle data structure pointer */
  short         bar_width;
  int           pixel_val;                   /* pixel value */
  int           origin_x;                    /* x axis origin */
  int           end_x;                       /* end x axis value */

 /* printf("in resize_px_x_axis\n"); */

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

/* Set the origin and end pixel values of the x axes. */
  origin_x = 0.0;
  end_x = pix_width;

/* Set the origin and end pixel values of the x and y axes. */
  data->plot->origin_x = 0.0;
  data->plot->origin_y = .1*height;  /*0.0;*/
  data->plot->end_x = pix_width;
  data->plot->end_y = height;
/*
 * Store origin_y in plot_cb_struct part of data structure so
 *  it is available for the mousetracker functions - gfs, 11/12/91
 * Also store height in plot_cb_struct for mousetracker - dp, 5/14/92
 */
  data->plot->px_origin_y = data->plot->origin_y;
  data->plot->px_height = height;
/*
 * Get current (before resize) values for scrollbar
 *   value, slider_size (width), and maximum_value (pix_width).
 */
 /***
  n=0;
  XtSetArg(wargs[n], XmNvalue, &current_scrollbar_value); n++;
  XtSetArg(wargs[n], XmNsliderSize, &current_slider_size); n++;
  XtSetArg(wargs[n], XmNmaximum, &current_scrollbar_maximum); n++;
  XtGetValues(data->plot->horiz_scrollbar_widget, wargs, n);
  
  ***/
/*
 * Set new values for slider and increments on scrollbar
 *  based on new window width.
 * Set new value by scaling current value.
 *
 * Center new window view on center of scrollbar slider.
 *
 * Do division arithmetic with floats so we keep the fraction.
 */
/*****
  float_current_location = current_scrollbar_value + current_slider_size/2;
  float_current_maximum = current_scrollbar_maximum;

  new_scrollbar_value =
   ((float_current_location / float_current_maximum) * pix_width)
	- width/2;

  if(new_scrollbar_value < 0) new_scrollbar_value = 0;
  if(new_scrollbar_value + width > pix_width)
		   new_scrollbar_value = pix_width - width;
*******/

/*  Calculate the minimum and maximum data values for the new window size
    based on the new scrollbar value (in pixels) and width of the drawing
    area.
*/
/**********
  pixel_val = new_scrollbar_value;
  data->plot->min_time_disp = pixel_to_val(&pixel_val, data->plot->min_x,
				      data->plot->max_x,
				      &origin_x, &end_x);

  pixel_val = new_scrollbar_value + width;
  data->plot->max_time_disp = pixel_to_val(&pixel_val, data->plot->min_x,
				      data->plot->max_x,
				      &origin_x, &end_x);
*************/

  data->plot->h_slider_size = width;

  n=0;
  /*XtSetArg(wargs[n], XmNvalue, new_scrollbar_value); n++;*/
  XtSetArg(wargs[n], XmNsliderSize, width); n++;
  XtSetArg(wargs[n], XmNmaximum, pix_width); n++;
  XtSetArg(wargs[n], XmNincrement, width / 10); n++;
  XtSetArg(wargs[n], XmNpageIncrement, width / 4); n++;
  XtSetValues(data->plot->horiz_scrollbar_widget, wargs, n);

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->plot->pix[1])
     XFreePixmap(XtDisplay(w), data->plot->pix[1]);

  data->plot->pix[1] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     pix_width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->plot->pix[1] == 0)
    {
     printf("No space to create pixmap for px_x axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->plot->gc[1] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->plot->gc[1] = XCreateGC(XtDisplay(w), data->plot->pix[1], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->plot->gc[1], background);
  XFillRectangle(XtDisplay(w), data->plot->pix[1], data->plot->gc[1],
		  0, 0, pix_width, height);

  XSetForeground(XtDisplay(w), data->plot->gc[1], foreground);

  bar_width = (short)((float)pix_width /
	      (float)data->plot->num_rr_pts * .75 /
	      (float)data->plot->num_rr_oper);
  if(bar_width < 1) bar_width = 1;

/*  printf("bar_width = %d\n", bar_width);  */
  draw_rectangles(w, data, PX, bar_width);

  gcv.foreground = get_pixel_by_name(w, "gray");
  XChangeGC(XtDisplay(data->plot->drawing_area_widget[1]),
		      data->plot->gc[1], GCForeground, &gcv);
  XFillRectangle(XtDisplay(w), data->plot->pix[1], data->plot->gc[1],
		 0, 0, pix_width, data->plot->origin_y);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/PlotTulsa/RCS/resize_px_x_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_px_x_axis.c,v 1.4 2007/05/16 16:43:20 aivo Exp $";}
/*  ===================================================  */

}
