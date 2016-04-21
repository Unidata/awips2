/* File: resize_rc_x_axis.c
 *
 * Resizes events for mods plot drawing areas.
 *
 * Creates a new image to proper scale and stores it in
 * the pixmap.
 *
 * Gets the width and height of the drawing area.
 *
 * Gets the foreground and background colors to make
 * the graphics context to draw into the pixmap.
 *
 * Sets the origin and end pixel values of the x axes and
 * gets the current values for thescrollbar.
 *
 * Sets new values for slider and increments on scrollbar
 * based on new window width.
 *
 * Calculates the minimum and maximum data values for the new window size
 * based on the new scrollbar value (in pixels) and width of the drawing
 * area.
 *
 */

#include "rating_curve.h"
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"

void resize_rc_x_axis(w, data, call_data)

   Widget                        w;          /* widget data structure */
   rc_struct                     *data;
   XmDrawingAreaCallbackStruct   *call_data;
{
   Arg           wargs[10];    /* window resource data structure array */
   Dimension     width;        /* width of drawing area */
   Dimension     height;       /* height of drawing area */
   Dimension     pix_width;    /* width of drawing area in pixels */
   int           current_scrollbar_maximum, current_slider_size;
   int           current_scrollbar_value, new_scrollbar_value;
   float         float_current_location, float_current_maximum;
   Pixel         foreground, background;    /* window foreground, background colors */
   XGCValues     gcv;                       /* graphics context data structure */
   int           n;                         /* counter */
   int           mask;                      /* foreground, backround graphic contexts mask */
   int           pixel_val;                 /* pixel value */

   /*printf("in resize_rc_x_axis\n");*/

/*
 * Resize events for mods plot drawing areas.
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

  pix_width = data->width_scale * width;

/* Set the origin and end pixel values of the x axis */
  data->origin_x = 0;
  /*data->end_x = 0.8*data->form_width * data->width_scale;*/
  data->end_x = pix_width;

/*
 * Get current (before resize) values for scrollbar
 *   value, slider_size (width), and maximum_value (pix_width).
 */
  n=0;
  XtSetArg(wargs[n], XmNvalue, &current_scrollbar_value); n++;
  XtSetArg(wargs[n], XmNsliderSize, &current_slider_size); n++;
  XtSetArg(wargs[n], XmNmaximum, &current_scrollbar_maximum); n++;
  XtGetValues(data->rc_horiz_scrollbar_widget, wargs, n);
/*
 * Set new values for slider and increments on scrollbar
 *  based on new window width.
 * Set new value by scaling current value.
 *
 * Center new window view on center of scrollbar slider.
 * >>> Change for rating curve plot - keep the lower left
 * >>>  portion of the old window view.
 * >>>   gfs, 11/14/91
 *
 * Do division arithmetic with floats so we keep the fraction.
 */
 /*
  >>> float_current_location = current_scrollbar_value +
  >>>                          current_slider_size/2;
  >>> float_current_maximum = current_scrollbar_maximum;
  >>>
  >>> new_scrollbar_value =
  >>>  ((float_current_location / float_current_maximum) * pix_width)
  >>>       - width/2;
  >>>
  >>> Replace the above with code to keep left part of window.
  */
  float_current_location = current_scrollbar_value;
  float_current_maximum = current_scrollbar_maximum;

  new_scrollbar_value =
   (float_current_location / float_current_maximum) * pix_width;

  if(new_scrollbar_value < 0) new_scrollbar_value = 0;
  if(new_scrollbar_value + width > pix_width)
		   new_scrollbar_value = pix_width - width;

/*  Calculate the minimum and maximum data values for the new window size
    based on the new scrollbar value (in pixels) and width of the drawing
    area.
*/
  pixel_val = new_scrollbar_value;
  data->min_q_disp = pixel_to_val(&pixel_val, &data->min_q,
				  &data->max_q,
				  &data->origin_x, &data->end_x);

  pixel_val = new_scrollbar_value + width;
  data->max_q_disp = pixel_to_val(&pixel_val, &data->min_q,
				  &data->max_q,
				  &data->origin_x, &data->end_x);

  data->rc_h_slider_size = width;

  n=0;
  XtSetArg(wargs[n], XmNvalue, new_scrollbar_value); n++;
  XtSetArg(wargs[n], XmNsliderSize, width); n++;
  XtSetArg(wargs[n], XmNmaximum, pix_width); n++;
  XtSetArg(wargs[n], XmNincrement, width / 10); n++;
  XtSetArg(wargs[n], XmNpageIncrement, width / 4); n++;
  XtSetValues(data->rc_horiz_scrollbar_widget, wargs, n);

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->pix[2])
     XFreePixmap(XtDisplay(w), data->pix[2]);

  data->pix[2] = XCreatePixmap(XtDisplay(w),
			       DefaultRootWindow(XtDisplay(w)),
			       pix_width, height,
			       DefaultDepthOfScreen(XtScreen(w))
			      );
  if(data->pix[2] == 0)
    {
     printf("No space to create pixmap for x_axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->gc[2] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->gc[2] = XCreateGC(XtDisplay(w), data->pix[2], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->gc[2], background);
  XFillRectangle(XtDisplay(w), data->pix[2], data->gc[2],
		  0, 0, pix_width, height);

  XSetForeground(XtDisplay(w), data->gc[2], foreground);

  rc_draw_x_axis(w, data);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_rc_x_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_rc_x_axis.c,v 1.1 1995/09/08 14:58:12 page Exp $";}
/*  ===================================================  */

}
