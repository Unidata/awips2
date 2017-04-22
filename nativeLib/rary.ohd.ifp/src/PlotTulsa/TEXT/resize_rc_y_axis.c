/* File: resize_rc_y_axis.c
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
 * Sets the origin and end pixel values for the y axis.
 *
 * Get current values for scrollbar value, slider_size (height),
 * and maximum_value (pix_height)
 *
 * Sets the new values for the slider and increments on the scrollbar
 * based on new window height.
 *
 */

#include "rating_curve.h"

void resize_rc_y_axis(w, data, call_data)

   Widget                        w;            /* widget data structure */
   rc_struct                     *data;        /* tables and plot data structure pointer */
   XmDrawingAreaCallbackStruct   *call_data;
{
   Arg           wargs[10];                    /* window resource data structure array */
   Dimension     width, height;                /* width and height of the drawing area */
   Dimension     pix_height;                   /* graph height */
   int           current_scrollbar_maximum, current_slider_size;
   int           current_scrollbar_value, new_scrollbar_value;
   float         float_current_location, float_current_maximum; /* window values */
   Pixel         foreground, background;       /* foreground, background colors */
   XGCValues     gcv;                          /* graphics context data structure */
   int           i, j, n;                      /* counters */
   int           mask;                         /* mask to declare which fields are valid */
   unsigned int  line_width;
   XSegment      ticks[11];                    /* tick marks on the y axis */
   int           num_ticks=11;                 /* number of y axis tick marks */
   int           length=13;                    /* y axis label length */
   int           x_offset;                     /* x position from the label border */
   int           y_offset;                     /* y position from the label border */
   int           direction;                    /* direction text is drawn */
   int           ascent;                       /* position above the baseline */
   int           descent;                      /* position below the baseline */
   char          y_label[14];                  /* y label character array */
   XFontStruct   *label_font;                  /* label font resource structure */
   Font          font_id;                      /* font resource id */
   XCharStruct   char_info;                    /* width, left bearing, right bearing string
						  information structure */
   int           pixel_val;                    /* pixel value */

   /*printf("in resize_rc_y_axis\n");*/

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

  pix_height = data->height_scale * height;
  line_width = 4;

/* Set the origin and end pixel values for the y axis.
*/
  data->origin_x = width - line_width/2;
  data->end_x = data->origin_x;
  data->origin_y = pix_height;
 /* data->end_y = 0.05*data->form_height * pix_height;*/
  data->end_y = 0;

/*
 * Get current (before resize) values for scrollbar
 *   value, slider_size (height), and maximum_value (pix_height).
 */
  n=0;
  XtSetArg(wargs[n], XmNvalue, &current_scrollbar_value); n++;
  XtSetArg(wargs[n], XmNsliderSize, &current_slider_size); n++;
  XtSetArg(wargs[n], XmNmaximum, &current_scrollbar_maximum); n++;
  XtGetValues(data->rc_vert_scrollbar_widget, wargs, n);
/*
 * Set new values for slider and increments on scrollbar
 *  based on new window height.
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
  >>>  ((float_current_location / float_current_maximum) * pix_height)
  >>>       - (height/2);
  >>>
  >>> Replace the above with code to keep lower part of window.
  */
  float_current_location = current_scrollbar_value +
			   current_slider_size;
  float_current_maximum = current_scrollbar_maximum;

  new_scrollbar_value =
   ((float_current_location / float_current_maximum) * pix_height) -
	height;

  if(new_scrollbar_value < 0) new_scrollbar_value = 0;
  if(new_scrollbar_value + height > pix_height)
     new_scrollbar_value = pix_height - height;

/*  Calculate the minimum and maximum data values for the new window size
    based on the new scrollbar value (in pixels) and height of the drawing
    area.
*/
  pixel_val = new_scrollbar_value + height;
  data->min_stage_disp = pixel_to_val(&pixel_val, &data->min_stg,
				      &data->stg_axis_max,
				      &data->origin_y, &data->end_y);

  pixel_val = new_scrollbar_value;
  data->max_stage_disp = pixel_to_val(&pixel_val, &data->min_stg,
				      &data->stg_axis_max,
				      &data->origin_y, &data->end_y);

  data->rc_v_slider_size = height;

  n=0;
  XtSetArg(wargs[n], XmNmaximum, pix_height); n++;
  XtSetArg(wargs[n], XmNsliderSize, height); n++;
  XtSetArg(wargs[n], XmNvalue, new_scrollbar_value); n++;
  XtSetArg(wargs[n], XmNincrement, height / 10); n++;
  XtSetArg(wargs[n], XmNpageIncrement, height / 4); n++;
  XtSetValues(data->rc_vert_scrollbar_widget, wargs, n);

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);

/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->pix[1])
     XFreePixmap(XtDisplay(w), data->pix[1]);

  data->pix[1] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, pix_height,
			     DefaultDepthOfScreen(XtScreen(w))
			     );
  if(data->pix[1] == 0)
    {
     printf("No space to create pixmap for stage axis drawing area\n");
     exit(1);
    }

/*
 * Create graphics context if needed.
 */
  if(data->gc[1] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->gc[1] = XCreateGC(XtDisplay(w), data->pix[1], mask, &gcv);
    }

/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->gc[1], background);
  XFillRectangle(XtDisplay(w), data->pix[1], data->gc[1],
		  0, 0, width, pix_height);

  XSetForeground(XtDisplay(w), data->gc[1], foreground);

  line_width = 4;
  XSetLineAttributes(XtDisplay(w), data->gc[1],
		     line_width, 0, 0, 0);
  XDrawLine(XtDisplay(w), data->pix[1], data->gc[1],
	    data->origin_x, data->origin_y,
	    data->end_x, data->end_y);

  line_width = 1;
  XSetLineAttributes(XtDisplay(w), data->gc[1],
		     line_width, 0, 0, 0);

  for(i=0; i<num_ticks; i++)
     {
      ticks[i].x1 = data->origin_x - 10;
      ticks[i].x2 = data->origin_x;
      ticks[i].y1 = ticks[i].y2 = data->origin_y +
				  (i*(data->end_y - data->origin_y)
				  / (num_ticks-1));
     }
  XDrawSegments(XtDisplay(w), data->pix[1], data->gc[1],
		ticks, num_ticks);
  /* Draw tick mark at bottom y axis. */
  XDrawLine(XtDisplay(w), data->pix[1], data->gc[1],
	    data->origin_x-10, data->origin_y-1,
	    data->origin_x, data->origin_y-1);
/*
 * Make sure line width is set to 1 after all this.
 */
  line_width = 1;
  XSetLineAttributes(XtDisplay(w), data->gc[1],
		     line_width, 0, 0, 0);

/*  Label y axis:  set the font and get info on string size in pixels
		   determine adjustments to placement of labels
		   determine the maximum value for y axis
		   create and draw labels
*/
 /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
  XSetFont(XtDisplay(w), data->gc[1], font_id);
  label_font = XQueryFont(XtDisplay(w), font_id);
  sprintf(y_label, "%10.2f", data->min_stg);
  length = strlen(y_label);
  x_offset = (int)(XTextWidth(label_font, y_label, length));
  XTextExtents(label_font, y_label, length, &direction,
	       &ascent, &descent, &char_info);
  y_offset = (int)((ascent+descent)/4);
  for(i=0; i<num_ticks; i++)
  {
     sprintf(y_label, "%10.2f", data->min_stg + i * data->rc_stg_increment);

     /* Label placement, if lettering will go off bottom or top of drawing
	area, adjust placement, otherwise center it on the tick mark.
     */
     if(ticks[i].y1+(y_offset*4) > pix_height)
	XDrawString(XtDisplay(w), data->pix[1], data->gc[1],
		    ticks[i].x1-x_offset, ticks[i].y1-y_offset,
		    y_label, length);
     else if(ticks[i].y1-(y_offset*4) < 0 /*data->end_stg*/)
	XDrawString(XtDisplay(w), data->pix[1], data->gc[1],
		    ticks[i].x1-x_offset, ticks[i].y1+y_offset*4,
		    y_label, length);
     else
	XDrawString(XtDisplay(w), data->pix[1], data->gc[1],
		    ticks[i].x1-x_offset, ticks[i].y1+y_offset,
		    y_label, length);
  }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_rc_y_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_rc_y_axis.c,v 1.3 2006/03/28 20:44:18 aivo Exp $";}
/*  ===================================================  */

}
