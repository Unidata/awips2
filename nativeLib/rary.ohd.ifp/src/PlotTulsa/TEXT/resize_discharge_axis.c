/* File: resize_discharge_axis.c
 *
 * Resizes events for Tulsa plot drawing areas.
 *
 * Creates new image to proper scale and stores it in the pixmap.
 *
 * Gets the width and height of the drawing area.
 *
 * Gets foreground and background colors to make graphics context
 * to draw into the pixmap.
 *
 * Sets new values for slider and increments on the scrollbar
 * based on new window height.
 *
 * Sets new value by scaling current value and centers the new
 * window view on center of scrollbar slider.
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void resize_discharge_axis(w, data, call_data)

  Widget                        w;
  combined_struct               *data;       /* combined data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                /* counters */
  Arg   wargs[10];           /* window resource data structure array */
  Dimension     width;       /* drawing area width */
  Dimension     height;      /* drawing area height */
  Dimension     pix_height;  /* plot height */
  int           current_scrollbar_maximum, current_slider_size;
  int           current_scrollbar_value, new_scrollbar_value;
  float         float_current_location, float_current_maximum;
  Pixel         foreground, background; /* foreground, background colors */
  unsigned int  line_width;  /* horizontal length of line */
  XSegment      ticks[11];   /* array of line segments used as tick marks in the graph */
  int           mask;        /* mask to declare which fields are valid */
  XGCValues     gcv;         /* graphics context data structure */
  int           num_ticks=11, length=13;
  int           x_offset;    /* x position from the label border */
  int           y_offset;    /* y position from the label border */
  int           direction;   /* direction text is drawn */
  int           ascent;      /* position above the baseline */
  int           descent;     /* position below the baseline */
  char          y_label[14]; /* y axis labels */
  XFontStruct   *label_font; /* label font pointer    */
  Font          font_id;
  XCharStruct   char_info;   /* width, left bearing, right bearing string
				information structure */
  int           pixel_val;   /* pixel value */
  int           vert_axis_max_scale_val; /* vertical axis maximum scale value */
  char          control_str[7];  /* controls the number of decimal places in the labels */
  float         range;           /* range of values to be displayed in the labels */

  /* printf("in resize_discharge_axis\n"); */

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

  pix_height = data->plot->height_scale * height;
  line_width = 4;

/* Set the origin and end pixel values of the discharge axis. */
  data->plot->origin_x = width - (0.5*line_width);
  data->plot->origin_y = pix_height;
  data->plot->end_x = data->plot->origin_x;
  data->plot->end_y = 0;

/*
 * Get current (before resize) values for scrollbar
 *   value, slider_size (height), and maximum_value (pix_height).
 */
  n=0;
  XtSetArg(wargs[n], XmNvalue, &current_scrollbar_value); n++;
  XtSetArg(wargs[n], XmNsliderSize, &current_slider_size); n++;
  XtSetArg(wargs[n], XmNmaximum, &current_scrollbar_maximum); n++;
  XtGetValues(data->plot->vertical_scrollbar_widget, wargs, n);

/*
 * Set new values for slider and increments on scrollbar
 *  based on new window height.
 * Set new value by scaling current value.
 *
 * Center new window view on center of scrollbar slider.
 *
 * Do division arithmetic with floats so we keep the fraction.
 */
  float_current_location = current_scrollbar_value + current_slider_size/2;
  float_current_maximum = current_scrollbar_maximum;

  new_scrollbar_value =
   ((float_current_location / float_current_maximum) * pix_height)
	- (height/2);

  if(new_scrollbar_value < 0) new_scrollbar_value = 0;
  if(new_scrollbar_value + height > pix_height)
     new_scrollbar_value = pix_height - height;

/*  Calculate the minimum and maximum data values for the new window size
    based on the new scrollbar value (in pixels) and height of the drawing
    area.
*/
  pixel_val = new_scrollbar_value + height;
  data->plot->min_discharge_disp = pixel_to_val(&pixel_val, data->plot->min_y,
					  data->plot->discharge_axis_max,
					  &data->plot->origin_y, &data->plot->end_y);

  pixel_val = new_scrollbar_value;
  data->plot->max_discharge_disp = pixel_to_val(&pixel_val, data->plot->min_y,
					  data->plot->discharge_axis_max,
					  &data->plot->origin_y, &data->plot->end_y);

  data->plot->v_slider_size = height;

  n=0;
  XtSetArg(wargs[n], XmNmaximum, pix_height); n++;
  XtSetArg(wargs[n], XmNsliderSize, height); n++;
  XtSetArg(wargs[n], XmNvalue, new_scrollbar_value); n++;
  XtSetArg(wargs[n], XmNincrement, height / 10); n++;
  XtSetArg(wargs[n], XmNpageIncrement, height / 4); n++;
  XtSetValues(data->plot->vertical_scrollbar_widget, wargs, n);

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);

/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->plot->pix[4])
     XFreePixmap(XtDisplay(w), data->plot->pix[4]);

  data->plot->pix[4] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, pix_height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->plot->pix[4] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
     exit(1);
    }

/*
 * Create graphics context if needed.
 */
  if(data->plot->gc[4] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->plot->gc[4] = XCreateGC(XtDisplay(w), data->plot->pix[4], mask, &gcv);
    }

/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->plot->gc[4], background);
  XFillRectangle(XtDisplay(w), data->plot->pix[4], data->plot->gc[4],
		  0, 0, width, pix_height);

  XSetForeground(XtDisplay(w), data->plot->gc[4], foreground);
  line_width = 4;
  XSetLineAttributes(XtDisplay(w), data->plot->gc[4],
		     line_width, 0, 0, 0);
  XDrawLine(XtDisplay(w), data->plot->pix[4], data->plot->gc[4],
	    data->plot->origin_x, data->plot->origin_y,
	    data->plot->end_x, data->plot->end_y);

  line_width = 1;
  XSetLineAttributes(XtDisplay(w), data->plot->gc[4],
		     line_width, 0, 0, 0);

 /* if (data->plot->h_q_plot_indicator != 1 && data->plot->h_q_plot_indicator != 2)
  { */
     for(i=0; i<num_ticks; i++)
     {
        ticks[i].x1 = data->plot->origin_x - 10;
        ticks[i].x2 = data->plot->origin_x;
        ticks[i].y1 = ticks[i].y2 = data->plot->origin_y +
   				    (i*(data->plot->end_y - data->plot->origin_y)
				    / (num_ticks-1));
     }
  XDrawSegments(XtDisplay(w), data->plot->pix[4], data->plot->gc[4],
		ticks, num_ticks);
  /*}*/
  /* Draw tick mark at bottom y axis. */
  XDrawLine(XtDisplay(w), data->plot->pix[4], data->plot->gc[4],
	    data->plot->origin_x-10, data->plot->origin_y-1,
	    data->plot->origin_x, data->plot->origin_y-1);
/*
 * Make sure line width is set to 1 after all this.
 */
  line_width = 1;
  XSetLineAttributes(XtDisplay(w), data->plot->gc[4],
		     line_width, 0, 0, 0);

/*  Label y axis:  set the font and get info on string size in pixels
		   determine adjustments to placement of labels
		   determine the maximum value for y axis
		   create and draw labels
*/
  /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
  XSetFont(XtDisplay(w), data->plot->gc[4], font_id);
  label_font = XQueryFont(XtDisplay(w), font_id);
  sprintf(y_label, "%13d", (int)(*data->plot->min_y));
  length = strlen(y_label);
  x_offset = (int)(XTextWidth(label_font, y_label, length));
  XTextExtents(label_font, y_label, length, &direction,
	       &ascent, &descent, &char_info);
  y_offset = (int)((ascent+descent)/4);

  /* get value of change_vert_axis_max_widget */
  n=0;
  XtSetArg(wargs[n], XmNvalue, &vert_axis_max_scale_val); n++;
  XtGetValues(data->plot->change_vert_axis_max_widget, wargs, n);

  /* decide how many decimal points to print based on the range of values */
  range = *data->plot->discharge_axis_max - *data->plot->min_y;
  memset(control_str, '\0', 7);
  if(range > 10.0)
     strncpy(control_str, "%13.0f", 6);
  else if(range > 1.0 && range <= 10.0)
     strncpy(control_str, "%13.1f", 6);
  else if(range > 0.0 && range <= 1.0)
     strncpy(control_str, "%13.2f", 6);
     
/*  only label discharge axis if there's either no rc shift mod in 
 *  effect (h_q_plot_indicator = 0) or if there is, it's a 
 *  discharge plot (h_q_plot_indicator = 1)
*/    
 if (data->plot->h_q_plot_indicator != 2)
 {
  for(i=0; i<num_ticks; i++)
  {
     sprintf(y_label, control_str, *data->plot->min_y +
				   i * data->plot->discharge_increment *
				   vert_axis_max_scale_val);

     /* Label placement, if lettering will go off bottom or top of drawing
	area, adjust placement, otherwise center it on the tick mark.
     */
     if(ticks[i].y1+(y_offset*4) > pix_height)
	XDrawString(XtDisplay(w), data->plot->pix[4], data->plot->gc[4],
		    ticks[i].x1-x_offset, ticks[i].y1-y_offset,
		    y_label, length);
     else if(ticks[i].y1-(y_offset*4) < data->plot->end_y)
	XDrawString(XtDisplay(w), data->plot->pix[4], data->plot->gc[4],
		    ticks[i].x1-x_offset, ticks[i].y1+y_offset*4,
		    y_label, length);
     else
	XDrawString(XtDisplay(w), data->plot->pix[4], data->plot->gc[4],
		    ticks[i].x1-x_offset, ticks[i].y1+y_offset,
		    y_label, length);
  } 
}


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_discharge_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_discharge_axis.c,v 1.5 2006/03/28 20:44:08 aivo Exp $";}
/*  ===================================================  */

}
