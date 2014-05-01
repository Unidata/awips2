/* File: resize_mp_y_axis.c
 *
 * Handles resize events for TSMods plot drawing areas.
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
#include "mods_plot.h"

void resize_mp_y_axis(w, data, call_data)

   Widget                        w;             /* Widget data structure                */
   mods_plot_struct              *data;         /* Mods plot data structure pointer     */
   XmDrawingAreaCallbackStruct   *call_data;    /* XmDrawingArea call back structure pointer    */
{
   Arg          wargs[10];      /* Window resource data structure array */
   Dimension    width;          /* Window width         */
   Dimension    height;         /* Window height        */
   Pixel        foreground;     /* window foreground color              */
   Pixel        background;     /* window background color              */
   XGCValues    gcv;            /* Graphics context data structures     */
   int          i, j, n;        /* Counters             */
   int          mask;           /* Foreground/backround graphic contexts mask   */
   unsigned int line_width;     /* Value used to define the x positions in
				   drawing the x axis   */
   XSegment     ticks[11];      /* Tick marks on the y axis             */
   int          num_ticks=11;   /* Number of y axis tick marks          */
   int          length=13;      /* Y axis label length                  */
   int          x_offset;       /* X position from the label border     */
   int          y_offset;       /* Y position from the label border     */
   int          direction;      /* Direction text is drawn              */
   int          ascent;         /* Position above the baseline          */
   int          descent;        /* Position below the baseline          */
   char         y_label[14];    /* Y label character array              */
   XFontStruct  *label_font;    /* Font label structure pointer         */
   Font         font_id;        /* Font resource id                     */
   XCharStruct  char_info;      /* Width, left bearing, right bearing string
				   information structure */
   int          mp_max_vert_scale;  /* Maximum vertical scale           */

   /*printf("in resize_mp_y_axis\n");*/

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

  line_width = 4;

   for (i=0; i<data->num_ts_sel; i++)
      for (j=0; j<data->num_pts; j++)
	 if(data->ts_array[i][j] > data->max_y)
	    data->max_y = data->ts_array[i][j];

/* Set the origin and end pixel values for the y axis.
   For the y axis, if the mod is RRICHNG set origin_y at top of graph
   so bars start at the top.
*/
  data->origin_x = width - line_width/2;
  data->end_x = data->origin_x;
  if(data->mod_type_sw == RRICHNG)
  {
     data->origin_y = 0.05*data->form_height;
     data->end_y = height;
  }
  else /* UH or ROCHNG */
  {
     data->origin_y = height;
     data->end_y = 0.05*data->form_height;
  }

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
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			     );
  if(data->pix[1] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
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
		  0, 0, width, height);

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
  sprintf(y_label, "%10.2f", data->min_y);
  length = strlen(y_label);
  x_offset = (int)(XTextWidth(label_font, y_label, length));
  XTextExtents(label_font, y_label, length, &direction,
	       &ascent, &descent, &char_info);
  y_offset = (int)((ascent+descent)/4);
  mp_max_vert_scale = (int)(data->y_axis_max / data->default_y_axis_max);

  for(i=0; i<num_ticks; i++)
  {
     sprintf(y_label, "%10.2f", data->min_y +
				(i * data->mp_y_increment * mp_max_vert_scale));

     /* Label placement, if lettering will go off bottom or top of drawing
	area, adjust placement, otherwise center it on the tick mark.
     */
     if(ticks[i].y1+(y_offset*4) > height)
	XDrawString(XtDisplay(w), data->pix[1], data->gc[1],
		    ticks[i].x1-x_offset, ticks[i].y1-y_offset,
		    y_label, length);
     else if(ticks[i].y1-(y_offset*4) < 0 /*data->end_y*/)
	XDrawString(XtDisplay(w), data->pix[1], data->gc[1],
		    ticks[i].x1-x_offset, ticks[i].y1+y_offset*4,
		    y_label, length);
     else
	XDrawString(XtDisplay(w), data->pix[1], data->gc[1],
		    ticks[i].x1-x_offset, ticks[i].y1+y_offset,
		    y_label, length);
  }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/resize_mp_y_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_mp_y_axis.c,v 1.3 2006/04/07 14:34:55 aivo Exp $";}
/*  ===================================================  */

}
