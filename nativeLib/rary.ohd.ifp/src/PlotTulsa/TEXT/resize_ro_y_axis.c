/* File: resize_ro_y_axis.c
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
 * Sets the origin and end pixel values of the ro y axis.
 *
 * Clears the window and generates an Expose event for the
 * entire window.
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void resize_ro_y_axis(w, data, call_data)

  Widget                        w;           /* widget data structure */
  combined_struct                *data;      /* tables and plot data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                                /* counters */
  Arg   wargs[10];                           /* window resource data structure array */
  Dimension     width, height;               /* width and height of the drawing area */
  Dimension     pix_width;                   /* width of pixmap */
  Pixel         foreground, background;      /* foreground, background colors */
  unsigned int  line_width;
  XSegment      ticks[6];                    /* tick marks on the y axis */
  int           mask;                        /* mask to declare which fields are valid */
  XGCValues     gcv;                         /* graphics context data structure */
  int           num_ticks=6;                 /* number of y axis tick marks */
  int           length=10;                   /* y axis label length */
  char          y_label[10];                 /* y axis label array */
  int           x_offset;                    /* x position from the label border */
  int           y_offset;                    /* y position from the label border */
  int           direction;                   /* direction text is drawn */
  int           ascent;                      /* position above the baseline */
  int           descent;                     /* position below the baseline */
  XFontStruct   *label_font;                 /* label font resource structure */
  Font          font_id;                     /* font resource id */
  XCharStruct   char_info;                   /* width, left bearing, right bearing string
						information structure */

  /* printf("in resize_ro_y_axis\n"); */

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
  line_width = 4;

/* Set the origin and end pixel values of the ro y axis. */
  data->plot->origin_x = width - (0.5*line_width);
  data->plot->origin_y = 0.9*height;
  data->plot->end_x = data->plot->origin_x;
  data->plot->end_y = 0.0;

/* Maximum value displayed by the ro y axis (px_ro_y_axis) is set as a
   function of the max of the px or ro data.  This is set
   in the resize_px_y_axis function.
*/

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->plot->pix[2])
     XFreePixmap(XtDisplay(w), data->plot->pix[2]);

  data->plot->pix[2] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->plot->pix[2] == 0)
    {
     printf("No space to create pixmap for ro_y axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->plot->gc[2] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->plot->gc[2] = XCreateGC(XtDisplay(w), data->plot->pix[2], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->plot->gc[2], background);
  XFillRectangle(XtDisplay(w), data->plot->pix[2], data->plot->gc[2],
		  0, 0, width, height);

  XSetForeground(XtDisplay(w), data->plot->gc[2], foreground);
  XSetLineAttributes(XtDisplay(w), data->plot->gc[2],
		     line_width, 0, 0, 0);
  XDrawLine(XtDisplay(w), data->plot->pix[2], data->plot->gc[2],
	    data->plot->origin_x, data->plot->origin_y,
	    data->plot->end_x, data->plot->end_y);

  line_width = 1;
  XSetLineAttributes(XtDisplay(w), data->plot->gc[2],
		     line_width, 0, 0, 0);

  for(i=0; i<6; i++)
     {
      ticks[i].x1 = width - 10;
      ticks[i].x2 = width;
      ticks[i].y1 = ticks[i].y2 = (data->plot->origin_y -
				  (data->plot->origin_y - data->plot->end_y)
				  * i / (num_ticks-1));
     }
  XDrawSegments(XtDisplay(w), data->plot->pix[2], data->plot->gc[2],
		ticks, 6);
/*
 * Make sure line width is set to 1 after all this.
 */
  line_width = 1;
  XSetLineAttributes(XtDisplay(w), data->plot->gc[2],
		     line_width, 0, 0, 0);
/************************************************************************/

/*  Label y axis:  set the font and get info on string size in pixels
		   determine adjustments to placement of labels
		   determine the maximum value for y axis
		   create and draw labels
*/
  gcv.foreground = get_pixel_by_name(w, "navy");
  XChangeGC(XtDisplay(w), data->plot->gc[2], GCForeground, &gcv);
  /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
  XSetFont(XtDisplay(w), data->plot->gc[2], font_id);
  label_font = XQueryFont(XtDisplay(w), font_id);
  sprintf(y_label, "%10.2f",
	  ((data->plot->px_ro_y_axis_max - data->plot->ro_min) * 0/
		(num_ticks - 1)));
  length = strlen(y_label);
  x_offset = (int)(XTextWidth(label_font, y_label, length));
  XTextExtents(label_font, y_label, length, &direction,
	       &ascent, &descent, &char_info);
  y_offset = (int)((ascent+descent)/4);
  for(i=0; i<num_ticks; i+=2)
  {
     sprintf(y_label, "%10.2f",
	     ((data->plot->px_ro_y_axis_max - data->plot->ro_min) * i/
		   (num_ticks - 1)));
     XDrawString(XtDisplay(w), data->plot->pix[2], data->plot->gc[2],
		 ticks[i].x1-x_offset, ticks[i].y1+y_offset,
		 y_label, length);
  }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_ro_y_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_ro_y_axis.c,v 1.3 2006/03/28 20:44:21 aivo Exp $";}
/*  ===================================================  */

}
