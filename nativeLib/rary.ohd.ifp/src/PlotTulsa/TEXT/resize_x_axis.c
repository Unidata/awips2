/* File: resize_x_axis.c
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
 * Sets the origin and end pixel values of the x axes and
 *
 * Clears the window and generates an Expose event for the
 * entire window.
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void resize_x_axis(w, data, call_data)

  Widget                        w;           /* widget data structure */
  combined_struct               *data;
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                                /* counters */
  Arg   wargs[10];                           /* window resource data structure array */
  Dimension     width, height;               /* width and height of drawing area */
  Dimension     pix_width;                   /* width of pixmap */
  Pixel         foreground, background;      /* window foreground, background colors */
  int           mask;                        /* foreground, backround graphic contexts mask */
  XGCValues     gcv;                         /* graphics context data structure */

  /* printf("in resize_x_axis\n"); */

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

/* Set the origin and end pixel values of the x axis. */
  data->plot->origin_x = 0.0;
  data->plot->end_x = pix_width;

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->plot->pix[7])
     XFreePixmap(XtDisplay(w), data->plot->pix[7]);

  data->plot->pix[7] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     pix_width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->plot->pix[7] == 0)
    {
     printf("No space to create pixmap for x axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->plot->gc[7] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->plot->gc[7] = XCreateGC(XtDisplay(w), data->plot->pix[7], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->plot->gc[7], background);
  XFillRectangle(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
		  0, 0, pix_width, height);

  XSetForeground(XtDisplay(w), data->plot->gc[7], foreground);

  draw_axes(w, data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_x_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_x_axis.c,v 1.1 1995/09/08 14:58:22 page Exp $";}
/*  ===================================================  */

}
