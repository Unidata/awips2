/* File: resize_rc_graph.c
 *
 * Resizes events for mods plot drawing areas.
 * Creates new image to proper scale and stores it in
 * the pixmap.
 *
 * Gets the width and height of the drawing area and the
 * foreground and background colors to make the graphics
 * context to draw into the pixmap.
 *
 * Sets the origin and end pixel values for the x and y axes.
 *
 * Clears the window and generates an Expose event for the
 * entire window.
 *
 * Frees the old pixmap ,creates one the size of the window
 * and creates the graphics context when needed.
 *
 */


#include "rating_curve.h"

void resize_rc_graph(w, data, call_data)

   Widget                        w;            /* widget data structures */
   rc_struct                     *data;
   XmDrawingAreaCallbackStruct   *call_data;
{
   Arg           wargs[10];                    /* window resource data structure array */
   Dimension     width, height, pix_width, pix_height;
   Pixel         foreground, background;       /* foreground/backround colors */
   XGCValues     gcv;                          /* graphics context data structure */
   int           i,n;                          /* counters */
   int           mask;                         /* foreground/backround graphic contexts mask */

   /*printf("in resize_rc_graph\n");*/

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
  pix_height = data->height_scale * height;

/* Set the origin and end pixel values for the x and y axes. */
  data->origin_x = 0;
  data->end_x = pix_width;
  data->origin_y = pix_height;
  data->end_y = 0;

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);

/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->pix[4])
     XFreePixmap(XtDisplay(w), data->pix[4]);

  data->pix[4] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     pix_width, pix_height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->pix[4] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->gc[4] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->gc[4] = XCreateGC(XtDisplay(w), data->pix[4], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->gc[4], background);
  XFillRectangle(XtDisplay(w), data->pix[4], data->gc[4],
		  0, 0, pix_width, pix_height);
  XSetForeground(XtDisplay(w), data->gc[4], foreground);

  rc_draw_info_lines(w, data);
  rc_draw_graph_line(w, data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_rc_graph.c,v $";
 static char rcs_id2[] = "$Id: resize_rc_graph.c,v 1.1 1995/09/08 14:58:11 page Exp $";}
/*  ===================================================  */

}
