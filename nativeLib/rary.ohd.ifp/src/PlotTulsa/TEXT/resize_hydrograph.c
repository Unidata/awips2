/* File: resize_hydrograph.c
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
 * Clears the window and generates an Expose event for the
 * entire window.
 *
 * Frees the old pixmap and creates one the size of the window.
 *
 */

#include "plot.h"
#include "ifp_struct.h"
extern void draw_info_lines(Widget, combined_struct *);

void resize_hydrograph(w, data, call_data)

  Widget                        w;            /* widget data structure */
  combined_struct               *data;        /* call back graph data */
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                                 /* counters */
  Arg   wargs[10];                            /* window resource data structure array */
  Dimension     width, height;                /* drawing area dimensions */
  Dimension     pix_width, pix_height;        /* pixmap dimensions */
  Pixel         foreground, background;       /* foreground, background colors */
  int           mask;                         /* mask to declare which fields are valid */
  XGCValues     gcv;                          /* graphics context data structure */
  XPoint        *points;                      /* points data structure pointer */
  Window        pixmap_root;                  /* pixmap root window structure  */
  int           pixmap_x, pixmap_y;           /* pixmap window x & y position */
  unsigned int  pixmap_width, pixmap_height;  /* pixmap width & height */
  unsigned int  pixmap_border_width, pixmap_depth;  /* pixmap border width and depth */
  Status        pixmap_status;                /* window status */

 /* printf("in resize_hydrograph\n"); */

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
  pix_height = data->plot->height_scale * height;

/* Set the origin and end pixel values of the x and y axes. */
  data->plot->origin_x = 0;
  data->plot->origin_y = pix_height;
  data->plot->end_x = data->plot->origin_x + pix_width;
  data->plot->end_y = 0;

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
line_gcv.foreground */
  if(data->plot->pix[5])
     XFreePixmap(XtDisplay(w), data->plot->pix[5]);

  data->plot->pix[5] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     pix_width, pix_height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->plot->pix[5] == 0)
    {
     printf("No space to create pixmap for hydrograph drawing area\n");
     exit(1);
    }
/*
 * Make sure the pixmap was successfully created.
 */
  pixmap_status = XGetGeometry(XtDisplay(w), data->plot->pix[5],
			    &pixmap_root, &pixmap_x, &pixmap_y,
			    &pixmap_width, &pixmap_height,
			    &pixmap_border_width, &pixmap_depth);
  if(pixmap_status == 0)
    printf("bad pixmap in XGetGeometry for hydrograph drawing area\n");
/*
 * Create graphics context if needed.
 */
  if(data->plot->gc[5] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->plot->gc[5] = XCreateGC(XtDisplay(w), data->plot->pix[5], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->plot->gc[5], background);
  XFillRectangle(XtDisplay(w), data->plot->pix[5], data->plot->gc[5],
		  0, 0, pix_width, pix_height);

/* Create graphics contexts for each plotted time series. */
  for(i=0; i<*data->plot->num_plotted_ts; i++)
  {
     if(data->plot->line_gc[i] == NULL)
     {
	gcv.foreground = get_pixel_by_name(w, data->plot->ts_color[i]);
	data->plot->line_gc[i] = XCreateGC(XtDisplay(w), data->plot->pix[5],
				     GCForeground, &gcv);
     }
  }
  draw_lines(w, data, call_data);
  draw_info_lines(w, data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/PlotTulsa/RCS/resize_hydrograph.c,v $";
 static char rcs_id2[] = "$Id: resize_hydrograph.c,v 1.3 2007/05/16 16:42:10 aivo Exp $";}
/*  ===================================================  */

}
