/* File: resize_mp_graph.c
 *
 * Handles resize events for mods plot drawing areas.
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

#include "mods_plot.h"

void resize_mp_graph(w, data, call_data)

   Widget                        w;             /* Widget data structure        */
   mods_plot_struct              *data;         /* Mods plot data structure pointer     */
   XmDrawingAreaCallbackStruct   *call_data;    /* XmDrawingArea call back structure pointer    */
{
   Arg          wargs[10];      /* Window resource data structure array */
   Dimension    width;          /* Window width         */
   Dimension    height;         /* Window height        */
   Pixel        foreground;     /* Window foreground color              */
   Pixel        background;     /* Window background color              */
   XGCValues    gcv;            /* Graphics context data structures     */
   int          i, n;           /* Counters             */
   int          mask;           /* Foreground/backround graphic contexts mask   */
   int          bar_width;      /* Width of respective bar              */

   /*printf("in resize_mp_graph\n");*/

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

/* Set the origin and end pixel values for the x and y axes.
   For the y axis, if the mod is RRICHNG set origin_y at top of graph
   so bars start at the top.
*/
  data->origin_x = 0;
  data->end_x = width;
  if(data->mod_type_sw == RRICHNG)
  {
     data->origin_y = 0;
     data->end_y = height;
  }
  else /* UH or ROCHNG */
  {
     data->origin_y = height;
     data->end_y = 0;
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
  if(data->pix[2])
     XFreePixmap(XtDisplay(w), data->pix[2]);

  data->pix[2] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->pix[2] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
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
		  0, 0, width, height);

  bar_width = (short)((float)width /
	      (float)(data->num_pts-1) * .75 /
	      (float)data->num_ts_sel);

  if(bar_width < 1) bar_width = 1;

  data->bar_width = bar_width;

  /* Calculate the number of time series that can be plotted. */
  if( (width*0.75) >= ((data->num_pts-1)*data->num_ts_sel*bar_width) )
     data->num_ts_plotted = data->num_ts_sel;
  else
  {
     data->num_ts_plotted = (width * 0.75)/((data->num_pts-1) * bar_width);
     printf("Warning: Not enough space to plot all of the px or ro time series.\n");
     printf("         Plotting first %d time series.  Expand window \n", data->num_ts_plotted);
     printf("         to see more time series.\n");
  }

  for(i=0; i<data->num_ts_plotted; i++)
     if(data->mod_type_sw == RRICHNG)
	mp_draw_rectangles(w, data, PX, i);
     else  /* UH or ROCHNG */
	mp_draw_rectangles(w, data, RO, i);

  mp_draw_end_obs_line(w, data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/resize_mp_graph.c,v $";
 static char rcs_id2[] = "$Id: resize_mp_graph.c,v 1.1 1995/09/08 14:59:13 page Exp $";}
/*  ===================================================  */

}
