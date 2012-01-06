/* File: resize_mp_x_axis.c
 *
 * Handles resize events for mods plot drawing areas.
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

#include "mods_plot.h"

void resize_mp_x_axis(w, data, call_data)

   Widget                        w;             /* Widget data structure        */
   mods_plot_struct              *data;         /* mods plot data structure pointer     */
   XmDrawingAreaCallbackStruct   *call_data;    /* XmDrawingArea call back structure pointer    */
{
   Arg          wargs[10];      /* window resource data structure array */
   Dimension    width;          /* window width         */
   Dimension    height;         /* window height        */
   Pixel        foreground;     /* window foreground color              */
   Pixel        background;     /* window background color              */
   XGCValues    gcv;            /* graphics context data structures     */
   int          n;              /* Counter              */
   int          mask;           /* Foreground/backround graphic contexts mask   */

   /*printf("in resize_mp_x_axis\n");*/

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

/* Set the end pixel value of the x axis */
  data->end_x = 0.8*data->form_width;

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->pix[3])
     XFreePixmap(XtDisplay(w), data->pix[3]);

  data->pix[3] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->pix[3] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->gc[3] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->gc[3] = XCreateGC(XtDisplay(w), data->pix[3], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->gc[3], background);
  XFillRectangle(XtDisplay(w), data->pix[3], data->gc[3],
		  0, 0, width, height);

  XSetForeground(XtDisplay(w), data->gc[3], foreground);

  mp_draw_x_axis(w, data);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/resize_mp_x_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_mp_x_axis.c,v 1.1 1995/09/08 14:59:15 page Exp $";}
/*  ===================================================  */

}
