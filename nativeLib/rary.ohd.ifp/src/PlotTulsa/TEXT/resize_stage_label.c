/* File: resize_stage_label.c
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
 */

#include "plot.h"
#include "ifp_struct.h"
#include <X11/Intrinsic.h>
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"

void resize_stage_label(w, data, call_data)

  Widget                        w;           /* widget data structure */
  plot_cb_struct                     *data;  /* plot callback data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                                /* counters */
  Arg   wargs[10];                           /* window resource data structure array */
  Dimension     width, height;               /* drawing area dimensions */
  Pixel         foreground, background;      /* foreground, background colors */
  int           mask;                        /* background, foreground mask */
  XGCValues     gcv;                         /* graphics context data structure */
  char          axis_label[9];               /* axis label character array */
  Font          font_id;                     /* font resource id */

  /* printf("in resize_stage_label\n"); */

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
/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->stage_label_pix)
     XFreePixmap(XtDisplay(w), data->stage_label_pix);

  data->stage_label_pix = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->stage_label_pix == 0)
    {
     printf("No space to create pixmap for stage_label drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->stage_label_gc == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->stage_label_gc = XCreateGC(XtDisplay(w), data->stage_label_pix, mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->stage_label_gc, background);
  XFillRectangle(XtDisplay(w), data->stage_label_pix, data->stage_label_gc,
		  0, 0, width, height);

  XSetForeground(XtDisplay(w), data->stage_label_gc, foreground);

  if(data->rc_data->RC == TRUE)
  {
     memset(axis_label, '\0', 9);
     if(data->h_q_plot_indicator != 1)
     {
        if(NWSRFS_general_units == 0)
	   strncpy(axis_label, "STAGE ft", 8);
        else
	   strncpy(axis_label, "STAGE m", 7);
     }
     else /* only label this if h_q_plot_indicator=1 */
        strncpy(axis_label, "RC SHIFT", 8);
        
        
     /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
     XSetFont(XtDisplay(w), data->stage_label_gc, font_id);
     label_axis(w, axis_label, data->stage_label_pix,
		data->stage_label_gc,
		1, height);
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_stage_label.c,v $";
 static char rcs_id2[] = "$Id: resize_stage_label.c,v 1.3 2002/02/11 19:40:24 dws Exp $";}
/*  ===================================================  */

}
