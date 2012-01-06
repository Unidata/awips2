/* File: resize_rc_y_axis_label.c
 *
 * Resizes events for mods plot drawing areas.
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

#include "rating_curve.h"
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"

void resize_rc_y_axis_label(w, data, call_data)

   Widget                        w;            /* widget data structure */
   rc_struct                     *data;        /* plot callback data structure pointer */
   XmDrawingAreaCallbackStruct   *call_data;
{
   Arg           wargs[10];                    /* window resource data structure array */
   Dimension     width, height;                /* drawing area dimensions */
   Pixel         foreground, background;       /* foreground, background colors */
   XGCValues     gcv;                          /* graphics context data structure */
   int           n;                            /* counters */
   int           mask;                         /* background/foreground mask */
   Font          font_id;                      /* font resource id */
   char          axis_label[25];               /* axis label character array */

   /*printf("in resize_rc_y_axis_label\n");*/

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

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->pix[0])
     XFreePixmap(XtDisplay(w), data->pix[0]);

  data->pix[0] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->pix[0] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->gc[0] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->gc[0] = XCreateGC(XtDisplay(w), data->pix[0], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->gc[0], background);
  XFillRectangle(XtDisplay(w), data->pix[0], data->gc[0],
		  0, 0, width, height);

  XSetForeground(XtDisplay(w), data->gc[0], foreground);

/* Set the appropriate axis label for the y axis */
  memset(axis_label, '\0', 20);
  if(NWSRFS_general_units == 0)
     strcpy(axis_label, "STAGE  ft ");
  else /* metric */
     strcpy(axis_label, "STAGE  m ");

  /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
  XSetFont(XtDisplay(w), data->gc[0], font_id);
  rc_label_axis(w, axis_label, data->pix[0], data->gc[0],
		10, height);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rz_rc_y_axis_label.c,v $";
 static char rcs_id2[] = "$Id: rz_rc_y_axis_label.c,v 1.2 2002/02/11 19:42:48 dws Exp $";}
/*  ===================================================  */

}
