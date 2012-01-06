/* File: rc_scale_height_changed.c
 *
 * Creates a wrist watch cursor when still processing.
 *
 * Sets new value for height scale and calls resize callbacks
 * for each axis and plot drawing area.
 *
 */

#include "rating_curve.h"
#include "ifp_struct.h"
#include <X11/cursorfont.h>

void rc_scale_height_changed(w, data, call_data)

  Widget                        w;
  rc_struct                     *data;      /* rating curve data structure pointer */
  XmScaleCallbackStruct         *call_data; /* call back data structure pointer */
{
  static Cursor wrist_watch = (Cursor)0;
/*
 * Create a wrist watch cursor if not already done.
 */
 if(wrist_watch == 0)
    wrist_watch = XCreateFontCursor(XtDisplay(w), XC_watch);
/*
 * Define the wrist watch cursor for the current window.
 */
  XDefineCursor(XtDisplay(w), XtWindow(XtParent(w)), wrist_watch);
/*
 * Set new value for height scale and call resize callbacks for
 *   each axis and plot drawing area.
 */
  data->height_scale = call_data->value;

  resize_rc_y_axis(data->drawing_area_widget[1], data, NULL);

  resize_rc_graph(data->drawing_area_widget[4], data, NULL);

  XUndefineCursor(XtDisplay(w), XtWindow(XtParent(w)));

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_scale_height_changed.c,v $";
 static char rcs_id2[] = "$Id: rc_scale_height_changed.c,v 1.2 2006/03/28 20:44:02 aivo Exp $";}
/*  ===================================================  */

}
