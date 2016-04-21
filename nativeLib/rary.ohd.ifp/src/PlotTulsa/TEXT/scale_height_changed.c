/* File: scale_height_changed.c
 *
 * Creates a wrist watch cursor if not already done.
 *
 * Defines the wrist watch cursor for the current window.
 *
 * Sets a new value for the height scale and calls resize callbacks
 * for each axis and plot drawing area.
 *
 */

#include "plot.h"
#include "ifp_struct.h"
#include <X11/cursorfont.h>

void scale_height_changed(w, data, call_data)

  Widget                        w;          /* widget data structure */
  combined_struct               *data;      /* tables and plot data structure pointer */
  XmScaleCallbackStruct         *call_data;
{
  static Cursor wrist_watch = (Cursor)0;
/*
 * Create a wrist watch cursor if not already done.
 */
 if(wrist_watch == (Cursor)0)
    wrist_watch = XCreateFontCursor(XtDisplay(w), XC_watch);
/*
 * Define the wrist watch cursor for the current window.
 */
  XDefineCursor(XtDisplay(w), XtWindow(XtParent(w)), wrist_watch);
/*
 * Set new value for height scale and call resize callbacks for
 *   each axis and plot drawing area.
 */
  data->plot->height_scale = call_data->value;

  resize_discharge_axis(data->plot->drawing_area_widget[4], data, NULL);

  resize_hydrograph(data->plot->drawing_area_widget[5], data, NULL);

  resize_stage_axis(data->plot->drawing_area_widget[6], data, NULL);

  XUndefineCursor(XtDisplay(w), XtWindow(XtParent(w)));

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/scale_height_changed.c,v $";
 static char rcs_id2[] = "$Id: scale_height_changed.c,v 1.2 2006/03/28 20:44:27 aivo Exp $";}
/*  ===================================================  */

}
