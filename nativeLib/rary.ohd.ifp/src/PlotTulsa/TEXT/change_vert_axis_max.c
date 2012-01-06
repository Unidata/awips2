#include "plot.h"
#include "ifp_struct.h"
#include <X11/cursorfont.h>


/* File: change_vert_axis_max.c
 *
 *  change_vert_axis_max is the callback function called when the
 *  change_vert_axis_max_widget slider is moved.  It changes the value
 *  of discharge_axis_max and calls the appropriate routines to redraw the
 *  view with the new value.
 *
 */

void change_vert_axis_max(w, data, call_data)

  Widget                        w;              /* widget data structure       */
  combined_struct               *data;          /* tables and plot data structure pointer */
  XmScaleCallbackStruct         *call_data;     /* XmScale call back structure pointer    */
{
  static Cursor wrist_watch = (Cursor)0;

/*
 * Create a wrist watch cursor if not already done.
 */
 if( wrist_watch == (Cursor)0 )
    wrist_watch = XCreateFontCursor(XtDisplay(w), XC_watch);

/*
 * Define the wrist watch cursor for the current window.
 */
  XDefineCursor(XtDisplay(w), XtWindow(XtParent(w)), wrist_watch);

/*
 * Set new value for discharge_axis_max and call resize callbacks for
 *   each plot drawing area.
 */

  *data->plot->discharge_axis_max = *data->plot->min_y +
				    10 * data->plot->discharge_increment *
				    call_data->value;
  resize_discharge_axis(data->plot->drawing_area_widget[4], data, NULL);

  resize_hydrograph(data->plot->drawing_area_widget[5], data, NULL);

  resize_stage_axis(data->plot->drawing_area_widget[6], data, NULL);

  XUndefineCursor(XtDisplay(w), XtWindow(XtParent(w)));

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/change_vert_axis_max.c,v $";
 static char rcs_id2[] = "$Id: change_vert_axis_max.c,v 1.2 2006/03/28 20:43:32 aivo Exp $";}
/*  ===================================================  */

}
