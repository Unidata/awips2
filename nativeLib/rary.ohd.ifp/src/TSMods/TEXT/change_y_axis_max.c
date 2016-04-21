#include "mods_plot.h"
#include "ifp_struct.h"
#include <X11/cursorfont.h>

/* File: change_y_axis_max.c    */

/*  change_y_axis_max is the callback function called when the
    change_y_axis_max_widget slider is moved.  It changes the value
    of discharge_axis_max and calls the appropriate routines to redraw the
    view with the new value.
*/

void change_y_axis_max(w, data, call_data)

  Widget                        w;		/* widget data structure	*/
  mods_plot_struct              *data;          /* mods plot data structure pointer     */
  XmScaleCallbackStruct         *call_data;     /* Xmscale call back structure pointer  */
{
  static Cursor wrist_watch = (unsigned int)NULL;

/*
 * Create a wrist watch cursor if not already done.
 */
 if(wrist_watch == (unsigned int)NULL)
    wrist_watch = XCreateFontCursor(XtDisplay(w), XC_watch);

/*
 * Define the wrist watch cursor for the current window.
 */
  XDefineCursor(XtDisplay(w), XtWindow(XtParent(w)), wrist_watch);

/*
 * Set new value for discharge_axis_max and call resize callbacks for
 *   each plot drawing area.
 */

  data->y_axis_max = data->default_y_axis_max * call_data->value;

  resize_mp_y_axis(data->drawing_area_widget[1], data, NULL);

  resize_mp_graph(data->drawing_area_widget[2], data, NULL);

  XUndefineCursor(XtDisplay(w), XtWindow(XtParent(w)));

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/change_y_axis_max.c,v $";
 static char rcs_id2[] = "$Id: change_y_axis_max.c,v 1.1 1995/09/08 14:58:48 page Exp $";}
/*  ===================================================  */

}
