 /* these functions from Young, p.140 */
#include <stdarg.h>
#include <stdio.h>

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>
#include <Xm/Label.h>
#include "libXs.h"
#include "mods_plot.h"

typedef struct {
		Widget  top_help_shell;
		char    *widget_name;
	       } help_cb_struct;

int   mp_create_mouse_tracker(parent, target, data, help_cb, top_help_shell)
      Widget            parent, target;
      Widget            top_help_shell;
      mods_plot_struct  *data;
      help_cb_struct    *help_cb;
	{
	extern  void clear_tracker();
	extern  void track_mouse_position();
	extern  void show_mouse_position();
	int     n;
	Arg     wargs[5];

	n=0;
	XtSetArg(wargs[n], XmNrecomputeSize, FALSE); n++;
	data->mp_mouse = XtCreateManagedWidget("mp_mousetracker",
						xmLabelWidgetClass,
						parent, wargs, n);
	XtAddEventHandler(target, ButtonPressMask, FALSE,
			  mp_show_mouse_position, data);

	XtAddEventHandler(target, ButtonMotionMask, FALSE,
			  mp_track_mouse_position, data);
	XtAddEventHandler(target, ButtonReleaseMask | LeaveWindowMask,
			  FALSE, mp_clear_tracker, data);

	help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

	help_cb->top_help_shell = top_help_shell;

	help_cb->widget_name = "mp_mousetracker";

	XtAddEventHandler(data->mp_mouse, EnterWindowMask, FALSE,
			  help_event_handler, help_cb);
	}

   void mp_show_mouse_position(w, data, event)
      Widget            w;
      mods_plot_struct  *data;
      XEvent            *event;
	 {
	 extern void    xs_wprintf();
	 float          xval, yval;
	 int            ixval;

	 xval = pixel_to_val(&event->xbutton.x, &data->min_x,
			     &data->max_x, &data->origin_x,
			     &data->end_x);
	 yval = pixel_to_val(&event->xbutton.y, &data->min_y,
			     &data->y_axis_max,
			     &data->origin_y, &data->end_y);

	 ixval = (int)xval + 1;

	 xs_wprintf(data->mp_mouse, "Date: %s  %s: %4.2f",
		    data->day_hr[ixval], data->mp_tracker_label, yval);

	 /*printf("X: %4.0f, Y: %4.4f\n", xval, yval);*/
	 }

   void mp_track_mouse_position(w, data, event)
      Widget             w;
      mods_plot_struct   *data;
      XEvent             *event;
	 {
	 extern void xs_wprintf();
	 float          xval, yval;
	 int            ixval;
	 int            xpix, ypix;

	 xpix = event->xmotion.x;
	 ypix = event->xmotion.y;

	 xval = pixel_to_val(&xpix, &data->min_x,
			     &data->max_x,
			     &data->origin_x, &data->end_x);
	 yval = pixel_to_val(&ypix, &data->min_y,
			     &data->y_axis_max,
			     &data->origin_y, &data->end_y);

	 ixval = (int)xval + 1;

	 xs_wprintf(data->mp_mouse, "Date: %s  %s: %4.2f",
		    data->day_hr[ixval], data->mp_tracker_label, yval);
       /*
	 printf("X: %4.4f, Y: %4.4f\n", xval, yval);
       */
	 }

   void mp_clear_tracker(w, data, event)
      Widget             w;
      mods_plot_struct   *data;
      XEvent             *event;
	 {
	 extern void xs_wprintf();
	 xs_wprintf(data->mp_mouse, " ");

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_mousetracker.c,v $";
 static char rcs_id2[] = "$Id: mp_mousetracker.c,v 1.2 2006/01/26 20:35:12 dws Exp $";}
/*  ===================================================  */

	 }
