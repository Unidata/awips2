/*
 * File: rc_mousetracker.c
 *
 * The functions in this file will create the event_handlers to show
 *  the location of the mouse sprite in the rating curve window, and provide
 *  a digital display in x,y coordinates of the sprite position while
 *  the left mouse button is depressed.
 * These functions were originally taken from X Window System, Programming
 *  and Applications with Xt by Douglas Young, starting at page 140.
 * The functions have been extensively modified to handle English/metric
 *  units conversion and to display a varying of decimal places depending
 *  on the magnitude of the coordinate values.
 *
 * These functions originally written by Steve Wiele, 1990.
 * Modified extensively for units, significant digits, and context
 *   sensitive help by Donna Page, 1991.
 */
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
#include "rating_curve.h"
#include "libXifp.h"

typedef struct {
		Widget  top_help_shell;
		char    *widget_name;
	       }  help_cb_struct;


int   rc_create_mouse_tracker(parent, target, data, help_cb, top_help_shell)
      Widget            parent, target;
      Widget            top_help_shell;
      rc_struct         *data;     /* rating curve data structure pointer */
      help_cb_struct    *help_cb;
	{
	extern  void rc_clear_tracker();
	extern  void rc_track_mouse_position();
	extern  void rc_show_mouse_position();
	Widget  tracker;
	int     n;
	Arg     wargs[5];

	n=0;
	XtSetArg(wargs[n], XmNrecomputeSize, FALSE); n++;
	data->rc_mouse = XtCreateManagedWidget("rc_mousetracker",
						xmLabelWidgetClass,
						parent, wargs, n);

	XtAddEventHandler(target, ButtonPressMask, FALSE,
			  rc_show_mouse_position, data);
	XtAddEventHandler(target, ButtonMotionMask, FALSE,
			  rc_track_mouse_position, data);
	XtAddEventHandler(target, ButtonReleaseMask | LeaveWindowMask,
			  FALSE, rc_clear_tracker, data);

	help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

	help_cb->top_help_shell = top_help_shell;

	help_cb->widget_name = "rc_mousetracker";

	XtAddEventHandler(data->rc_mouse, EnterWindowMask, FALSE,
			  help_event_handler, help_cb);
	}

   void rc_show_mouse_position(w, data, event)
      Widget            w;
      rc_struct         *data;
      XEvent            *event;
	 {
	 extern void    xs_wprintf();
	 float          xval, yval, stage;
	 int            ixval;
	 float          range;

	 xval = pixel_to_val(&event->xbutton.x, &data->min_q_disp,
			     &data->max_q_disp, &data->origin_x,
			     &data->rc_h_slider_size);
	 yval = pixel_to_val(&event->xbutton.y, &data->min_stage_disp,
			     &data->max_stage_disp,
			     &data->rc_v_slider_size, &data->end_y);

	 ixval = (int)xval + 1;

	/* decide how many discharge (q) decimal points to print based on
	   the range of q values displayed */
	range = data->max_q_disp - data->min_q_disp;
	if(range > 10.0)
	   xs_wprintf(data->rc_mouse, "Q: %8.0f  Stg: %8.2f",
		      xval, yval);
	else if(range > 1.0 && range <= 10.0)
	   xs_wprintf(data->rc_mouse, "Q: %8.1f  Stg: %8.2f",
		      xval, yval);
	else if(range > 0.0 && range <= 1.0)
	   xs_wprintf(data->rc_mouse, "Q: %8.2f  Stg: %8.2f",
		      xval, yval);

	 /*printf("X: %4.0f, Y: %4.4f\n", xval, yval);*/
	 }

   void rc_track_mouse_position(w, data, event)
      Widget             w;
      rc_struct          *data;
      XEvent             *event;
	 {
	 extern void xs_wprintf();
	 float          xval, yval, stage;
	 int            ixval;
	 int            xpix, ypix;
	 float          range;

	 xpix = event->xmotion.x;
	 ypix = event->xmotion.y;

	 xval = pixel_to_val(&xpix, &data->min_q_disp,
			     &data->max_q_disp,
			     &data->origin_x, &data->rc_h_slider_size);
	 yval = pixel_to_val(&ypix, &data->min_stage_disp,
			     &data->max_stage_disp,
			     &data->rc_v_slider_size, &data->end_y);

	 ixval = (int)xval + 1;

	/* decide how many discharge (q) decimal points to print based on
	   the range of q values displayed */
	range = data->max_q_disp - data->min_q_disp;
	if(range > 10.0)
	   xs_wprintf(data->rc_mouse, "Q: %8.0f  Stg: %8.2f",
		      xval, yval);
	else if(range > 1.0 && range <= 10.0)
	   xs_wprintf(data->rc_mouse, "Q: %8.1f  Stg: %8.2f",
		      xval, yval);
	else if(range > 0.0 && range <= 1.0)
	   xs_wprintf(data->rc_mouse, "Q: %8.2f  Stg: %8.2f",
		      xval, yval);

       /*
	 printf("X: %4.4f, Y: %4.4f\n", xval, yval);
       */
	 }

   void rc_clear_tracker(w, data, event)
      Widget             w;
      rc_struct          *data;
      XEvent             *event;
	 {
	 extern void xs_wprintf();
	 xs_wprintf(data->rc_mouse, " ");

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_mousetracker.c,v $";
 static char rcs_id2[] = "$Id: rc_mousetracker.c,v 1.2 2006/01/26 20:35:16 dws Exp $";}
/*  ===================================================  */

	 }
