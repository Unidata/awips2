#include "rating_curve.h"

#define  DELAY  200

/* File: rc_crosshairs.c
 *
 * Draws vertical and horizontal crosshairs
 *  which follow the cursor when the button is pressed
 *  in the mods plot.
 *
 * This global XtIntervalId (within this .c file) is
 *  needed because when the mods plot window is partially
 *  obscured the drawing area may be refreshed from its
 *  pixmap after the buttonPress event is processed in
 *  the mods plot window.  This causes the image in the window
 *  (which now has the initial crosshairs from the buttonPress)
 *  to be overwritten by the pixmap image which doesn't have
 *  the crosshairs.  As the mouse is moved, or the button
 *  released, the crosshairs are "erased" by drawing again.
 *  Since the image from the pixmap doesn't have the
 *  crosshairs from the initial drawing a crosshair remains
 *  on the screen.
 *
 * The fix is to delay the initial writing of the crosshair to the
 *  window by use of a timeout event handler.  The interval id
 *  is needed so that the timer can be removed if the button is
 *  release before the alarm goes off.
 */

XtIntervalId    *rc_crosshairs_timer_id;  /* rating curve crosshairs timer id */
int             rc_starting_x;            /* rating curve x pointer location */
int             rc_starting_y;            /* rating curve y pointer location */


void rc_crosshairs_timer(w, data, event)
  Widget                w;           /* widget data structure */
  rc_struct             *data;       /* rating curve structure data pointer */
  XEvent                *event;      /* XEvent structure pointer */
{
  Window        root, child;
  int           root_x, root_y;      /* coordinates relative to the root */
  unsigned int  keys_buttons;        /* button states */
/*
 * Query for the current pointer location, we may
 *  use this to draw the starting pointer location
 *  in start_crosshairs.
 */
  XQueryPointer(XtDisplay(w),
		XtWindow(data->drawing_area_widget[4]),
		&root, &child,
		&root_x, &root_y,
		&rc_starting_x, &rc_starting_y,
		&keys_buttons);
/*
 * When a buttonPress event occurs in the hydrograph window
 *  install a timeout callback and start the countdown.
 */
  XtAddEventHandler(data->drawing_area_widget[4],
		    ButtonReleaseMask, FALSE,
		    rc_disable_crosshairs_timer,
		    data);

  rc_crosshairs_timer_id = (XtIntervalId  *)XtAddTimeOut(DELAY,
				     rc_start_crosshairs,
				     data);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void rc_disable_crosshairs_timer(w, data, event)
  Widget                w;
  rc_struct             *data;
  XEvent                *event;
{
/*
 * Remove the timeout callback, then remove this
 *  function as a callback.
 */
  XtRemoveTimeOut((XtIntervalId)rc_crosshairs_timer_id);

  XtRemoveEventHandler(data->drawing_area_widget[4],
		       ButtonReleaseMask, FALSE,
		       rc_disable_crosshairs_timer,
		       data);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void rc_start_crosshairs(data, id)
  rc_struct             *data;
  XtIntervalId          *id;
{
  int           n;
  Arg           wargs[5];
  XEvent        event;

/*
 * If this function was called, the crosshairs alarm must have gone off,
 *  so remove the rc_disable_crosshairs_timer event handler.
 */
  XtRemoveEventHandler(data->drawing_area_widget[4],
		       ButtonReleaseMask, FALSE,
		       rc_disable_crosshairs_timer,
		       data);
/*
 * Now add event handlers to track the sprite motion, and to clear
 *  the crosshairs when the button is released.
 */
  XtAddEventHandler(data->drawing_area_widget[4],
		    ButtonMotionMask, FALSE, rc_track_crosshairs, data);
  XtAddEventHandler(data->drawing_area_widget[4],
		    ButtonReleaseMask, FALSE, rc_end_crosshairs, data);
/*
 * Get width and height of the rating curve drawing area.
 */
  n = 0;
  XtSetArg(wargs[n], XmNwidth, &data->drawing_area_4_width); n++;
  XtSetArg(wargs[n], XmNheight, &data->drawing_area_4_height); n++;
  XtGetValues(data->drawing_area_widget[4], wargs, n);

  event.xbutton.x = rc_starting_x;
  event.xbutton.y = rc_starting_y;

  rc_draw_crosshairs(data->drawing_area_widget[4],
		     data, &event);

}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void rc_track_crosshairs(w, data, event)
  Widget                w;
  rc_struct             *data;
  XEvent                *event;
{
 /*
  * Erase the old lines.
  */
  rc_erase_crosshairs(w, data, event);
 /*
  * Draw the new lines.
  */
  rc_draw_crosshairs(w, data, event);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void rc_end_crosshairs(w, data, event)
  Widget                w;
  rc_struct             *data;
  XEvent                *event;
{
 /*
  * Draw once to clear the previous lines.
  */
  rc_erase_crosshairs(w, data, event);
 /*
  * Remove the event handlers installed in start_crosshairs.
  */
  XtRemoveEventHandler(data->drawing_area_widget[4],
		       ButtonMotionMask, FALSE, rc_track_crosshairs, data);
  XtRemoveEventHandler(data->drawing_area_widget[4],
		       ButtonReleaseMask, FALSE, rc_end_crosshairs, data);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void rc_draw_crosshairs(w, data, event)
  Widget                w;
  rc_struct             *data;
  XEvent                *event;
{
/*
 * Draw horizontal and vertical lines in the
 *  mods drawing area.
 */
  XDrawLine(XtDisplay(w), XtWindow(data->drawing_area_widget[4]),
	    data->crosshairs_gc,
	    0, event->xbutton.y,
	    data->drawing_area_4_width, event->xbutton.y);

  XDrawLine(XtDisplay(w), XtWindow(data->drawing_area_widget[4]),
	    data->crosshairs_gc,
	    event->xbutton.x, 0,
	    event->xbutton.x, data->drawing_area_4_height);

  data->previous_mouse_position.x = event->xbutton.x;
  data->previous_mouse_position.y = event->xbutton.y;
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void rc_erase_crosshairs(w, data, event)
  Widget                w;
  rc_struct             *data;
  XEvent                *event;
{
 /*
  * Draw once to clear the previous lines.
  */
  XDrawLine(XtDisplay(w), XtWindow(data->drawing_area_widget[4]),
	    data->crosshairs_gc,
	    0,
	    data->previous_mouse_position.y,
	    data->drawing_area_4_width,
	    data->previous_mouse_position.y);

  XDrawLine(XtDisplay(w), XtWindow(data->drawing_area_widget[4]),
	    data->crosshairs_gc,
	    data->previous_mouse_position.x,
	    0,
	    data->previous_mouse_position.x,
	    data->drawing_area_4_height);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_crosshairs.c,v $";
 static char rcs_id2[] = "$Id: rc_crosshairs.c,v 1.2 2006/03/28 20:43:56 aivo Exp $";}
/*  ===================================================  */

}
