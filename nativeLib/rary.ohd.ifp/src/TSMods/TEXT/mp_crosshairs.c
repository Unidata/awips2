/* File : mp_crosshairs.c
 *
 * This global XtIntervalId (within this .c file) is
 *  needed because when the mods plot window is partially
 *  obscured the
 *  drawing area may be refreshed from its
 *  pixmap after the buttonPress event is processed in
 *  the mods plot window.  This causes the image in the window
 *  (which now has the initial crosshairs from the buttonPress)
 *  to be overwritten by the pixmap image which doesn't have
 *  the crosshairs.  As the mouse is moved, or the button
 *  released, the crosshairs are "erased" by drawing again.
 *  Since the image from the pixmap doesn't have the
 *  crosshairs from the initial drawing a crosshair remains
 *  on the screen.
 * The fix is to delay the initial writing of the crosshair to the
 *  window by use of a timeout event handler.  The interval id
 *  is needed so that the timer can be removed if the button is
 *  release before the alarm goes off.
 */

#include "mods_plot.h"

#define  DELAY  200

XtIntervalId    mp_crosshairs_timer_id;         /* crosshairs timer id structure        */
int             mp_starting_x, mp_starting_y;   /* x & y starting position              */

/*
 * mp_crosshairs.c - Draw vertical and horizontal crosshairs
 *  which follow the cursor when the button is pressed
 *  in the mods plot.
 */

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void mp_crosshairs_timer(w, data, event)
  Widget                w;              /* Widget data structure        */
  mods_plot_struct      *data;          /* Mods plot data structure pointer     */
  XEvent                *event;         /* Event structure      */
{
  Window        root;                   /* Root window structure        */
  Window        child;                  /* Child window structure       */
  int           root_x, root_y;         /* Root x & y position          */
  unsigned int  keys_buttons;           /* Keys, buttons values         */

/*
 * Query for the current pointer location, we may
 *  use this to draw the starting pointer location
 *  in start_crosshairs.
 */
  XQueryPointer(XtDisplay(w),
		XtWindow(data->drawing_area_widget[2]),
		&root, &child,
		&root_x, &root_y,
		&mp_starting_x, &mp_starting_y,
		&keys_buttons);
/*
 * When a buttonPress event occurs in the hydrograph window
 *  install a timeout callback and start the countdown.
 */
  XtAddEventHandler(data->drawing_area_widget[2],
		    ButtonReleaseMask, FALSE,
		    mp_disable_crosshairs_timer,
		    data);

  mp_crosshairs_timer_id = XtAddTimeOut(DELAY,
				     mp_start_crosshairs,
				     data);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void mp_disable_crosshairs_timer(w, data, event)
  Widget                w;
  mods_plot_struct       *data;
  XEvent                *event;
{
/*
 * Remove the timeout callback, then remove this
 *  function as a callback.
 */
  XtRemoveTimeOut(mp_crosshairs_timer_id);

  XtRemoveEventHandler(data->drawing_area_widget[2],
		       ButtonReleaseMask, FALSE,
		       mp_disable_crosshairs_timer,
		       data);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void mp_start_crosshairs(data, id)
  mods_plot_struct       *data;
  XtIntervalId          *id;
{
  int           n;
  Arg           wargs[5];
  XEvent        event;
/*
 * If this function was called, the crosshairs alarm must have gone off,
 *  so remove the mp_disable_crosshairs_timer event handler.
 */
  XtRemoveEventHandler(data->drawing_area_widget[2],
		       ButtonReleaseMask, FALSE,
		       mp_disable_crosshairs_timer,
		       data);
/*
 * Now add event handlers to track the sprite motion, and to clear
 *  the crosshairs when the button is released.
 */
  XtAddEventHandler(data->drawing_area_widget[2],
		    ButtonMotionMask, FALSE, mp_track_crosshairs, data);
  XtAddEventHandler(data->drawing_area_widget[2],
		    ButtonReleaseMask, FALSE, mp_end_crosshairs, data);
/*
 * Get width and height of the mods drawing area.
 */
  n = 0;
  XtSetArg(wargs[n], XmNwidth, &data->drawing_area_2_width); n++;
  XtSetArg(wargs[n], XmNheight, &data->drawing_area_2_height); n++;
  XtGetValues(data->drawing_area_widget[2], wargs, n);

  event.xbutton.x = mp_starting_x;
  event.xbutton.y = mp_starting_y;

  mp_draw_crosshairs(data->drawing_area_widget[2],
		  data, &event);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void mp_track_crosshairs(w, data, event)
  Widget                w;
  mods_plot_struct       *data;
  XEvent                *event;
{
 /*
  * Erase the old lines.
  */
  mp_erase_crosshairs(w, data, event);
 /*
  * Draw the new lines.
  */
  mp_draw_crosshairs(w, data, event);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void mp_end_crosshairs(w, data, event)
  Widget                w;
  mods_plot_struct       *data;
  XEvent                *event;
{
 /*
  * Draw once to clear the previous lines.
  */
  mp_erase_crosshairs(w, data, event);
 /*
  * Remove the event handlers installed in start_crosshairs.
  */
  XtRemoveEventHandler(data->drawing_area_widget[2],
		       ButtonMotionMask, FALSE, mp_track_crosshairs, data);
  XtRemoveEventHandler(data->drawing_area_widget[2],
		       ButtonReleaseMask, FALSE, mp_end_crosshairs, data);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void mp_draw_crosshairs(w, data, event)
  Widget                w;
  mods_plot_struct       *data;
  XEvent                *event;
{
/*
 * Draw horizontal and vertical lines in the
 *  mods drawing area.
 */
  XDrawLine(XtDisplay(w), XtWindow(data->drawing_area_widget[2]),
	 data->rb_gc,
				  0, event->xbutton.y,
	 data->drawing_area_2_width, event->xbutton.y);

  XDrawLine(XtDisplay(w), XtWindow(data->drawing_area_widget[2]),
	 data->rb_gc,
	 event->xbutton.x,                             0,
	 event->xbutton.x, data->drawing_area_2_height);

  data->previous_mouse_position.x = event->xbutton.x;
  data->previous_mouse_position.y = event->xbutton.y;
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void mp_erase_crosshairs(w, data, event)
  Widget                w;
  mods_plot_struct       *data;
  XEvent                *event;
{
 /*
  * Draw once to clear the previous lines.
  */
  XDrawLine(XtDisplay(w), XtWindow(data->drawing_area_widget[2]),
	    data->rb_gc,
	    0,
	    data->previous_mouse_position.y,
	    data->drawing_area_2_width,
	    data->previous_mouse_position.y);

  XDrawLine(XtDisplay(w), XtWindow(data->drawing_area_widget[2]),
	    data->rb_gc,
	    data->previous_mouse_position.x,
	    0,
	    data->previous_mouse_position.x,
	    data->drawing_area_2_height);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_crosshairs.c,v $";
 static char rcs_id2[] = "$Id: mp_crosshairs.c,v 1.1 1995/09/08 14:58:58 page Exp $";}
/*  ===================================================  */

}
