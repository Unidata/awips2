#include "plot.h"
#include "ifp_struct.h"

#define  DELAY  200

/* File: crosshairs.c
 *
 * This global XtIntervalId (within this .c file) is
 *  needed because when the Tulsa plot window is partially
 *  obscured any or all of the precip, runoff, or hydrograph
 *  drawing areas may be refreshed from their respective
 *  pixmaps after the buttonPress event is processed in
 *  the Tulsa plot window.  This causes the image in the window
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
 *
 *  Draw vertical and horizontal crosshairs
 *  which follow the cursor when the button is pressed
 *  in the hydrograph plot.
 *  Also, draw vertical lines through the precipitation
 *  and runoff plots above the cursor location.
 *
 * Modified 11/8/91 - gfs
 *  to display crosshairs in px and ro plots also.
 */

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

XtIntervalId    *crosshairs_timer_id;    /* crosshairs timer id structure */
int             starting_x, starting_y;  /* x & y starting position       */

void crosshairs_timer(w, data, event)
  Widget                w;               /* widget data structure         */
  combined_struct       *data;           /* tables and plot data structure pointer */
  XEvent                *event;          /* event structure */
{
  Window        root;                    /* root window structure  */
  Window        child;                   /* child window structure */
  int           root_x, root_y;          /* root x & y position    */
  unsigned int  keys_buttons;            /* keys, buttons values   */
/*
 * Store the widget where the button press event occurred
 *  in the data structure.
 */
  data->plot->crosshairs_event_widget =
	  XtWindowToWidget(XtDisplay(w), event->xbutton.window);
/*
 * Query for the current pointer location, we may
 *  use this to draw the starting pointer location
 *  in start_crosshairs.
 */
  XQueryPointer(XtDisplay(w),
		event->xbutton.window,
		&root, &child,
		&root_x, &root_y,
		&starting_x, &starting_y,
		&keys_buttons);
/*
 * When a buttonPress event occurs in the px, ro, or hydrograph window
 *  install a timeout callback and start the countdown.
 */
  XtAddEventHandler(data->plot->crosshairs_event_widget,
		    ButtonReleaseMask, FALSE,
		    disable_crosshairs_timer,
		    data);

  crosshairs_timer_id = (XtIntervalId *)XtAddTimeOut(DELAY,
				     start_crosshairs,
				     data);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void disable_crosshairs_timer(w, data, event)
  Widget                w;
  combined_struct       *data;
  XEvent                *event;
{
/*
 * Remove the timeout callback, then remove this
 *  function as a callback.
 */
  XtRemoveTimeOut((XtIntervalId)crosshairs_timer_id);

  XtRemoveEventHandler(data->plot->crosshairs_event_widget,
		       ButtonReleaseMask, FALSE,
		       disable_crosshairs_timer,
		       data);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void start_crosshairs(data, id)
  combined_struct       *data;
  XtIntervalId          *id;
{
  int           n;
  Arg           wargs[5];
  XEvent        event;
/*
 * If this function was called, the crosshairs alarm must have gone off,
 *  so remove the disable_crosshairs_timer event handler.
 */
  XtRemoveEventHandler(data->plot->crosshairs_event_widget,
		       ButtonReleaseMask, FALSE,
		       disable_crosshairs_timer,
		       data);
/*
 * Now add event handlers to track the sprite motion, and to clear
 *  the crosshairs when the button is released.
 */
  XtAddEventHandler(data->plot->crosshairs_event_widget,
		    ButtonMotionMask, FALSE, track_crosshairs, data);
  XtAddEventHandler(data->plot->crosshairs_event_widget,
		    ButtonReleaseMask, FALSE, end_crosshairs, data);
/*
 * Get width and height of the hydrograph drawing area,
 *  and the height of the px_x_axis and ro_x_axis drawing areas.
 */
  n = 0;
  XtSetArg(wargs[n], XmNwidth, &data->plot->hydrograph_width); n++;
  XtSetArg(wargs[n], XmNheight, &data->plot->hydrograph_height); n++;
  XtGetValues(data->plot->drawing_area_widget[5], wargs, n);

  if(data->plot->num_rr_oper > 0)
    {
     n = 0;
     XtSetArg(wargs[n], XmNheight, &data->plot->ro_height); n++;
     XtGetValues(data->plot->drawing_area_widget[3], wargs, n);
    }
  event.xbutton.x = starting_x;
  event.xbutton.y = starting_y;

  draw_crosshairs(data->plot->crosshairs_event_widget,
		  data, &event);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void track_crosshairs(w, data, event)
  Widget                w;
  combined_struct       *data;
  XEvent                *event;
{
 /*
  * Erase the old lines.
  */
  erase_crosshairs(w, data, event);
 /*
  * Draw the new lines.
  */
  draw_crosshairs(w, data, event);
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void end_crosshairs(w, data, event)
  Widget                w;
  combined_struct       *data;
  XEvent                *event;
{
 /*
  * Draw once to clear the previous lines.
  */
  erase_crosshairs(w, data, event);
 /*
  * Remove the event handlers installed in start_crosshairs.
  */
  XtRemoveEventHandler(data->plot->crosshairs_event_widget,
		       ButtonMotionMask, FALSE, track_crosshairs, data);
  XtRemoveEventHandler(data->plot->crosshairs_event_widget,
		       ButtonReleaseMask, FALSE, end_crosshairs, data);
/*
 * For neatness, set crosshairs_event_widget to NULL.
 */
  data->plot->crosshairs_event_widget = NULL;
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void draw_crosshairs(w, data, event)
  Widget                w;
  combined_struct       *data;
  XEvent                *event;
{
  int   px_top, ro_bottom;
  GC    gc;
/*
 * Draw horizontal line in the crosshairs_event_widget drawing area.
 */
  if      (data->plot->crosshairs_event_widget ==
	   data->plot->drawing_area_widget[5])
				   gc = data->plot->rb_gc;
  else if (data->plot->crosshairs_event_widget ==
	   data->plot->drawing_area_widget[3])
				   gc = data->plot->rb_gc_ro;
  else                             gc = data->plot->rb_gc_px;

  XDrawLine(XtDisplay(w), XtWindow(data->plot->crosshairs_event_widget),
	 gc,
				    0, event->xbutton.y,
	 data->plot->hydrograph_width, event->xbutton.y);
/*
 * Draw vertical line in the hydrograph drawing area.
 */
  XDrawLine(XtDisplay(w), XtWindow(data->plot->drawing_area_widget[5]),
	 data->plot->rb_gc,
	 event->xbutton.x,                             0,
	 event->xbutton.x, data->plot->hydrograph_height);
/*
 * Draw the vertical lines in the px and ro_x_axes if any data in plots.
 */
  if(data->plot->num_rr_oper > 0)
    {
     px_top = 0.1 * data->plot->px_height;
     ro_bottom = 0.9 * data->plot->ro_height;

     XDrawLine(XtDisplay(w), XtWindow(data->plot->drawing_area_widget[1]),
	    data->plot->rb_gc_px,
	    event->xbutton.x,                px_top,
	    event->xbutton.x, data->plot->px_height);

     XDrawLine(XtDisplay(w), XtWindow(data->plot->drawing_area_widget[3]),
	    data->plot->rb_gc_ro,
	    event->xbutton.x,         0,
	    event->xbutton.x, ro_bottom);
    }
  data->plot->previous_mouse_position.x = event->xbutton.x;
  data->plot->previous_mouse_position.y = event->xbutton.y;
}

/* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ */

void erase_crosshairs(w, data, event)
  Widget                w;
  combined_struct       *data;
  XEvent                *event;
{
  int           px_top, ro_bottom;
  GC    gc;
 /*
  * Draw once to clear the previous lines.
  */
  if      (data->plot->crosshairs_event_widget ==
	   data->plot->drawing_area_widget[5])
				   gc = data->plot->rb_gc;
  else if (data->plot->crosshairs_event_widget ==
	   data->plot->drawing_area_widget[3])
				   gc = data->plot->rb_gc_ro;
  else                             gc = data->plot->rb_gc_px;

  XDrawLine(XtDisplay(w), XtWindow(data->plot->crosshairs_event_widget),
	    gc,
	    0,
	    data->plot->previous_mouse_position.y,
	    data->plot->hydrograph_width,
	    data->plot->previous_mouse_position.y);

  XDrawLine(XtDisplay(w), XtWindow(data->plot->drawing_area_widget[5]),
	    data->plot->rb_gc,
	    data->plot->previous_mouse_position.x,
	    0,
	    data->plot->previous_mouse_position.x,
	    data->plot->hydrograph_height);

  if(data->plot->num_rr_oper > 0)
    {
     px_top = 0.1 * data->plot->px_height;
     ro_bottom = 0.9 * data->plot->ro_height;

     XDrawLine(XtDisplay(w), XtWindow(data->plot->drawing_area_widget[1]),
	       data->plot->rb_gc_px,
	       data->plot->previous_mouse_position.x,
	       px_top,
	       data->plot->previous_mouse_position.x,
	       data->plot->px_height);

     XDrawLine(XtDisplay(w), XtWindow(data->plot->drawing_area_widget[3]),
	       data->plot->rb_gc_ro,
	       data->plot->previous_mouse_position.x,
	       0,
	       data->plot->previous_mouse_position.x,
	       ro_bottom);
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/crosshairs.c,v $";
 static char rcs_id2[] = "$Id: crosshairs.c,v 1.2 2006/03/28 20:43:38 aivo Exp $";}
/*  ===================================================  */

}
