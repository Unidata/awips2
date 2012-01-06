/*
 *      mouse_fcns.c
 *
 * The functions in this file will create the event_handlers to show
 *  the location of the mouse sprite in the hydrograph, px, or ro windows,
 *  and provide a digital display in x,y coordinates of the sprite
 *  position while the left mouse button is depressed.
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
#include "plot.h"
#include "ifp_struct.h"
#include "libXifp.h"
#include "mod_struct.h" 
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"
#include "c_call_f/fcshft.h"

typedef struct {
		Widget  top_help_shell;
		char    *widget_name;
	       } help_cb_struct;

float         q_to_stage();    /* discharge rate to stage data */

int   create_mouse_tracker(parent, px_widget, ro_widget,
				   hydrograph_widget,
				   data, help_cb, top_help_shell)
      Widget            parent;
      Widget            px_widget, ro_widget, hydrograph_widget;
      Widget            top_help_shell;
      plot_cb_struct    *data;        /* call back data structure pointer */
      help_cb_struct    *help_cb;     /* call back help data structure pointer */
	 {
	extern  void show_mouse_position();
	extern  void track_mouse_position();
	extern  void clear_tracker();
	int     n;         /* array index */
	Arg     wargs[5];  /* window resource data structure array */

	n=0;
	XtSetArg(wargs[n], XmNrecomputeSize, FALSE); n++;
	data->mouse_rc = XtCreateManagedWidget("mousetracker",
						xmLabelWidgetClass,
						parent, wargs, n);

	XtAddEventHandler(px_widget, ButtonPressMask, FALSE,
			  show_mouse_position, data);
	XtAddEventHandler(ro_widget, ButtonPressMask, FALSE,
			  show_mouse_position, data);
	XtAddEventHandler(hydrograph_widget, ButtonPressMask, FALSE,
			  show_mouse_position, data);

	XtAddEventHandler(px_widget, ButtonMotionMask, FALSE,
			  track_mouse_position, data);
	XtAddEventHandler(ro_widget, ButtonMotionMask, FALSE,
			  track_mouse_position, data);
	XtAddEventHandler(hydrograph_widget, ButtonMotionMask, FALSE,
			  track_mouse_position, data);

	XtAddEventHandler(px_widget,
			  ButtonReleaseMask | LeaveWindowMask,
			  FALSE, clear_tracker, data);
	XtAddEventHandler(ro_widget,
			  ButtonReleaseMask | LeaveWindowMask,
			  FALSE, clear_tracker, data);
	XtAddEventHandler(hydrograph_widget,
			  ButtonReleaseMask | LeaveWindowMask,
			  FALSE, clear_tracker, data);

	help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

	help_cb->top_help_shell = top_help_shell;

	help_cb->widget_name = "mousetracker";

	XtAddEventHandler(data->mouse_rc, EnterWindowMask, FALSE,
			  help_event_handler, help_cb);
	}

   void show_mouse_position(w, data, event)
      Widget    w;
      plot_cb_struct *data;
      XEvent    *event;

   {
    extern void    xs_wprintf(Widget, char *, ...);
    float          xval, yval, stage, yval_metric;
    int            ixval;
    int            zero, px_drawing_area_height;
    float          range;

      xval = pixel_to_val(&event->xbutton.x, &data->min_time_disp,
			  &data->max_time_disp, &data->origin_x,
			  &data->h_slider_size);

/*      ixval = (int)xval + 1;*/
 /*ixval is hour index for fcshft(),but is period index after it ---kwz*/
       ixval = ((int)xval + 1) * data->plot_delta_t;

      if (data->rc_data->RC == TRUE)
      {
         FCSHFT(&ixval,data->rc_data->rc_id);
      }
       ixval = (int)xval + 1;

      if(w == data->drawing_area_widget[5])
	{
	 yval = pixel_to_val(&event->xbutton.y, &data->min_discharge_disp,
			     &data->max_discharge_disp,
			     &data->v_slider_size, &data->end_y);

	 /* find range of discharge values displayed;
	    used to determine number decimal pts */
	 range = data->max_discharge_disp - data->min_discharge_disp;

	 if(data->rc_data->RC == TRUE)
	 {
	    if(NWSRFS_general_units == 0)
	    {
	      /* convert from English to metric */
	      yval_metric = (yval - data->rc_data->q_add_constant) /
			     data->rc_data->q_mult_conver_factor;

	      stage = q_to_stage(yval_metric);

	      /* convert from metric to English*/
	      stage = stage * data->rc_data->stg_mult_conver_factor +
		      data->rc_data->stg_add_constant;
	    }
	    else
	       stage = q_to_stage(yval);

	    /* decide how many discharge decimal pts to print based on range */
	    if(range > 10.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.0f  Stg: %5.2f",
			  data->day_hrs[ixval], data->tracker_label, yval,
			  stage);
	    else if(range > 1.0 && range <= 10.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.1f  Stg: %5.2f",
			  data->day_hrs[ixval], data->tracker_label, yval,
			  stage);
	    else if(range > 0.0 && range <= 1.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.2f  Stg: %5.2f",
			  data->day_hrs[ixval], data->tracker_label, yval,
			  stage);
	 }
	 else
	 {
	    /* decide how many discharge decimal pts to print based on range */
	    if(range > 10.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.0f",
			  data->day_hrs[ixval], data->tracker_label, yval);
	    else if(range > 1.0 && range <= 10.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.1f",
			  data->day_hrs[ixval], data->tracker_label, yval);
	    else if(range > 0.0 && range <= 1.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.2f",
			  data->day_hrs[ixval], data->tracker_label, yval);
	 }
	}

      else if(w == data->drawing_area_widget[3])
	{
	 zero = 0;

	 yval = pixel_to_val(&event->xbutton.y,
			     &data->ro_min,
			     &data->px_ro_y_axis_max,
			     &data->ro_origin_y,
			     &zero);
	 if(yval < 0.0) yval = 0.0;

	 xs_wprintf(data->mouse_rc, "Date: %s, Runoff: %4.2f",
		    data->day_hrs[ixval], yval);
	}

      else if(w == data->drawing_area_widget[1])
	{
	 /* local variable to store data->px_height (Dimension)
	    as an int to be passed to pixel_to_val - dp, 5/14/92
	 */
	 px_drawing_area_height = data->px_height;
	 yval = pixel_to_val(&event->xbutton.y,
			     &data->px_min,
			     &data->px_ro_y_axis_max,
			     &data->px_origin_y,
			     &px_drawing_area_height);
	 if(yval < 0.0) yval = 0.0;

	 xs_wprintf(data->mouse_rc, "Date: %s, Px / Rain+Melt: %4.2f",
		    data->day_hrs[ixval], yval);
	}
   }

   void track_mouse_position(w, data, event)
      Widget      w;
      plot_cb_struct   *data;
      XEvent      *event;
     {
      extern void xs_wprintf(Widget, char *, ...);
      float          xval, yval, stage, yval_metric;
      int            ixval;
      int            zero, px_drawing_area_height;
      int            xpix, ypix;
      float          range;

      xpix = event->xmotion.x;
      ypix = event->xmotion.y;

      xval = pixel_to_val(&xpix, &data->min_time_disp,
			  &data->max_time_disp, &data->origin_x,
			  &data->h_slider_size);

/*      ixval = (int)xval + 1;*/
       ixval = ((int)xval + 1) * data->plot_delta_t;

      if (data->rc_data->RC == TRUE)
      {
         FCSHFT(&ixval,data->rc_data->rc_id);
      }
       ixval = (int)xval + 1;

      if(w == data->drawing_area_widget[5])
	{
	 yval = pixel_to_val(&ypix, &data->min_discharge_disp,
			     &data->max_discharge_disp,
			     &data->v_slider_size, &data->end_y);

	 /* find range of discharge values displayed;
	    used to determine number of decimal pts */
	 range = data->max_discharge_disp - data->min_discharge_disp;

	 if(data->rc_data->RC == TRUE)
	 {
	    if(NWSRFS_general_units == 0)
	    {
	      /* convert from English to metric */
	      yval_metric = (yval - data->rc_data->q_add_constant) /
			     data->rc_data->q_mult_conver_factor;

	      stage = q_to_stage(yval_metric);

	      /* convert from metric to English*/
	      stage = stage * data->rc_data->stg_mult_conver_factor +
		      data->rc_data->stg_add_constant;
	    }
	    else
	       stage = q_to_stage(yval);

	    /* decide how many discharge decimal pts to print based on range */
	    if(range > 10.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.0f  Stg: %5.2f",
			  data->day_hrs[ixval], data->tracker_label, yval,
			  stage);
	    else if(range > 1.0 && range <= 10.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.1f  Stg: %5.2f",
			  data->day_hrs[ixval], data->tracker_label, yval,
			  stage);
	    else if(range > 0.0 && range <= 1.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.2f  Stg: %5.2f",
			  data->day_hrs[ixval], data->tracker_label, yval,
			  stage);
	 }
	 else
	 {
	    /* decide how many discharge decimal pts to print based on range */
	    if(range > 10.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.0f",
			  data->day_hrs[ixval], data->tracker_label, yval);
	    else if(range > 1.0 && range <= 10.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.1f",
			  data->day_hrs[ixval], data->tracker_label, yval);
	    else if(range > 0.0 && range <= 1.0)
	       xs_wprintf(data->mouse_rc, "Date: %s  %s: %4.2f",
			  data->day_hrs[ixval], data->tracker_label, yval);
	 }
	}

      else if(w == data->drawing_area_widget[3])
	{
	 zero = 0;

	 yval = pixel_to_val(&event->xbutton.y,
			     &data->ro_min,
			     &data->px_ro_y_axis_max,
			     &data->ro_origin_y,
			     &zero);
	 if(yval < 0.0) yval = 0.0;

	 xs_wprintf(data->mouse_rc, "Date: %s, Runoff: %4.2f",
		    data->day_hrs[ixval], yval);
	}

      else if(w == data->drawing_area_widget[1])
	{
	 /* local variable to store data->px_height (Dimension)
	    as an int to be passed to pixel_to_val - dp, 5/14/92
	 */
	 px_drawing_area_height = data->px_height;
	 yval = pixel_to_val(&event->xbutton.y,
			     &data->px_min,
			     &data->px_ro_y_axis_max,
			     &data->px_origin_y,
			     &px_drawing_area_height);
	 if(yval < 0.0) yval = 0.0;

	 xs_wprintf(data->mouse_rc, "Date: %s, Px / Rain+Melt: %4.2f",
		    data->day_hrs[ixval], yval);
	}
     }

   void clear_tracker(w, data, event)
      Widget      w;
      plot_cb_struct   *data;
      XEvent      *event;
	 {
         extern void xs_wprintf(Widget, char *, ...);
	 xs_wprintf(data->mouse_rc, " ");
	 }

    void xs_wprintf(Widget w, char *format, ...)
       {
	 va_list        args;
	 char           str[50];
	 XmString       xmstr;
	 Arg            wargs[5];
	 int            n;

	 va_start(args,format);

	 memset(str, '\0', 50);
/*       w = va_arg(args, Widget);    */

	 if(!XtIsSubclass(w, xmLabelWidgetClass))
	    {
	    printf("xs_wprintf() requires a Label Widget\n");
	    return;
	    XtError("xs_wprintf() requires a Label Widget");
	    }

/*       format = va_arg(args, char *); */
	 vsprintf(str, format, args);
	 xmstr = XmStringLtoRCreate(str, XmSTRING_DEFAULT_CHARSET);

	 n=0;
	 XtSetArg(wargs[n], XmNlabelString, xmstr); n++;
	 XtSetValues(w, wargs, n);

	 XmStringFree(xmstr);

	 va_end(args);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/mouse_fcns.c,v $";
 static char rcs_id2[] = "$Id: mouse_fcns.c,v 1.9 2006/02/13 21:33:33 dsa Exp $";}
/*  ===================================================  */

	 }
