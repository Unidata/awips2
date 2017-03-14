#include "rating_curve.h"
#include "libXifp.h"

/* File: create_rc_plot.c
 *
 * Creates the rating curve plot
 *
 */

Widget          global_toplevel;        /* top level widget structure */

typedef struct {
		Widget  top_help_shell;
		char    *widget_name;
	       }  help_cb_struct;       /* call back help structure */


Widget create_rc_plot(rc_data)
   rc_struct        *rc_data;           /* rating curve data sturcture pointer */
 {
   Arg                  wargs[10];         /* window resource data structure array */
   Widget               shell;             /* shell widget structure               */
   Widget               rc_main_drawing_area;   /* main drawing area widget        */
   Widget               rc_form;                /* rating curve form widget        */
   Widget               rc_y_axis_label;        /* y axis label widget             */
   Widget               rc_y_axis;              /* y axis label widget             */
   Widget               rc_x_axis_label;        /* x label widget                  */
   Widget               rc_x_axis;              /* x axis label widget             */
   Widget               rc_graph;               /* rating curve graph widget       */
   Widget               rc_horiz_scrollbar;     /* horizontal scrollbar widget     */
   Widget               rc_vert_scrollbar;      /* vertical scrollbar widget       */
   Widget               rc_horiz_scale_widget;  /* horizontal scale widget         */
   Widget               rc_vert_scale_widget;   /* vertical scale widget           */
   Widget               top_help_shell;         /* top help shell widget           */
   int                  n;                      /* window resource array index     */
   XGCValues            gcv;                    /* graphics context data structure */
   Pixel                foreground;             /* foreground color                */
   Pixel                background;             /* background color                */
   static help_cb_struct       *help_cb;        /* call back help structure        */
   int                  width_scale;            /* width scale factor              */
   int                  height_scale;           /* height scale factor             */

/*
 * Set pix and gc variables to NULL.
 */
   rc_data->pix[0] = (Pixmap)NULL;                /* y_axis_label   */
   rc_data->pix[1] = (Pixmap)NULL;                /* y_axis         */
   rc_data->pix[2] = (Pixmap)NULL;                /* x_axis         */
   rc_data->pix[3] = (Pixmap)NULL;                /* x_axis_label   */
   rc_data->pix[4] = (Pixmap)NULL;                /* graph          */

   rc_data->gc[0] = (GC)NULL;                /* y_axis_label   */
   rc_data->gc[1] = (GC)NULL;                /* y_axis         */
   rc_data->gc[2] = (GC)NULL;                /* x_axis         */
   rc_data->gc[3] = (GC)NULL;                /* x_axis_label   */
   rc_data->gc[4] = (GC)NULL;                /* graph          */

   rc_data->crosshairs_gc = (GC)NULL;        /* gc for crosshairs */
 /*
 * all drawing_area_widgets used in expose callbacks
 */
   rc_data->drawing_area_widget[0] = (Widget)NULL;  /* y_axis_label   */
   rc_data->drawing_area_widget[1] = (Widget)NULL;  /* y_axis         */
   rc_data->drawing_area_widget[2] = (Widget)NULL;  /* x_axis         */
   rc_data->drawing_area_widget[3] = (Widget)NULL;  /* x_axis_label   */
   rc_data->drawing_area_widget[4] = (Widget)NULL;  /* graph          */

 /*
 * all scrollbar and scale widgets used for zoom and roam
 */
   rc_data->rc_horiz_scrollbar_widget = (Widget)NULL; /* horiz scrollbar */
   rc_data->rc_vert_scrollbar_widget = (Widget)NULL;  /* vert scrollbar  */
   rc_data->rc_horiz_scale_widget = (Widget)NULL;     /* horiz scale     */
   rc_data->rc_vert_scale_widget = (Widget)NULL;      /* vert scale      */


   n=0;
   XtSetArg(wargs[n], XmNgeometry, "800x600+300+300"); n++;
   shell = XtCreatePopupShell("rating_curve_shell",
			      transientShellWidgetClass,
			      global_toplevel, wargs, n);


   top_help_shell = XtCreatePopupShell("top_help_shell",
					transientShellWidgetClass,
					shell, NULL, 0);

   n=0;
   XtSetArg(wargs[n], XmNminAspectX, 4); n++;
   XtSetArg(wargs[n], XmNmaxAspectX, 4); n++;
   XtSetArg(wargs[n], XmNminAspectY, 3); n++;
   XtSetArg(wargs[n], XmNmaxAspectY, 3); n++;
   XtSetArg(wargs[n], XmNtitle, "Rating Curve"); n++;
   XtSetValues(shell, wargs, n);

   n=0;
   rc_main_drawing_area = XtCreateManagedWidget("rc_main_drawing_area",
						xmDrawingAreaWidgetClass,
						shell, NULL, n);
/*
 * Create form widget to hold all other drawing areas and the legend
 */
   n=0;
   rc_form = XtCreateManagedWidget("rc_form",
				   xmFormWidgetClass,
				   rc_main_drawing_area, wargs, n);

   XtAddCallback(rc_main_drawing_area, XmNresizeCallback,
		 resize_rc_form_widget, rc_data);

   rc_data->form = rc_form;
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
   n=0;
   rc_y_axis_label = XtCreateManagedWidget("rc_y_axis_label",
					    xmDrawingAreaWidgetClass,
					    rc_form, wargs, n);

   help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

   help_cb->top_help_shell = top_help_shell;

   help_cb->widget_name = "rc_y_axis_label";

   XtAddEventHandler(rc_y_axis_label, EnterWindowMask, FALSE,
		     help_event_handler, help_cb);

   XtAddCallback(rc_y_axis_label, XmNresizeCallback,
		 resize_rc_y_axis_label, rc_data);

   XtAddCallback(rc_y_axis_label, XmNexposeCallback,
		 copy_one_rc_drawing_area, rc_data);

   rc_data->drawing_area_widget[0] = rc_y_axis_label;
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
   n=0;
   XtSetArg(wargs[n], XmNleftWidget, rc_y_axis_label); n++;
   rc_y_axis = XtCreateManagedWidget("rc_y_axis",
				     xmDrawingAreaWidgetClass,
				     rc_form, wargs, n);

   help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

   help_cb->top_help_shell = top_help_shell;

   help_cb->widget_name = "rc_y_axis";

   XtAddEventHandler(rc_y_axis, EnterWindowMask, FALSE,
		     help_event_handler, help_cb);

   XtAddCallback(rc_y_axis, XmNresizeCallback,
		 resize_rc_y_axis, rc_data);

   XtAddCallback(rc_y_axis, XmNexposeCallback,
		 copy_one_rc_drawing_area, rc_data);

   rc_data->drawing_area_widget[1] = rc_y_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
   n=0;
   /*XtSetArg(wargs[n], XmNtopWidget, rc_graph); n++;*/
   rc_x_axis = XtCreateManagedWidget("rc_x_axis",
				     xmDrawingAreaWidgetClass,
				     rc_form, wargs, n);

   help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

   help_cb->top_help_shell = top_help_shell;

   help_cb->widget_name = "rc_x_axis";

   XtAddEventHandler(rc_x_axis, EnterWindowMask, FALSE,
		     help_event_handler, help_cb);

   XtAddCallback(rc_x_axis, XmNresizeCallback,
		 resize_rc_x_axis, rc_data);

   XtAddCallback(rc_x_axis, XmNexposeCallback,
		 copy_one_rc_drawing_area, rc_data);

   rc_data->drawing_area_widget[2] = rc_x_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
   n=0;
   XtSetArg(wargs[n], XmNtopWidget, rc_x_axis); n++;
   rc_x_axis_label = XtCreateManagedWidget("rc_x_axis_label",
					   xmDrawingAreaWidgetClass,
					   rc_form, wargs, n);

   help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

   help_cb->top_help_shell = top_help_shell;

   help_cb->widget_name = "rc_x_axis_label";

   XtAddEventHandler(rc_x_axis_label, EnterWindowMask, FALSE,
		     help_event_handler, help_cb);

   XtAddCallback(rc_x_axis_label, XmNresizeCallback,
		 resize_rc_x_axis_label, rc_data);

   XtAddCallback(rc_x_axis_label, XmNexposeCallback,
		 copy_one_rc_drawing_area, rc_data);

   rc_data->drawing_area_widget[3] = rc_x_axis_label;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
   n=0;
   XtSetArg(wargs[n], XmNleftWidget, rc_y_axis); n++;
   rc_graph = XtCreateManagedWidget("rc_graph",
				     xmDrawingAreaWidgetClass,
				     rc_form, wargs, n);

   help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

   help_cb->top_help_shell = top_help_shell;

   help_cb->widget_name = "rc_graph";

   XtAddEventHandler(rc_graph, EnterWindowMask, FALSE,
		     help_event_handler, help_cb);

   XtAddCallback(rc_graph, XmNresizeCallback,
		 resize_rc_graph, rc_data);

   XtAddCallback(rc_graph, XmNexposeCallback,
		 copy_one_rc_drawing_area, rc_data);
/*
 * Add callback so we can quit from tulplot by typing 'q' or 'Q'
 *  in rc_graph drawing area
   XtAddCallback(rc_graph, XmNinputCallback,
		 exit_rc_cb, NULL);
 */

   rc_data->drawing_area_widget[4] = rc_graph;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
   n=0;
   XtSetArg(wargs[n], XmNlabelString,
	    XmStringCreate(rc_data->rc_id, XmSTRING_DEFAULT_CHARSET));
   n++;
   rc_data->rc_id_widget = XtCreateManagedWidget("rc_id",
						 xmLabelWidgetClass,
						 rc_form, wargs, n);

   help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

   help_cb->top_help_shell = top_help_shell;

   help_cb->widget_name = "rc_id";

   XtAddEventHandler(rc_data->rc_id_widget, EnterWindowMask, FALSE,
		     help_event_handler, help_cb);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/*
 * Set initial value of vertical slider to max - slider_size
 *  so that if plot is originally defined with a height scale > 1
 *  we see the bottom portion of the plot.
 * The actual number doesn't matter here, just that
 *  value + sliderSize = max.
 */
  n=0;
  XtSetArg(wargs[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(wargs[n], XmNmaximum, 100); n++;
  XtSetArg(wargs[n], XmNsliderSize, 10); n++;
  XtSetArg(wargs[n], XmNvalue, 90); n++;

  rc_vert_scrollbar = XtCreateManagedWidget("rc_vert_scrollbar",
					     xmScrollBarWidgetClass,
					     rc_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "rc_vert_scrollbar";

  XtAddEventHandler(rc_vert_scrollbar, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(rc_vert_scrollbar, XmNvalueChangedCallback,
		rc_vert_scrollbar_moved, rc_data);
  XtAddCallback(rc_vert_scrollbar, XmNdragCallback,
		rc_vert_scrollbar_moved, rc_data);
  XtAddCallback(rc_vert_scrollbar, XmNtoTopCallback,
		rc_vert_scrollbar_moved, rc_data);
  XtAddCallback(rc_vert_scrollbar, XmNtoBottomCallback,
		rc_vert_scrollbar_moved, rc_data);

  rc_data->rc_vert_scrollbar_widget = rc_vert_scrollbar;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, rc_x_axis_label); n++;
  XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(wargs[n], XmNvalue, 0); n++;

  rc_horiz_scrollbar = XtCreateManagedWidget("rc_horiz_scrollbar",
					     xmScrollBarWidgetClass,
					     rc_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "rc_horiz_scrollbar";

  XtAddEventHandler(rc_horiz_scrollbar, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(rc_horiz_scrollbar, XmNvalueChangedCallback,
		rc_horiz_scrollbar_moved, rc_data);
  XtAddCallback(rc_horiz_scrollbar, XmNdragCallback,
		rc_horiz_scrollbar_moved, rc_data);
  XtAddCallback(rc_horiz_scrollbar, XmNtoTopCallback,
		rc_horiz_scrollbar_moved, rc_data);
  XtAddCallback(rc_horiz_scrollbar, XmNtoBottomCallback,
		rc_horiz_scrollbar_moved, rc_data);

  rc_data->rc_horiz_scrollbar_widget = rc_horiz_scrollbar;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  width_scale = 1;
  rc_data->width_scale = width_scale;
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, rc_horiz_scrollbar); n++;
  XtSetArg(wargs[n], XmNvalue, width_scale); n++;
  XtSetArg(wargs[n], XmNtitleString,
	   XmStringCreate("Horizontal Scale",
			  XmSTRING_DEFAULT_CHARSET)); n++;

  rc_horiz_scale_widget = XtCreateManagedWidget("rc_horiz_scale_widget",
						xmScaleWidgetClass,
						rc_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "rc_horiz_scale_widget";

  XtAddEventHandler(rc_horiz_scale_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(rc_horiz_scale_widget, XmNvalueChangedCallback,
		rc_scale_width_changed, rc_data);

  rc_data->rc_horiz_scale_widget = rc_horiz_scale_widget;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  height_scale = 1;
  rc_data->height_scale = height_scale;
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, rc_horiz_scrollbar); n++;
  XtSetArg(wargs[n], XmNvalue, height_scale); n++;
  XtSetArg(wargs[n], XmNtitleString,
	   XmStringCreate("Vertical Scale",
			  XmSTRING_DEFAULT_CHARSET)); n++;

  rc_vert_scale_widget = XtCreateManagedWidget("rc_vert_scale_widget",
						xmScaleWidgetClass,
						rc_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "rc_vert_scale_widget";

  XtAddEventHandler(rc_vert_scale_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(rc_vert_scale_widget, XmNvalueChangedCallback,
		rc_scale_height_changed, rc_data);

  rc_data->rc_vert_scale_widget = rc_vert_scale_widget;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

  XtRealizeWidget(shell);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

  rc_create_mouse_tracker(rc_form, rc_graph, rc_data, help_cb,
			  top_help_shell);

  XSelectInput(XtDisplay(shell), XtWindow(shell),
	       StructureNotifyMask);

  XSelectInput(XtDisplay(shell),
		DefaultRootWindow(XtDisplay(shell)),
				  PropertyChangeMask);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/*
 * Add event handler for crosshairs in mods plot.
 * Note that this event handler is disabled when in
 *  change mode (i.e., when rubberbanding is on).
 */

/* Set up graphics context for crosshairs */
  n = 0;
  XtSetArg(wargs[n], XtNbackground,  &gcv.background);n++;
  XtGetValues(rc_graph, wargs, n);
  gcv.foreground = get_pixel_by_name(rc_graph, "white")
				     ^ gcv.background;
  gcv.function = GXxor;
  rc_data->crosshairs_gc = XCreateGC(XtDisplay(rc_graph),
			     XtWindow(rc_graph),
			     GCFunction | GCBackground | GCForeground,
			     &gcv);

  XGrabButton(XtDisplay(rc_graph), AnyButton, AnyModifier,
	      XtWindow(rc_graph), TRUE,
	      ButtonPressMask | ButtonMotionMask |
	      ButtonReleaseMask,
	      GrabModeAsync, GrabModeAsync,
	      XtWindow(rc_graph),
	      XCreateFontCursor(XtDisplay(rc_graph),
				XC_crosshair));


  XtAddEventHandler(rc_data->drawing_area_widget[4],
		    ButtonPressMask, FALSE, rc_crosshairs_timer, rc_data);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */



   XtPopup(shell, XtGrabNone);

   return(shell);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/create_rc_plot.c,v $";
 static char rcs_id2[] = "$Id: create_rc_plot.c,v 1.2 2006/03/28 20:43:35 aivo Exp $";}
/*  ===================================================  */

}
