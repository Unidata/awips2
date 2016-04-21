/* File: tulplot.c
 *
 *  This is a test program to create the graphical Tulsa plot
 *     using Motif widgets.
 */

#include <math.h>
#include "cex25.h"
#include "plot.h"
#include "ifp_atoms.h"
#include "ifp_struct.h"
#include "libXifp.h"
#include "mod_struct.h"
#include "libXs.h"
#include "ifp_globals.h"
#include "c_call_f/fdcode.h"
#include "c_call_f/fconvt.h"
#include "c_call_f/get_segment_description.h"

extern void assign_ts_colors(plot_cb_struct*, char*[], TS_INFO*);
extern void find_plot_delta_t(plot_cb_struct *, TS_INFO *);
/*extern int get_pixel_by_name(Widget, char *);*/

typedef struct {
		Widget  top_help_shell;
		char    *widget_name;
	       } help_cb_struct;

int get_river_name(char operation_name[9],char river_name[21] );
void tulplot(ts_array, tsid, plot_symbol, day_hrs, plot_mask,
	     obs_mask, num_ts, passed_num_pts, orig_ts_array, myt, rc_data,
	     cb_data, ptm_data, minimum_maximum_discharge, ts_info, nbase, operation_name)

   int          *plot_mask, *obs_mask;  /* values determine to  plot index */
   int          *num_ts;   /* pointer to Number of time series used in the current forcast program */
   int          passed_num_pts;  /* number of passed points */
   float        **ts_array;      /* Address of pointer to time series array  */
   float        **orig_ts_array; /* Address of pointer to original time */
   char         **tsid;          /* Address of pointer to time series id */
   char         *plot_symbol[];  /* plot symbol array pointer */
   char         **day_hrs;
   char         *myt;            /* month, year, time zone pointer */
   rc_struct    *rc_data;        /* rating curve data pointer */
   plot_cb_struct  *cb_data;     /* call back data pointer */
   combined_struct *ptm_data;    /* tables and plot data structure pointer */
   float        minimum_maximum_discharge;
   TS_INFO      *ts_info;        /* Pointer to time series information structure */
   float        nbase;           /* y axis base numeric value */
   char         operation_name[9];
{
  Widget        main_plot_shell, main_drawing_area, form;
  Widget        px_y_axis, px_x_axis;
  Widget        ro_y_axis, ro_x_axis;
  Widget        discharge_axis, hydrograph, stage_axis;
  Widget        x_axis;
  Widget        horiz_scrollbar, vertical_scrollbar;
  Widget        horiz_scale_widget, vertical_scale_widget;
  Widget        legend_rc;
  Widget        top_help_shell;
  Widget        change_vert_axis_max_widget;
  int           i, j, n;
  static int    max_points, width_scale, height_scale;
  Arg           wargs[10];              /* window resource data structure array */
  static help_cb_struct       *help_cb; /* help call back data structure pointer */
  static float  discharge_axis_max;
  int           argc;
  char          *argv[1];
  static char   *colors[] = {"yellow", "magenta", "sky blue",
			     "lime green", "salmon", "cyan", "green"};
  static int    num_plotted_ts;  /* number of plotted time series points */
  static float  min_x, max_x, min_y, max_y;
  float         max_y_test;
  GC            graph_gc;
  XGCValues     graph_gcv;
  static int    plot_index[MAX_TS];

  float         discharge_increment;

  static int    num_pts;
  char          px_ro_units[7];
  char          legend_name[20];
  char          plot_name[21];
  char          main_shell_name[49];

  char          type_string[4], std_units[4], dimen[4], time_scale[4];
  int           missing_allowed;
  int           nv_dt;   /* number of values per time interval for this data type */
  int           nadd;    /* Number of pieces of additional information */
  int           err_flag; /* Error flag, 0 no error, 1 indicates not a valid data type */
  char          std_Eng_units[4];
  float         mult_conver_factor, add_constant;
  Dimension     width, height;
  char          river_name[21];

  num_pts = passed_num_pts; 

  for (j = 0;j < *num_ts; j++)
  {
/*   printf(" in tulplot.c j cb_data->end[j] = %d %d\n",j,cb_data->end[j]); */
     if ((plot_mask[j] == PLOT || obs_mask[j] == PLOT) && 
        (num_pts < cb_data->end[j]))
     {
        num_pts = cb_data->end[j];
     }
  } 
/*printf(" in tulplot.c num_pts = %d\n",num_pts); */
  cb_data->num_pts = &num_pts;
  max_y = 0.0;
  min_y = 0.0;

  cb_data->ts_index = 0;
  cb_data->num_ts = *num_ts;

  max_points = MAX_POINTS;
  width_scale = WIDTH_SCALE;
  height_scale = HEIGHT_SCALE;

  if(max_points < 2)     max_points = 2;
  if(max_points > 2000)  max_points = 2000;
  if(width_scale < 1) width_scale = 1;
  if(width_scale > 5) width_scale = 5;
  if(height_scale < 1) height_scale = 1;
  if(height_scale > 5) height_scale = 5;

/* Create an array to hold the index of each plotted time series
   in the whole data array (ts_array) and get count of the
   number of plotted time series. */

      for(i = 0; i < MAX_TS; i++)
	  plot_index[i] = 0;
	  
      num_plotted_ts = 0;
      j = 0;
      for(i = 0; i < *num_ts; i++)
	 if (plot_mask[i] == PLOT || obs_mask[i] == PLOT)
	 {
	    plot_index[j] = i;
	    j++;
	    num_plotted_ts = j;
	 }

/*  Malloc space for pointers in the line_args and axes_args structures.  */
      cb_data->ts_name = (char **)malloc((num_plotted_ts) *
			 sizeof(char *));
      cb_data->ts_color = (char **)malloc((num_plotted_ts) *
			   sizeof(char *));
      cb_data->ts_symbol = (char **)malloc((num_plotted_ts) *
			   sizeof(char *));
      cb_data->y = (float **)malloc((num_plotted_ts) * sizeof(float *));
      cb_data->x = (float *)malloc((num_pts) * sizeof(float));
      for(i = 0; i < num_plotted_ts; i++)
      {
	 cb_data->ts_name[i] = (char*)malloc(sizeof(char)*20);
	 cb_data->ts_color[i] = (char*)malloc(sizeof(char)*20);
	 cb_data->ts_symbol[i] = (char*)malloc(sizeof(char));
      }
      cb_data->legend = (Widget*)malloc(num_plotted_ts*sizeof(Widget));

/*  Assign values for cb_data arrays.  */
      for(i = 0; i < num_plotted_ts; i++)
      {
	 strcpy(cb_data->ts_name[i], tsid[plot_index[i]]);
	 strncat(cb_data->ts_name[i], " (", 2);
	 strncat(cb_data->ts_name[i], plot_symbol[plot_index[i]], 1);
         
	 strncat(cb_data->ts_name[i], ")", 1);
	 /*cb_data->ts_color[i] = ts_color[plot_index[i]];*/
	 cb_data->ts_symbol[i] = plot_symbol[plot_index[i]];
	 cb_data->line_gc[i] = NULL;
	 if(obs_mask[plot_index[i]] == PLOT)
         {
	    /*cb_data->end[i] = end_obs+1;*/
	    /*cb_data->end[i] = num_pts;*/
         }
	 else
         {
	    /*cb_data->end[i] = num_pts;*/
         }
	 cb_data->y[i] = ts_array[plot_index[i]];
	 
	 /* add code to figure out min_y also - make sure to check for
	  * missing values (-999) and ignore them.   dp 3 Mar. 98 
	  */
	 for (j = 0; j < cb_data->end[plot_index[i]] - 1; j++)
	 {
	    if(cb_data->y[i][j] > max_y) max_y = cb_data->y[i][j];
	    if(cb_data->y[i][j] < min_y && 
	       (cb_data->y[i][j] > -998.99 || cb_data->y[i][j] < -999.01)) 
	         min_y = cb_data->y[i][j];
	 }
      }

    /* find max and min of px and ro ts */
      cb_data->px_max = cb_data->px_min = 0;
      cb_data->ro_max = cb_data->ro_min = 0;
      
      for (i=0; i<cb_data->num_rr_oper; i++)
	 for (j=0; j<cb_data->num_rr_pts; j++)
	 {
	    if(cb_data->px[i][j] > cb_data->px_max)
	       cb_data->px_max = cb_data->px[i][j];
            
	    if(cb_data->ro[i][j] > cb_data->ro_max)
            {
	       cb_data->ro_max = cb_data->ro[i][j];
               /*AV linux debug */
               
            }
	 }

      /* set max and min for x axis */
      for(i = 0; i < num_pts; i++)
	cb_data->x[i] = (float)i;

      min_x = 0.0;
      max_x = (float)(*cb_data->num_pts-1);

      /* set max and min for y axis */
      max_y_test = (minimum_maximum_discharge > max_y) ?
		    minimum_maximum_discharge : max_y;

      if((int)nbase > 0)
      {
	 if(NWSRFS_general_units == 0)  /* English units */
	    min_y = (int)nbase;
	 else                           /* metric units */
	 {
	    strncpy(type_string, cb_data->disch_label_data_type, 4);
	    FDCODE(type_string, std_units, dimen, &missing_allowed, &nv_dt,
		   time_scale, &nadd, &err_flag);
	    FCONVT(std_units, dimen, std_Eng_units, &mult_conver_factor,
		   &add_constant, &err_flag);

	    min_y = (int)((nbase - add_constant) / mult_conver_factor);
	 }
      }
      
      /* Set the discharge_axis_max and increment for plotting y axis
	 values */
      scale_max_min(min_y, max_y_test, minimum_maximum_discharge,
		    &min_y, &discharge_axis_max, &discharge_increment);

      cb_data->rc_data = rc_data;
      cb_data->min_x = &min_x;
      cb_data->max_x = &max_x;
      cb_data->min_y = &min_y;
      cb_data->max_y = &max_y;
      cb_data->discharge_axis_max = &discharge_axis_max;
      cb_data->default_discharge_axis_max = discharge_axis_max;
      cb_data->discharge_increment = discharge_increment;
      cb_data->start_end_sw = START;
      cb_data->ts_change_flag = START;
      cb_data->ipt = 1000;
      cb_data->num_plotted_ts = &num_plotted_ts;
      cb_data->day_hrs = day_hrs;
      cb_data->plot_mask = plot_mask;
      cb_data->plot_index = plot_index;
      cb_data->obs_mask = obs_mask;
      cb_data->orig_ts_array = orig_ts_array;
      cb_data->ts_array = ts_array;

      assign_ts_colors(cb_data, colors, ts_info);
      
      
      /* call routine to figure out the time step of the plot
       * dp - 13 Jan. 1998
       */
      find_plot_delta_t(cb_data, ts_info);      


/*
 * Fill cb_data structure.
 */
  cb_data->pix[0] = (Pixmap)NULL;                /* px_y_axis         */
  cb_data->pix[1] = (Pixmap)NULL;                /* px_x_axis         */
  cb_data->pix[2] = (Pixmap)NULL;                /* ro_y_axis         */
  cb_data->pix[3] = (Pixmap)NULL;                /* ro_x_axis         */
  cb_data->pix[4] = (Pixmap)NULL;                /* discharge_axis    */
  cb_data->pix[5] = (Pixmap)NULL;                /* hydrograph        */
  cb_data->pix[6] = (Pixmap)NULL;                /* stage_axis        */
  cb_data->pix[7] = (Pixmap)NULL;                /* x_axis            */
  cb_data->px_label_pix = (Pixmap)NULL;
  cb_data->ro_label_pix = (Pixmap)NULL;
  cb_data->discharge_label_pix = (Pixmap)NULL;
  cb_data->stage_label_pix = (Pixmap)NULL;

  cb_data->gc[0] = (GC)NULL;                 /* px_y_axis         */
  cb_data->gc[1] = (GC)NULL;                 /* px_x_axis         */
  cb_data->gc[2] = (GC)NULL;                 /* ro_y_axis         */
  cb_data->gc[3] = (GC)NULL;                 /* ro_x_axis         */
  cb_data->gc[4] = (GC)NULL;                 /* discharge_axis    */
  cb_data->gc[5] = (GC)NULL;                 /* hydrograph        */
  cb_data->gc[6] = (GC)NULL;                 /* stage_axis        */
  cb_data->gc[7] = (GC)NULL;                 /* x_axis            */
  cb_data->px_label_gc = (GC)NULL;
  cb_data->ro_label_gc = NULL;
  cb_data->discharge_label_gc = (GC)NULL;
  cb_data->stage_label_gc = (GC)NULL;
/*
 * drawing_area_widget will be used for px_x_axis,
 *   ro_x_axis, and hydrograph valueChanged callbacks
 *   from the horizontal scale widget
 *
 * drawing_area_widget will be used for
 *   discharge_axis, hydrograph, and stage_axis
 *   valueChanged callbacksfrom the vertical scale widget
 *
 * all drawing_area_widgets used in expose callbacks
 */
  cb_data->drawing_area_widget[0] = (Widget)NULL;  /* px_y_axis         */
  cb_data->drawing_area_widget[1] = (Widget)NULL;  /* px_x_axis         */
  cb_data->drawing_area_widget[2] = (Widget)NULL;  /* ro_y_axis         */
  cb_data->drawing_area_widget[3] = (Widget)NULL;  /* ro_x_axis         */
  cb_data->drawing_area_widget[4] = (Widget)NULL;  /* discharge_axis    */
  cb_data->drawing_area_widget[5] = (Widget)NULL;  /* hydrograph        */
  cb_data->drawing_area_widget[6] = (Widget)NULL;  /* stage_axis        */
  cb_data->drawing_area_widget[7] = (Widget)NULL;  /* x_axis            */
  cb_data->px_label_da_widget = (Widget)NULL;
  cb_data->ro_label_da_widget = (Widget)NULL;
  cb_data->discharge_label_da_widget = (Widget)NULL;
  cb_data->stage_label_da_widget = (Widget)NULL;
/*
 * horiz_scrollbar_widget will be used for px_x_axis,
 *   ro_x_axis, hydrograph, and x_axis expose callbacks
 */
  cb_data->horiz_scrollbar_widget = (Widget)NULL;
/*
 * vertical_scrollbar_widget will be used for
 *   discharge_axis, hydrograph, and stage_axis
 *   expose callbacks
 */
  cb_data->vertical_scrollbar_widget = (Widget)NULL;
/*
 * legend row column widget to hold time series labels
 */
  cb_data->legend_rc = (Widget)NULL;
/*
 * undo button to reset time series values
 */
  cb_data->undo_widget = (Widget)NULL;
/*
 * row column widget to hold labels with mouse position
 */
  cb_data->mouse_rc = (Widget)NULL;
/*
 * labels to hold mouse location value
 */
  cb_data->mouse_date = NULL;
  cb_data->mouse_discharge = NULL;
  cb_data->mouse_stage = NULL;
/*
 * label to hold current month and year
 */
  cb_data->month_year_widget = NULL;
/*
 * max_points will only be used for px_x_axis,
 *   ro_x_axis, hydrograph, and x_axis expose callbacks
 */
  cb_data->max_points = max_points;
/*
 * width_scale will be used for px_x_axis,
 *   ro_x_axis, hydrograph, and x_axis expose callbacks
 */
  cb_data->width_scale = width_scale;
/*
 * height_scale will be used for
 *   discharge_axis, hydrograph, and stage_axis
 *   expose callbacks
 */
  cb_data->height_scale = height_scale;
/*
 * Create toplevel shell widget
 */

  n=0;
  XtSetArg(wargs[n], XmNgeometry, "800x600+200+200"); n++;
  main_plot_shell = XtCreateApplicationShell("main_plot_shell",
				transientShellWidgetClass,
				wargs, n);
 
  cb_data->main_plot_shell = main_plot_shell;

  top_help_shell = XtCreatePopupShell("top_help_shell",
				transientShellWidgetClass,
				main_plot_shell, NULL, 0);

  memset(main_shell_name, '\0', 49);
  strcpy(main_shell_name, "IFP Plot:  ");
  if(cb_data->rc_data->RC == TRUE)
     strcat(main_shell_name, cb_data->rc_data->rc_station_name);
  else
  {
     memset(plot_name, '\0', 21);
     GET_SEGMENT_DESCRIPTION(plot_name);     
     strcat(main_shell_name, plot_name);
  } 
  strcat(main_shell_name," - ");
  strcat(main_shell_name, operation_name);
  
  /* MR 562 - insert river name to the tile of the plot */
  if(get_river_name(ptm_data->seg_name,river_name))
	{
 	/*printf("RIVER NAME = %s\n", river_name);*/
  	strcat(main_shell_name," - ");
  	strcat(main_shell_name,river_name);
	}
  else
 	printf("RIVER NAME = NOT FOUND\n");

  n=0;
  XtSetArg(wargs[n], XmNminAspectX, 4); n++;
  XtSetArg(wargs[n], XmNmaxAspectX, 4); n++;
  XtSetArg(wargs[n], XmNminAspectY, 3); n++;
  XtSetArg(wargs[n], XmNmaxAspectY, 3); n++;
  XtSetArg(wargs[n], XmNtitle, main_shell_name); n++;
  XtSetValues(main_plot_shell, wargs, n);

  main_drawing_area = XtCreateManagedWidget("main_drawing_area",
			xmDrawingAreaWidgetClass,
			main_plot_shell, NULL, 0);
/*
 * Create form widget to hold all other drawing areas and the legend
 */
  form = XtCreateManagedWidget("form",
	   xmFormWidgetClass,
	   main_drawing_area, NULL, 0);

  XtAddCallback(main_drawing_area, XmNresizeCallback,
		resize_form_widget, form);
/*
 * Create each of the drawing areas and
 *  the row/column widget for the legend
 */
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  cb_data->px_label_da_widget = XtCreateManagedWidget("px_label",
						     xmDrawingAreaWidgetClass,
						     form, NULL, 0);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "px_label";

  XtAddEventHandler(cb_data->px_label_da_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(cb_data->px_label_da_widget, XmNresizeCallback,
		resize_px_label, cb_data);

  XtAddCallback(cb_data->px_label_da_widget, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNleftWidget, cb_data->px_label_da_widget); n++;
  px_y_axis = XtCreateManagedWidget("px_y_axis",
		xmDrawingAreaWidgetClass,
		form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "px_y_axis";

  XtAddEventHandler(px_y_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(px_y_axis, XmNresizeCallback,
		resize_px_y_axis, ptm_data);

  XtAddCallback(px_y_axis, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

  cb_data->drawing_area_widget[0] = px_y_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNleftWidget, px_y_axis); n++;
  px_x_axis = XtCreateManagedWidget("px_x_axis",
				    xmDrawingAreaWidgetClass,
				    form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "px_x_axis";

  XtAddEventHandler(px_x_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(px_x_axis, XmNresizeCallback,
		resize_px_x_axis, ptm_data);

  XtAddCallback(px_x_axis, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

  cb_data->drawing_area_widget[1] = px_x_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, cb_data->px_label_da_widget); n++;
  cb_data->ro_label_da_widget = XtCreateManagedWidget("ro_label",
						     xmDrawingAreaWidgetClass,
						     form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "ro_label";

  XtAddEventHandler(cb_data->ro_label_da_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(cb_data->ro_label_da_widget, XmNresizeCallback,
		resize_ro_label, cb_data);

  XtAddCallback(cb_data->ro_label_da_widget, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, px_y_axis); n++;
  XtSetArg(wargs[n], XmNleftWidget, cb_data->ro_label_da_widget); n++;
  ro_y_axis = XtCreateManagedWidget("ro_y_axis",
		xmDrawingAreaWidgetClass,
		form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "ro_y_axis";

  XtAddEventHandler(ro_y_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(ro_y_axis, XmNresizeCallback,
		resize_ro_y_axis, ptm_data);

  XtAddCallback(ro_y_axis, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

  cb_data->drawing_area_widget[2] = ro_y_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, px_x_axis); n++;
  XtSetArg(wargs[n], XmNleftWidget, ro_y_axis); n++;
  ro_x_axis = XtCreateManagedWidget("ro_x_axis",
		xmDrawingAreaWidgetClass,
		form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "ro_x_axis";

  XtAddEventHandler(ro_x_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(ro_x_axis, XmNresizeCallback,
		resize_ro_x_axis, ptm_data);

  XtAddCallback(ro_x_axis, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

  cb_data->drawing_area_widget[3] = ro_x_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, cb_data->ro_label_da_widget); n++;
  cb_data->discharge_label_da_widget = XtCreateManagedWidget("discharge_label",
							    xmDrawingAreaWidgetClass,
							    form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "discharge_label";

  XtAddEventHandler(cb_data->discharge_label_da_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(cb_data->discharge_label_da_widget, XmNresizeCallback,
		resize_discharge_label, cb_data);

  XtAddCallback(cb_data->discharge_label_da_widget, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, ro_y_axis); n++;
  XtSetArg(wargs[n], XmNleftWidget, cb_data->discharge_label_da_widget); n++;
  discharge_axis = XtCreateManagedWidget("discharge_axis",
		     xmDrawingAreaWidgetClass,
		     form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "discharge_axis";

  XtAddEventHandler(discharge_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(discharge_axis, XmNresizeCallback,
		resize_discharge_axis, ptm_data);

  XtAddCallback(discharge_axis, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

  cb_data->drawing_area_widget[4] = discharge_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, ro_x_axis); n++;
  XtSetArg(wargs[n], XmNleftWidget, discharge_axis); n++;
  hydrograph = XtCreateManagedWidget("hydrograph",
				     xmDrawingAreaWidgetClass,
				     form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "hydrograph";

  XtAddEventHandler(hydrograph, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(hydrograph, XmNresizeCallback,
		resize_hydrograph, ptm_data);

  XtAddCallback(hydrograph, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);
/*
 * Add callback so we can quit from tulplot by typing 'q' or 'Q'
 *  in hydrograph drawing area - gfs, 6/21/91
 */
  XtAddCallback(hydrograph, XmNinputCallback,
		exit_tulplot_cb, NULL);

  cb_data->drawing_area_widget[5] = hydrograph;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, hydrograph); n++;
  x_axis = XtCreateManagedWidget("x_axis",
		     xmDrawingAreaWidgetClass,
		     form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "x_axis";

  XtAddEventHandler(x_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(x_axis, XmNresizeCallback,
		resize_x_axis, ptm_data);

  XtAddCallback(x_axis, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

  cb_data->drawing_area_widget[7] = x_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/*
 * Set initial value of vertical slider to max - slider_size
 *  so that if plot is originally defined with a height scale > 1
 *  we see the bottom portion of the plot.
 * The actual number don't matter here, just that
 *  value + sliderSize = max.
 */
  n=0;
  XtSetArg(wargs[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(wargs[n], XmNmaximum, 100); n++;
  XtSetArg(wargs[n], XmNsliderSize, 10); n++;
  XtSetArg(wargs[n], XmNvalue, 90); n++;

  vertical_scrollbar = XtCreateManagedWidget("vertical_scrollbar",
					     xmScrollBarWidgetClass,
					     form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "vertical_scrollbar";

  XtAddEventHandler(vertical_scrollbar, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(vertical_scrollbar, XmNvalueChangedCallback,
		plot_vertical_scrollbar_moved, cb_data);
  XtAddCallback(vertical_scrollbar, XmNdragCallback,
		plot_vertical_scrollbar_moved, cb_data);
  XtAddCallback(vertical_scrollbar, XmNtoTopCallback,
		plot_vertical_scrollbar_moved, cb_data);
  XtAddCallback(vertical_scrollbar, XmNtoBottomCallback,
		plot_vertical_scrollbar_moved, cb_data);

  cb_data->vertical_scrollbar_widget = vertical_scrollbar;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNleftWidget, hydrograph); n++;
  stage_axis = XtCreateManagedWidget("stage_axis",
				     xmDrawingAreaWidgetClass,
				     form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "stage_axis";

  XtAddEventHandler(stage_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(stage_axis, XmNresizeCallback,
		resize_stage_axis, ptm_data);

  XtAddCallback(stage_axis, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

  cb_data->drawing_area_widget[6] = stage_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNleftWidget, stage_axis); n++;
  XtSetArg(wargs[n], XmNrightWidget, vertical_scrollbar); n++;
  cb_data->stage_label_da_widget = XtCreateManagedWidget("stage_label",
							 xmDrawingAreaWidgetClass,
							 form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "stage_label";

  XtAddEventHandler(cb_data->stage_label_da_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(cb_data->stage_label_da_widget, XmNresizeCallback,
		resize_stage_label, cb_data);

  XtAddCallback(cb_data->stage_label_da_widget, XmNexposeCallback,
		copy_one_tulplot_drawing_area, cb_data);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, x_axis); n++;
  XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(wargs[n], XmNvalue, 0); n++;

  horiz_scrollbar = XtCreateManagedWidget("horiz_scrollbar",
		xmScrollBarWidgetClass,
		form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "horiz_scrollbar";

  XtAddEventHandler(horiz_scrollbar, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(horiz_scrollbar, XmNvalueChangedCallback,
		plot_horiz_scrollbar_moved, cb_data);
  XtAddCallback(horiz_scrollbar, XmNdragCallback,
		plot_horiz_scrollbar_moved, cb_data);
  XtAddCallback(horiz_scrollbar, XmNtoTopCallback,
		plot_horiz_scrollbar_moved, cb_data);
  XtAddCallback(horiz_scrollbar, XmNtoBottomCallback,
		plot_horiz_scrollbar_moved, cb_data);

  cb_data->horiz_scrollbar_widget = horiz_scrollbar;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, horiz_scrollbar); n++;
  legend_rc = XtCreateManagedWidget("legend_rc",
				    xmRowColumnWidgetClass,
				    form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "legend_rc";

  XtAddEventHandler(legend_rc, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  cb_data->legend_rc = legend_rc;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  for(i=0; i<num_plotted_ts; i++)
  {
     n=0;
     XtSetArg(wargs[n], XmNbackground, get_pixel_by_name(legend_rc,
	      "gray85")); n++;
     XtSetArg(wargs[n], XmNforeground, get_pixel_by_name(legend_rc,
	      cb_data->ts_color[i])); n++;
     cb_data->legend[i] = XtCreateManagedWidget(cb_data->ts_name[i],
				      xmPushButtonWidgetClass,
				      legend_rc, wargs, n);
     XtAddCallback(cb_data->legend[i], XmNactivateCallback, change_ts,
		   ptm_data);
     help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

     help_cb->top_help_shell = top_help_shell;

     help_cb->widget_name = "legend_rc";

     XtAddEventHandler(cb_data->legend[i], EnterWindowMask, FALSE,
		       help_event_handler, help_cb);
  }

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNheight,    50);n++;
  XtSetArg(wargs[n], XmNtopWidget, horiz_scrollbar); n++;
  XtSetArg(wargs[n], XmNrightWidget, legend_rc); n++;
  XtSetArg(wargs[n], XmNvalue, width_scale); n++;
  XtSetArg(wargs[n], XmNtitleString,
	   XmStringCreate("Horizontal Scale",
			  XmSTRING_DEFAULT_CHARSET)); n++;

  horiz_scale_widget = XtCreateManagedWidget("horiz_scale_widget",
					     xmScaleWidgetClass,
					     form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "horiz_scale_widget";

  XtAddEventHandler(horiz_scale_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(horiz_scale_widget, XmNvalueChangedCallback,
		scale_width_changed, ptm_data);

  cb_data->horiz_scale_widget = horiz_scale_widget;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNheight,    50);n++;
  XtSetArg(wargs[n], XmNtopWidget, horiz_scrollbar); n++;
  XtSetArg(wargs[n], XmNleftWidget, legend_rc); n++;
  XtSetArg(wargs[n], XmNvalue, height_scale); n++;
  XtSetArg(wargs[n], XmNtitleString,
	   XmStringCreate("Vertical Scale",
			  XmSTRING_DEFAULT_CHARSET)); n++;

  vertical_scale_widget = XtCreateManagedWidget("vertical_scale_widget",
						xmScaleWidgetClass,
						form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "vertical_scale_widget";

  XtAddEventHandler(vertical_scale_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(vertical_scale_widget, XmNvalueChangedCallback,
		scale_height_changed, ptm_data);

  cb_data->vertical_scale_widget = vertical_scale_widget;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
/*  XtSetArg(wargs[n], XmNtopWidget, stage_axis); n++;*/
  XtSetArg(wargs[n], XmNleftWidget, horiz_scrollbar); n++;
  XtSetArg(wargs[n], XmNvalue, 1); n++;
  XtSetArg(wargs[n], XmNtitleString,
	   XmStringCreate("y_max",
			  XmSTRING_DEFAULT_CHARSET)); n++;

  change_vert_axis_max_widget = XtCreateManagedWidget("change_vert_axis_max_widget",
						      xmScaleWidgetClass,
						      form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "change_vert_axis_max_widget";

  XtAddEventHandler(change_vert_axis_max_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(change_vert_axis_max_widget, XmNvalueChangedCallback,
		change_vert_axis_max, ptm_data);

  cb_data->change_vert_axis_max_widget = change_vert_axis_max_widget;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n = 0;
  cb_data->undo_widget = XtCreateManagedWidget("undo_widget",
						xmPushButtonWidgetClass,
						form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "undo_widget";

  XtSetSensitive(cb_data->undo_widget, FALSE);
  XtAddCallback(cb_data->undo_widget, XmNactivateCallback,
		undo_change_ts, ptm_data);


  XtAddEventHandler(cb_data->undo_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNlabelString,
	   XmStringCreate(myt, XmSTRING_DEFAULT_CHARSET)); n++;
  cb_data->month_year_widget = XtCreateManagedWidget("month_year",
						    xmLabelWidgetClass,
						    form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "month_year";

  XtAddEventHandler(cb_data->month_year_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  memset(px_ro_units, '\0', 7);
  if(NWSRFS_general_units == 0)
     strncpy(px_ro_units, "inches", 6);
  else
     strncpy(px_ro_units, "mm", 2);

  n=0;
  XtSetArg(wargs[n], XmNlabelString,
	   XmStringCreate(px_ro_units, XmSTRING_DEFAULT_CHARSET)); n++;
  XtSetArg(wargs[n], XmNleftWidget, px_x_axis); n++;
  cb_data->px_ro_units_widget = XtCreateManagedWidget("px_ro_units",
						    xmLabelWidgetClass,
						    form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "px_ro_units";

  XtAddEventHandler(cb_data->px_ro_units_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/* cew core dump here */
  
   XtRealizeWidget(main_plot_shell);
  
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/* Set up graphics context for rubberbanding and add event handlers
   for hydrograph.
*/
  n = 0;
  XtSetArg(wargs[n], XtNbackground,  &graph_gcv.background);n++;
  XtGetValues(hydrograph, wargs, n);
  graph_gcv.foreground = get_pixel_by_name(hydrograph,
			 SELECTED_LINE_COLOR) ^ graph_gcv.background;
  graph_gcv.function = GXxor;
  cb_data->rb_gc = XCreateGC(XtDisplay(hydrograph),
			    XtWindow(hydrograph),
			    GCFunction | GCBackground | GCForeground,
			    &graph_gcv);

  XGrabButton(XtDisplay(hydrograph), AnyButton, AnyModifier,
	      XtWindow(hydrograph), TRUE,
	      ButtonPressMask | ButtonMotionMask |
	      ButtonReleaseMask,
	      GrabModeAsync, GrabModeAsync,
	      XtWindow(hydrograph),
	      XCreateFontCursor(XtDisplay(hydrograph),
				XC_crosshair));

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNlabelString,
	   XmStringCreate(ptm_data->seg_name, XmSTRING_DEFAULT_CHARSET));
  n++;
  cb_data->seg_name_widget = XtCreateManagedWidget("seg_name",
						    xmLabelWidgetClass,
						    form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "seg_name";

  XtAddEventHandler(cb_data->seg_name_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  create_mouse_tracker(form, px_x_axis, ro_x_axis, hydrograph,
		       cb_data, help_cb, top_help_shell);
  XSelectInput(XtDisplay(main_plot_shell), XtWindow(main_plot_shell),
	       StructureNotifyMask);

  XSelectInput(XtDisplay(main_plot_shell),
		DefaultRootWindow(XtDisplay(main_plot_shell)),
				  PropertyChangeMask);
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/*
 * Event handlers added here to draw large crosshairs on hydrograph,
 *  runoff, and precipitation plots when the mouse button is pressed.
 *  gfs, 08/07/91
 *
 * Set up graphics context for crosshairs.
 */
  n = 0;
  XtSetArg(wargs[n], XtNbackground,  &graph_gcv.background);n++;
  XtGetValues(px_x_axis, wargs, n);
  graph_gcv.foreground = get_pixel_by_name(px_x_axis,
			 SELECTED_LINE_COLOR) ^ graph_gcv.background;
  graph_gcv.function = GXxor;
  cb_data->rb_gc_px = XCreateGC(XtDisplay(px_x_axis),
			    XtWindow(px_x_axis),
			    GCFunction | GCBackground | GCForeground,
			    &graph_gcv);

  XGrabButton(XtDisplay(px_x_axis), AnyButton, AnyModifier,
	      XtWindow(px_x_axis), TRUE,
	      ButtonPressMask | ButtonMotionMask |
	      ButtonReleaseMask,
	      GrabModeAsync, GrabModeAsync,
	      XtWindow(px_x_axis),
	      XCreateFontCursor(XtDisplay(px_x_axis),
				XC_crosshair));

  n = 0;
  XtSetArg(wargs[n], XtNbackground,  &graph_gcv.background);n++;
  XtGetValues(ro_x_axis, wargs, n);
  graph_gcv.foreground = get_pixel_by_name(ro_x_axis,
			 SELECTED_LINE_COLOR) ^ graph_gcv.background;
  graph_gcv.function = GXxor;
  cb_data->rb_gc_ro = XCreateGC(XtDisplay(ro_x_axis),
			    XtWindow(ro_x_axis),
			    GCFunction | GCBackground | GCForeground,
			    &graph_gcv);

  XGrabButton(XtDisplay(ro_x_axis), AnyButton, AnyModifier,
	      XtWindow(ro_x_axis), TRUE,
	      ButtonPressMask | ButtonMotionMask |
	      ButtonReleaseMask,
	      GrabModeAsync, GrabModeAsync,
	      XtWindow(ro_x_axis),
	      XCreateFontCursor(XtDisplay(ro_x_axis),
				XC_crosshair));
/*
 * Add event handler for crosshairs in px, ro, or hydrograph plots.
 * Note that this event handler is disabled when in time series
 *  change mode (i.e., when rubberbanding is on).
 */
  if(cb_data->num_rr_oper > 0)
    {
     XtAddEventHandler(cb_data->drawing_area_widget[1],
		       ButtonPressMask, FALSE, crosshairs_timer, ptm_data);
     XtAddEventHandler(cb_data->drawing_area_widget[3],
		       ButtonPressMask, FALSE, crosshairs_timer, ptm_data);
    }
  XtAddEventHandler(cb_data->drawing_area_widget[5],
		    ButtonPressMask, FALSE, crosshairs_timer, ptm_data);

  cb_data->crosshairs_event_widget = NULL;
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

/* Call resize_form_widget to force form to the correct size. It gets
   resized incorrectly after creating the mousetracker label widget.
   dp - 12/8/92
*/
  resize_form_widget(main_drawing_area, form, NULL);


/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/PlotTulsa/RCS/tulplot.c,v $";
 static char rcs_id2[] = "$Id: tulplot.c,v 1.15 2007/05/16 16:43:33 aivo Exp $";}
/*  ===================================================  */

}

/*  This routine reads file seg_sort.in from .ifp_files/local directory        */
/*  It returns the river name associated with current segment  A.V. 10-13-99   */
int get_river_name(char operation_name[9],char river_name[21] )
{
   FILE  	*fnamefp;

   char  	path_name[80],
   		buf[80],
		*cptr;

   int		n,
		found;

   memset(river_name, '\0', 21);
   strcpy(path_name,(char *)getenv("HOME"));

   strcat(path_name,"/.ifp_files/local/seg_sort.in");

   if( (fnamefp = fopen(path_name, "r")) == NULL)
   {
   	printf("Error in open %s\n",path_name);
	return ( 0 );
   }
   while ( 1 )
   {
	if ( ! fgets(buf, 80,fnamefp)) break;


	if ((cptr = strstr (buf,operation_name)) )
	{

		if ((cptr = strstr (buf,"EMPTY")) ) continue;

		for ( n = 0; n< 3; n++)
			fgets(buf, 80,fnamefp);
/*r22-27 changed here to fix river_name being one character truncated.  AV 1/08/03*/
fgets(buf, 9,fnamefp);/*Added these 2 lines, seg_sort.in format changed.  kwz*/
fgets(buf, 80,fnamefp);
		strncpy(river_name,buf,20);

		found = 1;

		break;
	}

   }

   fclose(fnamefp);

   return ( found );
}



