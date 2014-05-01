/* File: plot_mod.c
 *
 *  This is a program to create the graphical plot for the
 *    UHCHNG, RROCHNG, RICHNG mods.
 */
/******************************************************************************
/*  AiV 4/30/04 added codes to handle horizontal scaling for UHGCHDATE mod
/*                        and UHGCHNG mod
/*              new routine: change_horizontal_scale()
*******************************************************************************/
#include "mods_plot.h"
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"


typedef struct {
		Widget  top_help_shell;         /* Widget data structure for top help shell     */
		char    *widget_name;           /* Widget name pointer          */
	       } help_cb_struct;

void set_currentModSavedFalseCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);

void plot_mod(Mods_everythingStruct *data)
{
   Arg          wargs[10];                      /* Window resource data structure array */
   Widget       mp_main_plot_shell;             /* Widget data structure for main plot shell    */
   Widget       mp_main_drawing_area;           /* Widget data structure for main drawing area  */
   Widget       mp_form;                        /* Widget data structure                */
   Widget       mp_y_axis_label;                /* Widget data structure for y-axis label       */
   Widget       mp_y_axis;                      /* Widget data structure for y-axis             */
   Widget       mp_x_axis_label;                /* Widget data structure for x-axis label       */
   Widget       mp_x_axis;                      /* Widget data structure for x-axis             */
   Widget       mp_graph;                       /* Widget data structure for the graph          */
   Widget       top_help_shell;                 /* Widget data structure for the help shell     */
   Widget       mp_legend_rc;                   /* Widget data structure for the graph legend   */
   Widget       change_y_axis_max_widget;       /* Widget to change the maximum value of y-axis */
   int          i, j, n;                        /* Counters              */
   float        y_axis_max;                     /* Maximum value for y-axis     */
   float        max_y=0.0;                      /* Pixel for the maximum y value        */
   XGCValues    mp_graph_gcv;                   /* Values for the graphics context      */
   static help_cb_struct    *help_cb;           /* Data structure for help      */
   char         mp_main_shell_name[20];         /* Name of main mods plot shell */
   float        mp_y_increment;                 /* Increment for plotting y-axis values */
   Widget       change_horizontal_scale_widget;       /* local var, Widget to change the maximum x-axis 
                                                AV added 4/21/04 */ 
   Pixel        foreground;
   Pixel        background;
   
     
    /*AV data was plotted from 0 to num_pts-1 only 
      originally j looped from 0 to num_pts */
   for (i=0; i<data->modsPlotData->num_ts_sel; i++)      
      for (j=0; j<data->modsPlotData->num_pts-1; j++)
	 if(data->modsPlotData->ts_array[i][j] > max_y)
	    max_y = data->modsPlotData->ts_array[i][j];

   /* Set the y_axis_max and increment for plotting y axis values */
   scale_max_min(data->modsPlotData->min_y, max_y, 0.0,
		 &data->modsPlotData->min_y, &y_axis_max, &mp_y_increment);

   data->modsPlotData->max_y = max_y;
   data->modsPlotData->y_axis_max = y_axis_max;
   data->modsPlotData->default_y_axis_max = y_axis_max;
   data->modsPlotData->mp_y_increment = mp_y_increment;


/*
 * Set pix and gc variables to NULL.
 */
  data->modsPlotData->pix[0] = (unsigned int)NULL;       /* y_axis_label   */
  data->modsPlotData->pix[1] = (unsigned int)NULL;       /* y_axis         */
  data->modsPlotData->pix[2] = (unsigned int)NULL;       /* graph          */
  data->modsPlotData->pix[3] = (unsigned int)NULL;       /* x_axis         */
  data->modsPlotData->pix[4] = (unsigned int)NULL;       /* x_axis_label   */

  data->modsPlotData->gc[0] = NULL;                /* y_axis_label   */
  data->modsPlotData->gc[1] = NULL;                /* y_axis         */
  data->modsPlotData->gc[2] = NULL;                /* graph          */
  data->modsPlotData->gc[3] = NULL;                /* x_axis         */
  data->modsPlotData->gc[4] = NULL;                /* x_axis_label   */

 /*
 * all drawing_area_widgets used in expose callbacks
 */
  data->modsPlotData->drawing_area_widget[0] = NULL;  /* y_axis_label   */
  data->modsPlotData->drawing_area_widget[1] = NULL;  /* y_axis         */
  data->modsPlotData->drawing_area_widget[2] = NULL;  /* graph          */
  data->modsPlotData->drawing_area_widget[3] = NULL;  /* x_axis         */
  data->modsPlotData->drawing_area_widget[4] = NULL;  /* x_axis_label   */


  n=0;
  XtSetArg(wargs[n], XmNgeometry, "800x600+200+200"); n++;
  mp_main_plot_shell = XtCreateApplicationShell("mp_main_plot_shell",
					   transientShellWidgetClass,
					   wargs, n);                                           

  data->modsPlotData->main_plot_shell = mp_main_plot_shell;

  top_help_shell = XtCreatePopupShell("top_help_shell",
				      transientShellWidgetClass,
				      mp_main_plot_shell, NULL, 0);

  memset(mp_main_shell_name, '\0', 20);
  strcpy(mp_main_shell_name, "Mod Plot:  ");
 /* if(data->modsPlotData->mod_type_sw == UH)
     strcat(mp_main_shell_name, "UHGCHNG");
  else if(data->modsPlotData->mod_type_sw == UHD)
      strcat(mp_main_shell_name, "UHGCDATE");
  else if(data->modsPlotData->mod_type_sw == RRICHNG)
     strcat(mp_main_shell_name, "RRICHNG");
  else if(data->modsPlotData->mod_type_sw == ROCHNG)
     strcat(mp_main_shell_name, "ROCHNG");*/
  switch (data->modsPlotData->mod_type_sw){
   case UH:
      strcat(mp_main_shell_name, "UHGCHNG");
      break;
   case UHD:
      strcat(mp_main_shell_name, "UHGCDATE");
      break;
   case RRICHNG:
      strcat(mp_main_shell_name, "RRICHNG");
      break;
   case ROCHNG:
      strcat(mp_main_shell_name, "ROCHNG");
      break;
   default:
      break;
  }

  n=0;
  XtSetArg(wargs[n], XmNminAspectX, 4); n++;
  XtSetArg(wargs[n], XmNmaxAspectX, 4); n++;
  XtSetArg(wargs[n], XmNminAspectY, 3); n++;
  XtSetArg(wargs[n], XmNmaxAspectY, 3); n++;
  XtSetArg(wargs[n], XmNtitle, mp_main_shell_name); n++;
  XtSetValues(mp_main_plot_shell, wargs, n);


  n=0;
  mp_main_drawing_area = XtCreateManagedWidget("mp_main_drawing_area",
					       xmDrawingAreaWidgetClass,
					       mp_main_plot_shell, NULL, n);
/*
 * Create form widget to hold all other drawing areas and the legend
 */
  n=0;
  mp_form = XtCreateManagedWidget("mp_form",
				  xmFormWidgetClass,
				  mp_main_drawing_area, NULL, n);

  XtAddCallback(mp_main_drawing_area, XmNresizeCallback,
		resize_mp_form_widget, data->modsPlotData);

  data->modsPlotData->form = mp_form;
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n = 0;
  data->modsPlotData->done_widget = XtCreateManagedWidget("done",
						xmPushButtonWidgetClass,
						mp_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "done";

  XtAddEventHandler(data->modsPlotData->done_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(data->modsPlotData->done_widget, XmNactivateCallback, mp_done,
		data);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  mp_y_axis_label = XtCreateManagedWidget("mp_y_axis_label",
					   xmDrawingAreaWidgetClass,
					   mp_form, wargs, n);
  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "mp_y_axis_label";

  XtAddEventHandler(mp_y_axis_label, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);
  XtAddCallback(mp_y_axis_label, XmNresizeCallback,
		resize_mp_y_axis_label, data->modsPlotData);

  XtAddCallback(mp_y_axis_label, XmNexposeCallback,
		copy_one_mp_drawing_area, data->modsPlotData);

  data->modsPlotData->drawing_area_widget[0] = mp_y_axis_label;
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNleftWidget, mp_y_axis_label); n++;
  mp_y_axis = XtCreateManagedWidget("mp_y_axis",
				    xmDrawingAreaWidgetClass,
				    mp_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "discharge_axis";

  XtAddEventHandler(mp_y_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(mp_y_axis, XmNresizeCallback,
		resize_mp_y_axis, data->modsPlotData);

  XtAddCallback(mp_y_axis, XmNexposeCallback,
		copy_one_mp_drawing_area, data->modsPlotData);

  data->modsPlotData->drawing_area_widget[1] = mp_y_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNleftWidget, mp_y_axis); n++;
  mp_graph = XtCreateManagedWidget("mp_graph",
				    xmDrawingAreaWidgetClass,
				    mp_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "mp_graph";

  XtAddEventHandler(mp_graph, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(mp_graph, XmNresizeCallback,
		resize_mp_graph, data->modsPlotData);

  XtAddCallback(mp_graph, XmNexposeCallback,
		copy_one_mp_drawing_area, data->modsPlotData);
/*
 * Add callback so we can quit from tulplot by typing 'q' or 'Q'
 *  in mp_graph drawing area
 */
  XtAddCallback(mp_graph, XmNinputCallback,
		exit_mp_cb, NULL);

  data->modsPlotData->drawing_area_widget[2] = mp_graph;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, mp_graph); n++;
  mp_x_axis = XtCreateManagedWidget("mp_x_axis",
				    xmDrawingAreaWidgetClass,
				    mp_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "mp_x_axis";

  XtAddEventHandler(mp_x_axis, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(mp_x_axis, XmNresizeCallback,
		resize_mp_x_axis, data->modsPlotData);

  XtAddCallback(mp_x_axis, XmNexposeCallback,
		copy_one_mp_drawing_area, data->modsPlotData);

  data->modsPlotData->drawing_area_widget[3] = mp_x_axis;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, mp_x_axis); n++;
  mp_x_axis_label = XtCreateManagedWidget("mp_x_axis_label",
					  xmDrawingAreaWidgetClass,
					  mp_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "mp_x_axis_label";

  XtAddEventHandler(mp_x_axis_label, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(mp_x_axis_label, XmNresizeCallback,
		resize_mp_x_axis_label, data->modsPlotData);

  XtAddCallback(mp_x_axis_label, XmNexposeCallback,
		copy_one_mp_drawing_area, data->modsPlotData);

  data->modsPlotData->drawing_area_widget[4] = mp_x_axis_label;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNtopWidget, mp_x_axis_label); n++;
  mp_legend_rc = XtCreateManagedWidget("mp_legend_rc",
				       xmRowColumnWidgetClass,
				       mp_form, wargs, n);


  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "mp_legend_rc";

  XtAddEventHandler(mp_legend_rc, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);


  data->modsPlotData->legend_rc = mp_legend_rc;
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  for(i=0; i<data->modsPlotData->num_ts_sel; i++)
  {
     n=0;
     XtSetArg(wargs[n], XmNbackground, get_pixel_by_name(mp_legend_rc,
	      "gray85")); n++;
     XtSetArg(wargs[n], XmNforeground, get_pixel_by_name(mp_legend_rc,
	      data->modsPlotData->ts_color[i])); n++;
     data->modsPlotData->legend[i] = XtCreateManagedWidget(data->modsPlotData->op_name[i],
						xmPushButtonWidgetClass,
						mp_legend_rc, wargs, n);
     XtAddCallback(data->modsPlotData->legend[i], XmNactivateCallback, mp_change_ts,
		   data->modsPlotData);
/*kwz.uhgchdate.Click on the button indicates mod plot changed.So remember it.*/
     XtAddCallback(data->modsPlotData->legend[i], XmNactivateCallback, (XtCallbackProc)set_currentModSavedFalseCB,
		   data);

     help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

     help_cb->top_help_shell = top_help_shell;

     help_cb->widget_name = "legend_rc";

     XtAddEventHandler(data->modsPlotData->legend[i], EnterWindowMask, FALSE,
		       help_event_handler, help_cb);
  }

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n = 0;
  data->modsPlotData->undo_widget = XtCreateManagedWidget("undo_widget",
						xmPushButtonWidgetClass,
						mp_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "undo_widget";

  XtAddEventHandler(data->modsPlotData->undo_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNlabelString,
	   XmStringCreate(data->modsPlotData->myt, XmSTRING_DEFAULT_CHARSET)); n++;
  data->modsPlotData->month_year_widget = XtCreateManagedWidget("mp_month_year",
						     xmLabelWidgetClass,
						     mp_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "mp_month_year";

  XtAddEventHandler(data->modsPlotData->month_year_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
  n=0;
  XtSetArg(wargs[n], XmNvalue, 1); n++;
  if(data->modsPlotData->mod_type_sw == UH || 
              data->modsPlotData->mod_type_sw == UHD)
     {XtSetArg(wargs[n], XmNmaximum, 3); n++;}
  else if(data->modsPlotData->mod_type_sw == ROCHNG)
     {XtSetArg(wargs[n], XmNmaximum, 5); n++;}
  else if(data->modsPlotData->mod_type_sw == RRICHNG)
     {XtSetArg(wargs[n], XmNmaximum, 10); n++;}

  XtSetArg(wargs[n], XmNtitleString,
	   XmStringCreate(" Change y max",
			  XmSTRING_DEFAULT_CHARSET)); n++;

  change_y_axis_max_widget = XtCreateManagedWidget("change_y_axis_max_widget",
						      xmScaleWidgetClass,
 						      mp_form, wargs, n);
 
  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "change_y_axis_max_widget";

  XtAddEventHandler(change_y_axis_max_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

  XtAddCallback(change_y_axis_max_widget, XmNvalueChangedCallback,
		change_y_axis_max, data->modsPlotData);

  data->modsPlotData->change_y_axis_max_widget = change_y_axis_max_widget;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

/* AV 4/21/04 added change x axis scale                                   */
/* horizontal scaling for UHGCHNG and UHGCDATE DISPLAY windows            */
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/**kwz.taking out the Horizontal Scale
/*if(data->modsPlotData->mod_type_sw == UH || 
/*               data->modsPlotData->mod_type_sw == UHD){
/*     n=0;
/*     XtSetArg(wargs[n], XmNvalue, 1); n++;
/* 
/*     XtSetArg(wargs[n], XmNtitleString,
/*	   XmStringCreate("Horizontal Scale",
/*			  XmSTRING_DEFAULT_CHARSET)); n++;
/*
/*     change_horizontal_scale_widget = XtCreateManagedWidget("change_horizontal_scale_widget",
/*						      xmScaleWidgetClass,
/*						      mp_form, wargs, n);
/*  
/*     help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));
/*
/*     help_cb->top_help_shell = top_help_shell;
/*
/*     help_cb->widget_name = "change_horizontal_scale_widget";
/*  
/*
/*     XtAddEventHandler(change_horizontal_scale_widget, EnterWindowMask, FALSE,
/*		    help_event_handler, help_cb);
/*   /* Disable horizontal scale on rochng and rrichng mods   */            
/*    if(data->modsPlotData->mod_type_sw == ROCHNG)
/*       XtSetSensitive(change_horizontal_scale_widget, FALSE);
/*    if(data->modsPlotData->mod_type_sw == RRICHNG)
/*       XtSetSensitive(change_horizontal_scale_widget, FALSE);
/*   
/*     XtAddCallback(change_horizontal_scale_widget, XmNvalueChangedCallback,
/*		change_horizontal_scale, data);
/*                
/*     data->modsPlotData->change_horizontal_scale_widget = change_horizontal_scale_widget;
/* }
*/
/* AV 4/21/04 added change x axis scale End
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */ 
/**************************************************************************/
  XtRealizeWidget(mp_main_plot_shell);
  
  
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/* Set up graphics context for rubberbanding and add event handlers
   for hydrograph.
*/
  n = 0;
  XtSetArg(wargs[n], XtNbackground,  &mp_graph_gcv.background);n++;
  XtGetValues(mp_graph, wargs, n);
  mp_graph_gcv.foreground = get_pixel_by_name(mp_graph,
			    RB_LINE_COLOR) ^ mp_graph_gcv.background;
  mp_graph_gcv.function = GXxor;
  data->modsPlotData->rb_gc = XCreateGC(XtDisplay(mp_graph),
			     XtWindow(mp_graph),
			     GCFunction | GCBackground | GCForeground,
			     &mp_graph_gcv);

  XGrabButton(XtDisplay(mp_graph), AnyButton, AnyModifier,
	      XtWindow(mp_graph), TRUE,
	      ButtonPressMask | ButtonMotionMask |
	      ButtonReleaseMask,
	      GrabModeAsync, GrabModeAsync,
	      XtWindow(mp_graph),
	      XCreateFontCursor(XtDisplay(mp_graph),
				XC_crosshair));


/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

  n=0;
  XtSetArg(wargs[n], XmNlabelString,
	   XmStringCreate(data->modsPlotData->seg_name, XmSTRING_DEFAULT_CHARSET));
  n++;
  data->modsPlotData->seg_name_widget = XtCreateManagedWidget("mp_seg_name",
						    xmLabelWidgetClass,
						    mp_form, wargs, n);

  help_cb = (help_cb_struct *)malloc(sizeof(help_cb_struct));

  help_cb->top_help_shell = top_help_shell;

  help_cb->widget_name = "mp_seg_name";

  XtAddEventHandler(data->modsPlotData->seg_name_widget, EnterWindowMask, FALSE,
		    help_event_handler, help_cb);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

  mp_create_mouse_tracker(mp_form, mp_graph, data->modsPlotData, help_cb,
			  top_help_shell);

  XSelectInput(XtDisplay(mp_main_plot_shell), XtWindow(mp_main_plot_shell),
	       StructureNotifyMask);

  XSelectInput(XtDisplay(mp_main_plot_shell),
		DefaultRootWindow(XtDisplay(mp_main_plot_shell)),
				  PropertyChangeMask);

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/* Added call to resize_mp_form_widget to force the form widget to the size
 * of the mp_main_drawing_area.
 * This is a temporary fix to correct the problem of the form getting
 * resized when a label widget is created after the main shell is
 * realized.  dp - 29 Dec. 1992
 */
  
  resize_mp_form_widget(mp_main_drawing_area, data->modsPlotData, NULL);
  data->modsPlotData->mp_main_drawing_area = mp_main_drawing_area;
    
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/*
 * Add event handler for crosshairs in mods plot.
 * Note that this event handler is disabled when in
 *  change mode (i.e., when rubberbanding is on).
 */
  XtAddEventHandler(data->modsPlotData->drawing_area_widget[2],
		    ButtonPressMask, FALSE, mp_crosshairs_timer, data->modsPlotData);

 
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/plot_mod.c,v $";
 static char rcs_id2[] = "$Id: plot_mod.c,v 1.8 2006/04/07 14:34:52 aivo Exp $";}
/*  ===================================================  */

}

/*    AV   5/02/04        ********************************/
/*    Ruoutine to handle scale change on x axis            
/*
/*
/*                       ********************************/
void change_horizontal_scale(w, data, call_data)

  Widget                        w;		/* widget data structure	*/
  
  Mods_everythingStruct         *data;          /* mods everything structure pointer    */
  
  XmScaleCallbackStruct         *call_data;     /* Xmscale call back structure pointer  */
{

  Arg          wargs[10]; 
  Dimension    width;        
  Dimension    height;    
  int  n, kk, num_pts;
  
  kk = 0;
  num_pts = data->modsPlotData->Orgnum_pts;
  n = call_data->value;
  /* scaling the plotting points base on the size of the horiz. slide bar */
  /* using percentage of the value  */
  if( n == 1 ) kk = 0;
  if( n == 2 ){
    kk = (25 * num_pts)/100;     
  }else if ( n == 3) {
    kk = (50 * num_pts)/100;   
  }else if ( n == 4) {
    printf("num_pts = %d\n",num_pts);
    kk = (75 * num_pts)/100;  
    /* if number of ordinates is <= 4 */
    if(num_pts <= 4) kk = 1; 
  }else kk = 0;  
 
  data->modsPlotData->end_obs = data->modsPlotData->Orgend_obs - kk;
  data->modsPlotData->num_pts = data->modsPlotData->Orgnum_pts - kk;
  data->modsPlotData->max_x   = data->modsPlotData->Orgmax_x   - (float)kk;

  n=0;
  XtSetArg(wargs[n], XmNwidth,  &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtGetValues(data->modsPlotData->main_plot_shell, wargs, n);
  n=0;
  width = width + 2; height = height + 2;
  XtSetArg(wargs[n], XmNwidth,  width); n++;
  XtSetArg(wargs[n], XmNheight, height); n++;  
  XtSetValues(data->modsPlotData->main_plot_shell, wargs, n);
 
 }
