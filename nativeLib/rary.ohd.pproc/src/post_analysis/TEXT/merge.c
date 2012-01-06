/*===================================================================*/
/*                         FILE NAME:   merge.c                      */
/*                                                                   */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_merge                */
/*                                      initialize_merge_data        */
/*                                                                   */
/*  FILE INFORMATION:                                                */
/*    interface for merging gage only and summed hourly MPE          */
/*      fields                                                       */
/*===================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/CascadeB.h>


#include "fill_pixmap.h"
#include "postX.h"
#include "xs_create_menu_buttons.h"
#include "post_stage3.h"
#include "overlay.h"
#include "postanalysis_functions.h"

/********************************************************************/
/*  FUNCTION NAME:   display_merge()                                */
/*       FUNCTION:   sets up and displays merged field window       */
/*********************************************************************

Function type:
   void

Called by function:
   merge_data (callback from Merge Data button)

Functions called:
   initailize_merged_data

*************************************** BEGIN display_merge ***********/

void display_merge()
{
 int     i, n, pixel;
 Arg     wargs[12];
 Widget  shell, form, menuBar;
 Widget  merged_heading;
 Widget  merged_frame;
 Widget  merged_canvas;
 Widget  legend;
 Widget  cascade[2];
 Widget  Control_mainMenuItem, Overlays_mainMenuItem;
 Widget  Help_mainMenuItem;
 Widget  states_widget,rivers_widget,counties_widget,basins_widget;
 draw_struct *mergedata;

static xs_menu_struct Save_menu_struct[] =
	{
	{"Save/separate", quit_and_save, NULL, TRUE, PUSH_BUTTON,
	   NULL, 0, NULL},
	{"Save/overwrite", quit_and_save, NULL, TRUE, PUSH_BUTTON,
	   NULL, 0, NULL},
	};

static xs_menu_struct Control_menu_struct[] =
	{
	{"Quit & Save", NULL, NULL, TRUE,PUSH_BUTTON, Save_menu_struct,
	   XtNumber(Save_menu_struct), NULL},
	{"Close Window", close_shell, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL}
	};


static xs_menu_struct Overlays_menu_struct[] =
	{
	{"States" , show_states, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL},
	{"County" , show_county, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL},
	{"Cities/Towns" , show_cities_and_towns, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL},
	{"Basin boundaries" , show_basin_boundaries, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL},
	{"Rivers" , show_rivers, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL}
	};

 mergedata = (draw_struct *)malloc(sizeof(draw_struct));

 shell = XtCreatePopupShell("Merged_field",
		transientShellWidgetClass, toplevel, NULL, 0);

 form = XtCreateManagedWidget("singleSite_main_form", xmFormWidgetClass,
	  shell, NULL, 0);

  XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
  XtSetArg(wargs[1], XmNleftAttachment, XmATTACH_FORM);
  XtSetArg(wargs[2], XmNrightAttachment, XmATTACH_FORM);
  menuBar = XmCreateMenuBar(form, "singleSite_main_menuBar", wargs, 3);
  XtManageChild(menuBar);

  pixel = get_pixel_by_name(toplevel, "black");
 
 /*merged_heading*/
 
  n = 0;
  XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Merged Field",
      XmSTRING_DEFAULT_CHARSET)); n++;
  XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(wargs[n], XmNtopWidget, menuBar); n++;
 /* XtSetArg(wargs[n], XmNx, 100); n++;
  XtSetArg(wargs[n], XmNtopOffset, 10); n++;*/
  merged_heading = XtCreateManagedWidget("merged_heading",
      xmLabelWidgetClass, form, wargs, n);


 /*merged_frame*/
 
  n = 0;
  XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(wargs[n], XmNtopWidget, merged_heading); n++;
  merged_frame = XtCreateManagedWidget("merged_frame",
	       xmFrameWidgetClass, form, wargs,n);

  /*merged_canvas*/
  n = 0;
  XtSetArg(wargs[n], XtNbackground, pixel); n++;
  
  merged_canvas = XtCreateManagedWidget("merged_canvas",
	          xmDrawingAreaWidgetClass, merged_frame, wargs, n);

  
  /*legend*/
  
  n=0;
  XtSetArg(wargs[n], XtNbackground, pixel); n++; 
  XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++; 
  XtSetArg(wargs[n], XmNleftOffset, 40); n++;
  XtSetArg(wargs[n], XmNrightOffset, 40); n++;
  XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(wargs[n], XmNtopWidget, merged_frame); n++;
  XtSetArg(wargs[n], XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(wargs[n], XmNtopOffset, 10); n++;  
  XtSetArg(wargs[n], XmNbottomOffset, 10); n++;
  legend = XtCreateManagedWidget("legend_canvas",
            xmDrawingAreaWidgetClass, form, wargs, n);

  initialize_merged_data(mergedata,0,merged_canvas,MAXX,MAXY);

  XtAddCallback(merged_canvas, XmNresizeCallback,
	      fill_postanalysis_pixmap,mergedata);
  	      
  XtAddCallback(merged_canvas, XmNexposeCallback,copy_area, mergedata);


  XtAddCallback(legend, XmNexposeCallback, create_legend, mergedata);

  Control_mainMenuItem = XmCreatePulldownMenu(menuBar,
	 "Control_mainMenuItem", NULL, 0);
  Overlays_mainMenuItem = XmCreatePulldownMenu(menuBar,
	 "Overlays_mainMenuItem", NULL, 0);

  XtSetArg(wargs[0], XmNsubMenuId, Control_mainMenuItem);
  XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Control", XmSTRING_DEFAULT_CHARSET));
  cascade[0] = XmCreateCascadeButton(menuBar, "singleSite_Control_cascade", wargs, 2);

  XtSetArg(wargs[0], XmNsubMenuId, Overlays_mainMenuItem);
  XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Overlays", XmSTRING_DEFAULT_CHARSET));
  cascade[1] = XmCreateCascadeButton(menuBar, "singleSite_Overlays_cascade", wargs, 2);
  XtManageChildren(cascade, 2);

 for(i = 0; i < XtNumber(Save_menu_struct); i++)
      Save_menu_struct[i].data = (caddr_t) shell;

 for (i =0; i < XtNumber(Control_menu_struct); i++)
      Control_menu_struct[i].sub_menu_title = Save_menu_struct[i].name;
      
 Control_menu_struct[1].data = (caddr_t) shell;
 xs_create_menu_buttons("", Control_mainMenuItem, Control_menu_struct,
		    XtNumber(Control_menu_struct));

 for(i = 0; i < XtNumber(Overlays_menu_struct); i++)
      Overlays_menu_struct[i].data = (caddr_t) mergedata;
 xs_create_menu_buttons("", Overlays_mainMenuItem, Overlays_menu_struct,
		    XtNumber(Overlays_menu_struct));

 n=0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Help",XmSTRING_DEFAULT_CHARSET)); n++;
 Help_mainMenuItem = XmCreateCascadeButton(menuBar, "Help_mainMenuItem", wargs, n);

 XtSetArg(wargs[0], XmNmenuHelpWidget,Help_mainMenuItem );
 XtSetValues(menuBar, wargs, 1);
 XtAddCallback(Help_mainMenuItem, XmNactivateCallback,
                  popup_help_window, "POSTMERGE");
 XtManageChild(Help_mainMenuItem);

  states_widget = XtNameToWidget(Overlays_mainMenuItem, "States");
  if(overlay_avail.state == 0) XtSetSensitive(states_widget,FALSE);

  counties_widget = XtNameToWidget(Overlays_mainMenuItem, "County");
  if(overlay_avail.county == 0) XtSetSensitive(counties_widget,FALSE);

  rivers_widget = XtNameToWidget(Overlays_mainMenuItem, "Rivers");
  if(overlay_avail.river == 0) XtSetSensitive(rivers_widget,FALSE);

  basins_widget = XtNameToWidget(Overlays_mainMenuItem,
    "Basin boundaries");
  if(overlay_avail.fgbasin == 0) XtSetSensitive(basins_widget,FALSE);

  XtAddEventHandler(merged_canvas, ButtonPressMask, FALSE,
		   locate_merge, mergedata);

  if (dbg)
    printf("before merged_canvas fill pixmap\n");
  fill_postanalysis_pixmap(merged_canvas, mergedata, NULL);
  if (dbg)
    printf("after merged_canvas fill pixmap\n");
  XtPopup(shell, XtGrabNone);

}
/*************************************** END display_merge ***********/

/********************************************************************/
/*  FUNCTION NAME:   initialize_merge_data()                        */
/*       FUNCTION:   setup merged data for display                  */
/*********************************************************************

Function type:
   void

Called by function:
   display_merge

Functions called:
   set_colorvalues

*************************** BEGIN initialize_merged_data ***********/

void initialize_merged_data(data,type,w,x,y)
   draw_struct   *data;
   int           type;
   Widget        w;
   int           x,y;
{
 int i,j;
 XGCValues       gcv;
 int             mask = GCForeground;
 Display         *dpy;

 if (dbg) printf("In initialize merged data\n");
 data->maximum_columns = x;
 data->maximum_rows = y;
 data->data_array =
     (short int **)malloc((data->maximum_columns)*sizeof(short int *));
 for (i=0; i<data->maximum_columns; i++)
     data->data_array[i] =
       (short int *)malloc((data->maximum_rows)*sizeof(short int));

 data->levels = (int *)malloc(17*sizeof(int));
 data->gc = (GC *)malloc(17*sizeof(GC));

 data->origin.x = XOR;
 data->origin.y = YOR;

/*-------------------------------------------------------*/
/*   merge array is generated by MergeData function      */
/*   gageonly array is generated by GageOnly function    */
/*-------------------------------------------------------*/

 if (type == 0)
   {
    for (i=0; i<data->maximum_columns; i++)
    for (j=0; j<data->maximum_rows; j++)
       data->data_array[i][j] = merge[j][i];
   }
 if (type == 1)
   {
    for (i=0; i<data->maximum_columns; i++)
    for (j=0; j<data->maximum_rows; j++)
       data->data_array[i][j] = gageonly2[j][i];
   }

 set_colorvalues(data);
 data->pix = (Pixmap) NULL;
 data->pixbase = (Pixmap) NULL;
 data->states_on = 1;
 data->w = w;
 dpy = XtDisplay(w);
 for (i=0; i<data->num_levels; i++)
    {
     gcv.foreground = get_pixel_by_name(w,color_list_levels[i]);
     data->gc[i] = XCreateGC(dpy, DefaultRootWindow(dpy),
				 mask, &gcv);
    }



}
/*************************** END initialize_merged_data ***********/
