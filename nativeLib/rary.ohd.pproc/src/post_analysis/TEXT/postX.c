/*=========================================================================*/
/*                         FILE NAME:   postX.c                            */     
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   post_analysis()                    */
/*                                      display_post()                     */
/*                                      initialize_draw_data()             */
/*                                      create_legend()                    */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <Xm/Protocols.h>
#include <Xm/CascadeB.h>
#include <stdlib.h>

#include "fill_pixmap.h"
#include "xs_create_menu_buttons.h"
#include "set_fields.h"
#include "post_stage3.h"
#include "overlay.h"
#include "postanalysis_functions.h"
#include "post_stage3_globals.h"
#include <stdlib.h>
#include "stage3_interface.h"
#include "postX.h"
#include "zoom_data_struct.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/********************************************************************/
/*  FUNCTION NAME:   post_analysis()                                */
/*       FUNCTION:   calls functions to generate and display fields */
/*********************************************************************

Function type:
   void

Called by function:
   callback from ok_button on choose_dates window

Functions called:
   set_fields
   display_post

******************************************** BEGIN post_analysis ***********/

void post_analysis(w, data, call_data)
   Widget       w;
   caddr_t     *data, *call_data;
{

 if (dbg) printf("Entering post analysis callback\n");

 XFlush(XtDisplay(w));

 /*-------------------------------------------------------------------------*/
 /*     calculate gageonly and summed hourly MPE fields                */
 /*-------------------------------------------------------------------------*/

 set_fields();

 /*-------------------------------------------------------------------------*/
 /*     display information                                                 */
 /*-------------------------------------------------------------------------*/

 display_post();
}

/********************************************* END post_analysis ***********/


/*********************************************************************/
/*  FUNCTION NAME:   display_post()                                  */
/*       FUNCTION:   set up and display main post analysis window    */
/*********************************************************************

Function type:
   void

Called by function:
   post_analysis

Functions called:
   get_pixel_by_name
   initialize_draw_data
   (callback) fill_pixmap
   (callback) copy_area
   (callback) create_legend
   xs_create_menu_buttons
   (callback) zoom
   (callback) show_states
   (callback) show_county
   (callback) show_cities_and_towns
   (callback) show_rivers
   (callback) show_basin_boundaries
   (callback) show_gages
   fill_pixmap
   xs_create_xor_gc

******************************************** BEGIN display_post ************/

void display_post()
{
   int                          i, n, pixel;
   Arg                          wargs[15];
   Widget                       shell, form, menuBar;
   Widget                       multi_sensor_heading, gage_only_heading;
   Widget                       multi_sensor_frame, gageonly_frame;
   Widget                       states_widget, cities_widget, rivers_widget;
   Widget                       basins_widget, gages_widget, gagval_widget, county_widget;
   Widget                       legend, cascade[3];
   Widget                       Control_mainMenuItem, Options_mainMenuItem, Overlays_mainMenuItem;
   Widget                       Help_mainMenuItem;
   
   static zoom_data_struct      zdata, z2data;
   Atom                         wmAtom;
   draw_struct                 *draw2_data, *draw_data;
   
 static xs_menu_struct Control_menu_struct[] =
    {
    {"Merge Data", merge_data, NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"Quit",       quit,       NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL}
    };

 static xs_menu_struct Options_menu_struct[] =
    {
    {"Zoom",       zoom,            NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"Gage table", show_gage_table, NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"Remove AP",  remove_ap,       NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL}
    };

 static xs_menu_struct Overlays_menu_struct[] =
    {
    {"States",           show_states,           NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"County",           show_county,           NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"Cities/Towns",     show_cities_and_towns, NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"Basin boundaries", show_basin_boundaries, NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"Rivers",           show_rivers,           NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"Gage Identifiers", show_gages,            NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL},
    {"Gage Values",      show_values,           NULL, TRUE,  PUSH_BUTTON, NULL, 0, NULL}
    };
 
 /*malloc space */
 
 zdata.ddata = (draw_struct *)malloc(sizeof(draw_struct));
 z2data.ddata = (draw_struct *)malloc(sizeof(draw_struct)); 

 draw_data = (draw_struct *)malloc(sizeof(draw_struct));
 draw2_data = (draw_struct *)malloc(sizeof(draw_struct));

 draw_data = zdata.ddata;
 draw2_data = z2data.ddata;
 
 shell = XtCreatePopupShell("Summed hourly MPE field & Gage only field", transientShellWidgetClass, toplevel, NULL, 0);

 
 form = XtCreateManagedWidget("singleSite_main_form", xmFormWidgetClass, shell, NULL, 0);
 
 /*for menu bar such as Control, Options and Overlays*/
 
 XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
 XtSetArg(wargs[1], XmNleftAttachment, XmATTACH_FORM);
 XtSetArg(wargs[2], XmNrightAttachment, XmATTACH_FORM);
 menuBar = XmCreateMenuBar(form, "singleSite_main_menuBar", wargs, 3);
 XtManageChild(menuBar);

 pixel = get_pixel_by_name(toplevel, "black");

 /*for string Summed Hourly MPE Field in managed widget multi_sensor_heading*/
 
 n = 0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Summed Hourly MPE Field", XmSTRING_DEFAULT_CHARSET)); n++; 
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, menuBar); n++;

 multi_sensor_heading = XtCreateManagedWidget("multi_sensor_heading", xmLabelWidgetClass, form, wargs, n);


 /*for multi_sensor_frame*/
 
 n = 0; 
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET);
 XtSetArg(wargs[n], XmNtopWidget, multi_sensor_heading); n++;

 multi_sensor_frame = XtCreateManagedWidget("multi_sensor_frame", xmDrawingAreaWidgetClass, form, wargs, n);

/*for Gage Only Field heading*/

 n = 0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Gage Only Field", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNtopWidget, multi_sensor_frame); n++; 
 gage_only_heading = XtCreateManagedWidget("gage_only_heading", xmLabelWidgetClass, form, wargs, n);
 
/*for gageonly_frame*/
 
 n = 0; 
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, gage_only_heading); n++;
 gageonly_frame = XtCreateManagedWidget("gageonly_frame", xmDrawingAreaWidgetClass, form, wargs, n);
 
 /*for multi_sensor_canvas*/
 
 n = 0;
 XtSetArg(wargs[n], XtNbackground, pixel); n++;  
 
 multi_sensor_canvas = XtCreateManagedWidget("multi_sensor_canvas", xmDrawingAreaWidgetClass, multi_sensor_frame, wargs, n);
    
 /*for gageonly_canvas*/

 n = 0;
 XtSetArg(wargs[n], XtNbackground, pixel); n++;

 gageonly_canvas = XtCreateManagedWidget("gageonly_canvas", xmDrawingAreaWidgetClass, gageonly_frame, wargs, n);
  
 /*create legend */
 
 n=0;
 XtSetArg(wargs[n], XtNbackground, pixel); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftWidget, gageonly_frame); n++;
 XtSetArg(wargs[n], XmNleftOffset, 40); n++;
 XtSetArg(wargs[n], XmNrightOffset, 40); n++;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, gageonly_frame); n++; 
 XtSetArg(wargs[n], XmNbottomAttachment,XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNtopOffset, 10); n++;
 XtSetArg(wargs[n], XmNbottomOffset, 10); n++;
 legend = XtCreateManagedWidget("legend_canvas", xmDrawingAreaWidgetClass, form, wargs, n);
  
 initialize_draw_data(draw_data,  0, multi_sensor_canvas, MAXX, MAXY);
 initialize_draw_data(draw2_data, 1, gageonly_canvas, MAXX, MAXY);

 XtAddCallback(multi_sensor_canvas, XmNresizeCallback, fill_postanalysis_pixmap, draw_data);
 XtAddCallback(multi_sensor_canvas, XmNexposeCallback, copy_area, draw_data); 
 XtAddCallback(gageonly_canvas, XmNresizeCallback, fill_postanalysis_pixmap, draw2_data);
 XtAddCallback(gageonly_canvas, XmNexposeCallback, copy_area,  draw2_data);
 XtAddCallback(legend, XmNexposeCallback, create_legend, draw_data);
 

 Control_mainMenuItem  = XmCreatePulldownMenu(menuBar, "Control_mainMenuItem",  NULL, 0);
 Options_mainMenuItem  = XmCreatePulldownMenu(menuBar, "Options_mainMenuItem",  NULL, 0);
 Overlays_mainMenuItem = XmCreatePulldownMenu(menuBar, "Overlays_mainMenuItem", NULL, 0);

 XtSetArg(wargs[0], XmNsubMenuId, Control_mainMenuItem);
 XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Control", XmSTRING_DEFAULT_CHARSET));
 cascade[0] = XmCreateCascadeButton(menuBar, "singleSite_Control_cascade", wargs, 2);

 XtSetArg(wargs[0], XmNsubMenuId, Options_mainMenuItem);
 XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Options", XmSTRING_DEFAULT_CHARSET));
 cascade[1] = XmCreateCascadeButton(menuBar, "singleSite_Options_cascade", wargs, 2);

 XtSetArg(wargs[0], XmNsubMenuId, Overlays_mainMenuItem);
 XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Overlays", XmSTRING_DEFAULT_CHARSET));
 cascade[2] = XmCreateCascadeButton(menuBar, "singleSite_Overlays_cascade", wargs, 2);

 XtManageChildren(cascade, 3);

 n=0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Help",XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNmnemonic, 'H'); n++;
 Help_mainMenuItem = XmCreateCascadeButton(menuBar, "Help_mainMenuItem", wargs, 2);

 XtSetArg(wargs[0], XmNmenuHelpWidget,Help_mainMenuItem );
 XtSetValues(menuBar, wargs, 1);
 XtAddCallback(Help_mainMenuItem, XmNactivateCallback,
                  popup_help_window, "POSTANALYSIS");
 XtManageChild(Help_mainMenuItem);

 Options_menu_struct[0].data = (caddr_t) &zdata;
/* Options_menu_struct[0].data = (caddr_t) &z2data;*/
 Options_menu_struct[1].data = (caddr_t) draw2_data;
 Options_menu_struct[2].data = (caddr_t) draw_data;
 

 for(i = 0; i < XtNumber(Overlays_menu_struct); i++)    
    Overlays_menu_struct[i].data = (caddr_t) draw_data;

 xs_create_menu_buttons("", Control_mainMenuItem,  Control_menu_struct,  XtNumber(Control_menu_struct));
 xs_create_menu_buttons("", Options_mainMenuItem,  Options_menu_struct,  XtNumber(Options_menu_struct));
 xs_create_menu_buttons("", Overlays_mainMenuItem, Overlays_menu_struct, XtNumber(Overlays_menu_struct));

 zoom_widget = XtNameToWidget(Options_mainMenuItem, "Zoom");
 XtAddCallback(zoom_widget, XmNactivateCallback, zoom, &z2data); 
 XtSetSensitive(zoom_widget,FALSE);
 

 remove_apw = XtNameToWidget(Options_mainMenuItem, "Remove AP");
 XtAddCallback(remove_apw, XmNactivateCallback, redo_gageonly, draw2_data);
 XtSetSensitive(remove_apw,FALSE);

 states_widget = XtNameToWidget(Overlays_mainMenuItem, "States");
 zdata.statew = states_widget;
 z2data.statew = states_widget; 
 XtAddCallback(states_widget, XmNactivateCallback, show_states,draw2_data);
 if(overlay_avail.state == 0) 
    XtSetSensitive(states_widget,FALSE);

 county_widget = XtNameToWidget(Overlays_mainMenuItem, "County");
 zdata.countyw = county_widget;
 z2data.countyw = county_widget;
 XtAddCallback(county_widget, XmNactivateCallback, show_county,draw2_data);
 if(overlay_avail.county == 0)
   XtSetSensitive(county_widget,FALSE);

 cities_widget = XtNameToWidget(Overlays_mainMenuItem, "Cities/Towns");
 zdata.cityw = cities_widget;
 z2data.cityw = cities_widget; 
 XtAddCallback(cities_widget,XmNactivateCallback,show_cities_and_towns,draw2_data);

 rivers_widget = XtNameToWidget(Overlays_mainMenuItem, "Rivers");
 zdata.riverw = rivers_widget;
 z2data.riverw = rivers_widget;
 XtAddCallback(rivers_widget, XmNactivateCallback, show_rivers, draw2_data);
 if(overlay_avail.river== 0) 
    XtSetSensitive(rivers_widget,FALSE);

 basins_widget = XtNameToWidget(Overlays_mainMenuItem, "Basin boundaries");
 zdata.basinw = basins_widget;
 z2data.basinw = basins_widget;
 XtAddCallback(basins_widget, XmNactivateCallback, show_basin_boundaries, draw2_data);
 if(overlay_avail.fgbasin== 0)
   XtSetSensitive(basins_widget,FALSE);

 gages_widget = XtNameToWidget(Overlays_mainMenuItem, "Gage Identifiers");
 XtSetSensitive(gages_widget,FALSE);
 zdata.gagew = gages_widget;
 z2data.gagew = gages_widget;
 XtAddCallback(gages_widget, XmNactivateCallback, show_gages, draw2_data);


 gagval_widget = XtNameToWidget(Overlays_mainMenuItem, "Gage Values");
 XtSetSensitive(gagval_widget,FALSE);
 zdata.gagvw = gagval_widget;
 z2data.gagvw = gagval_widget;
 XtAddCallback(gagval_widget, XmNactivateCallback, show_values, draw2_data);
 
  
 /*--------------------------------*/
 /*  Add window manager callbacks  */
 /*--------------------------------*/

 wmAtom = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", FALSE);
 XmAddWMProtocolCallback(shell, wmAtom, quit_post_from_WM, NULL);
 /*-----------------------------------------------------------------------------*/

 fill_postanalysis_pixmap(multi_sensor_canvas, draw_data, NULL);
 fill_postanalysis_pixmap(gageonly_canvas, draw2_data, NULL);
 
 XtUnmanageChild(working_shell);
 XtPopup(shell, XtGrabNone);

 rbdata.gc = xs_create_xor_gc(multi_sensor_canvas, "white");

 XtAddEventHandler(multi_sensor_canvas, ButtonPressMask,   FALSE, start_rubber_band, &rbdata);
 XtAddEventHandler(multi_sensor_canvas, ButtonMotionMask,  FALSE, track_rubber_band, &rbdata);
 XtAddEventHandler(multi_sensor_canvas, ButtonReleaseMask, FALSE, end_rubber_band,   &rbdata);
 XtAddEventHandler(gageonly_canvas,     ButtonPressMask,   FALSE, start_rubber_band, &rbdata);
 XtAddEventHandler(gageonly_canvas,     ButtonMotionMask,  FALSE, track_rubber_band, &rbdata);
 XtAddEventHandler(gageonly_canvas,     ButtonReleaseMask, FALSE, end_rubber_band,   &rbdata);

 XtAddEventHandler(multi_sensor_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
   zoom_widget);
 XtAddEventHandler(gageonly_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
   zoom_widget);

 XtAddEventHandler(multi_sensor_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
   remove_apw);

 XtAddEventHandler(multi_sensor_canvas, ButtonPressMask, FALSE, locate_main, draw_data);
 XtAddEventHandler(gageonly_canvas,     ButtonPressMask, FALSE, locate_main, draw2_data);

}

/********************************************* END display_post ************/


/***************************************************************************/
/*  FUNCTION NAME:   initialize_draw_data()                                */
/*       FUNCTION:   set parameters used to display fields                 */
/***************************************************************************

Function type:
   void

Called by function:
   display_post

Functions called:
   set_colorvalues
   get_pixel_by_name

******************************************** BEGIN initialize_draw_data ****/

void initialize_draw_data(data,type,w,x,y)
   draw_struct *data;
   int          type;
   Widget       w;
   int          x, y;
{
   int          i, j;
   XGCValues    gcv;
   int          mask = GCForeground;
   Display     *dpy;

 if (dbg) printf("In initialize draw_data\n");

 data->maximum_columns = x;
 data->maximum_rows = y;
 data->data_array = (short int **)malloc((data->maximum_columns)*sizeof(short int *));

 for (i=0; i<data->maximum_columns; i++)
    data->data_array[i] = (short int *)malloc((data->maximum_rows)*sizeof(short int));

 data->levels = (int *)malloc(17*sizeof(int));
 data->gc = (GC *)malloc(17*sizeof(GC));

 data->states_on = istate;
 data->rivers_on = iriver;
 data->basins_on = ibound;
 data->cities_on = icity;
 data->county_on = icounty;

 data->origin.x = XOR;
 data->origin.y = YOR;

 if (type == 0)
    {
    for (i=0; i<data->maximum_columns; i++)
    {
      for (j=0; j<data->maximum_rows; j++)
      {
        data->data_array[i][j] = (short) (precip24[j][i]/10); /*in 10mm unit*/        	   
      }
    }          		  
    }
 if (type == 1)
    {
    for (i=0; i<data->maximum_columns; i++)
    for (j=0; j<data->maximum_rows; j++)
       data->data_array[i][j] = gageonly2[j][i]; /*in 10mm  unit*/
    }

 set_colorvalues(data);
 data->pix = (Pixmap) NULL;
 data->pixbase = (Pixmap) NULL;
 data->w = w;
 dpy = XtDisplay(w);
 for (i=0; i<data->num_levels; i++)
    {
    gcv.foreground = get_pixel_by_name(w, color_list_levels[i]);
    data->gc[i] = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);
    }
}

/********************************************* END initialize_draw_data ****/



/*********************************************************************/
/*  FUNCTION NAME:   create_legend()                                 */
/*       FUNCTION:   creates and displays legend at bottom of main   */
/*                     post analysis window                          */
/**********************************************************************

Function type:
   void

Called by function:
   (callback) display_post

Functions called:
   get_pixel_by_name

******************************************** BEGIN create_legend ***********/

void create_legend(w, data, call_data)
   Widget                       w;
   draw_struct                 *data;
   XmDrawingAreaCallbackStruct *call_data;
{
   int                          i;
   short                        x, y;
   unsigned short               width, height;
   char                         str[10], strleg[50];
   GC                           textgc;
   XGCValues                    gcv;
   int                          mask = GCForeground;
   
 if (dbg) printf("In create legend\n");

 gcv.foreground = get_pixel_by_name(w, "white");
 textgc = XCreateGC(display, DefaultRootWindow(display), mask, &gcv);

 XSetFont(XtDisplay(w), textgc, XLoadFont(XtDisplay(w), "8x13"));

 width = 40.;
 height = 20.;
 y = 20;

 memset(strleg, '\0', 30);
 if(durcode == 6)
 {
   sprintf(strleg,"6 hour run ending at %s   rfc=%s",date_time.lldate,RFC);
 }
 else if(durcode == 48)
 {
   sprintf(strleg,"48 hour run ending at %s   rfc=%s",date_time.lldate,RFC);
 }
 else if(durcode == 24)
 {
   sprintf(strleg,"24 hour run ending at %s   rfc=%s",date_time.lldate,RFC);
 }

 for (i=0;i<data->num_levels;i++)
 {
    x = width*i;
    XFillRectangle(XtDisplay(w), XtWindow(w), data->gc[i], x, y, width, height);
 }

 sprintf(str,"0.00");
 XDrawString(XtDisplay(w), XtWindow(w), textgc, width+5, y-5, str, strlen(str));

 sprintf(str,"%.2f",level_value[1]);
 XDrawString(XtDisplay(w), XtWindow(w), textgc, 3*width-11, y-5, str, strlen(str));

 for (i=2;i<data->num_levels-2;i++)
 {
    x = (int) width*(i+1);
    sprintf(str,"%.2f",level_value[i]);
    if (level_value[i] > 6)
    {
      sprintf(str,"%.0f",level_value[i]);
      XDrawString(XtDisplay(w), XtWindow(w), textgc, x+width-5, y-5, str, strlen(str));
    }
    else
    {
      XDrawString(XtDisplay(w), XtWindow(w), textgc, x+width-11, y-5, str, strlen(str));
    }
 }

 /*XSetFont(XtDisplay(w),textgc,XLoadFont(XtDisplay(w),"ncenB14"));*/
 XDrawString(XtDisplay(w), XtWindow(w), textgc, 2, y+43, strleg, strlen(strleg));

}

