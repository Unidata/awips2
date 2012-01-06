/*******************************************************************************
* FILENAME:            create_ss_interface_rfcwide.c
* NUMBER OF MODULES:   11
* GENERAL INFORMATION:
*   MODULE 1:          toggle_overlays
* DESCRIPTION:         This routine keeps track whether or not the overlay
*                      was turned on or off.
*
*   MODULE 2:          create_ss_interface_rfcwide
* DESCRIPTION:         This routine sets up windows and buttons for single
*                      site option.
*
*   MODULE 3:          initialize_draw_data_rfcwide
* DESCRIPTION:         This routine prepares each panel in single site
*		       window for display.
*
*   MODULE 4:          missing_data_rfcwide
* DESCRIPTION:         This routine writes "Missing Data" message in single
*		       site window.
*
*   MODULE 6:          locate_ss_RFCW
* DESCRIPTION:         This routine displaying  lat/lon,
*                      HRAP coord, raw StageI, unbiased StageI,
*                      radar coverage  and radclim values of cursor location.
*
*   MODULE 7 :         edit_bias_value_RFCW
* DESCRIPTION:         This routine displays the edit window for
*                      editing the bias value.
*
*   MODULE 8 :         show_ss_gages_RFCW
* DESCRIPTION:         This routine is a callback to display gage locations
*                      on all panels of  single site window.
*
*   MODULE 9 :         popdown_singleSite
* DESCRIPTION:         This routine closes single radar site window.
*
*   MODULE 10:         cancel_window_single_site
* DESCRIPTION:         This routine cancels single radar site display window
*		       in select single site list.
*
* MODULE 11:           init_draw_data_free_memory
* DESCRIPTION:         This routine frees dynamically allocated
*                      memory used by four panes "Single Site Radar"
*                      pop down window when the application is closed.
*
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP Unix / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*   All             4/9/2002     Bryon Lawrence    Revision
*****************************************************************************/

#include <X11/cursorfont.h>
#include <Xm/CascadeB.h>
#include <Xm/Protocols.h>
#include <stdio.h>

#include "create_ss_interface_rfcwide.h"
#include "display_adapt_param.h"
#include "display_suppl_data.h"
#include "drawa.h"
#include "edit_bias_RFCW.h"
#include "get_mpe_colors.h"
#include "libXs.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "overlay.h"
#include "post_functions.h"
#include "read_adapt_param_RFCW.h"
#include "read_precip_data.h"
#include "read_radar_grids.h"
#include "read_rresult.h" /* For the IgnoreRadar and DontIgnoreRadar
                             definitions. */
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "xs_create_menu_buttons.h"
#include "Xtools.h"   /* needed for set title */

/********  GLOBAL VARIABLES************************************************/

extern int      ss_window_changed ;
extern int 	cv_duration ;
int 		single_radar_popup_flag = 0 ;
int             num_draw_array_elements = 0 ;
int             single_site_window_open_count = 0 ;
Widget		single_radar_popup ;

rfcwide_widget_struct * widget_struct_ss = NULL ;
extern draw_struct * ds_array [ 4 ] ;

static xs_menu_struct ss_Control_menu_struct[] =
   {
   {"Close", popdown_window_single_site, NULL, TRUE, PUSH_BUTTON,NULL, 0, NULL}
   };

static xs_menu_struct ss_Options_menu_struct[] =
   {
   {"Edit Bias Value", edit_bias_value_RFCW, NULL, TRUE, PUSH_BUTTON,NULL, 0, NULL},
   {"Ignore Radar", ignore_radar_RFCW, NULL, TRUE, PUSH_BUTTON,NULL, 0, NULL},
   {"Display Adaptable Param",  display_adapt_param, NULL, TRUE, PUSH_BUTTON,NULL, 0, NULL},
   {"Display Supplemental Data", display_suppl_data, NULL, TRUE, PUSH_BUTTON,NULL, 0, NULL}
   };

enum SsOverlays { SS_RFC, SS_STATE , SS_COUNTY , SS_CITY , SS_BASIN , SS_RIVER ,
                  SS_GAGE , SS_RING , NUM_SS_OVERLAYS } ;

static xs_menu_struct ss_Overlays_menu_struct [ NUM_SS_OVERLAYS ] =
   {
   {"RFC boundaries", toggle_overlays, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL},
   {"States", toggle_overlays, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL},
   {"County", toggle_overlays, NULL, TRUE,PUSH_BUTTON, NULL, 0, NULL},
   {"Cities/Towns", toggle_overlays, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"Basin boundaries", toggle_overlays, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"Rivers", toggle_overlays, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"Precip Gages", toggle_overlays, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"Radar umbrella", toggle_overlays, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
   };


/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   toggle_overlays
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if the following
* condition occurs:
*
* 1) Bad value in a case statement.
*
*******************************************************************************
*/

void toggle_overlays ( Widget w , XtPointer client_data ,
                       XtPointer call_data )
{
   draw_struct * pDrawMsData = NULL ;
   int overlay_and_ss_number ;
   int ss_number ;
   enum SsOverlays overlay ;

   /* The overlay number is stored in the least significant byte
      of the integer passed in through the client_data XtPointer.
      The single site radar number is the stored in the second to
      least significant byte. */
   overlay_and_ss_number = ( int ) client_data ;
   overlay = ( enum SsOverlays ) overlay_and_ss_number & 255 ;
   ss_number = overlay_and_ss_number ;
   ss_number >>= 8 ;
   ss_number &= 255 ;

   pDrawMsData = ( draw_struct * ) & ds_array [ MsData ] [ ss_number ] ;
   switch ( overlay )
   {
      case SS_RFC:

         if ( pDrawMsData->rfc_on == 1)
         {
            pDrawMsData->rfc_on = 0 ;
         }
         else
         {
            pDrawMsData->rfc_on = 1 ;
         }

         break ;

      case SS_STATE:

         if ( pDrawMsData->states_on == 1)
         {
            pDrawMsData->states_on = 0 ;
         }
         else
         {
            pDrawMsData->states_on = 1 ;
         }

         break ;

      case SS_COUNTY:

         if ( pDrawMsData->county_on == 1)
         {
            pDrawMsData->county_on = 0 ;
         }
         else
         {
            pDrawMsData->county_on = 1 ;
         }

         break ;

      case SS_CITY:

         if ( pDrawMsData->cities_on == 1)
         {
            pDrawMsData->cities_on = 0 ;
         }
         else
         {
            pDrawMsData->cities_on = 1 ;
         }

         break ;

      case SS_BASIN:

         if ( pDrawMsData->basins_on == 1)
         {
            pDrawMsData->basins_on = 0 ;
         }
         else
         {
            pDrawMsData->basins_on = 1 ;
         }

         break ;

      case SS_RIVER:

         if ( pDrawMsData->rivers_on == 1)
         {
            pDrawMsData->rivers_on = 0 ;
         }
         else
         {
            pDrawMsData->rivers_on = 1 ;
         }

         break ;

      case SS_GAGE:

         if ( pDrawMsData->gages_on == 1)
         {
            pDrawMsData->gages_on = 0 ;
         }
         else
         {
            pDrawMsData->gages_on = 1 ;
         }

         break ;


      case SS_RING:

         if ( pDrawMsData->rings_on == 1)
         {
            pDrawMsData->rings_on = 0 ;
         }
         else
         {
            pDrawMsData->rings_on = 1 ;
         }

         break ;

      default:

         flogMessage ( stderr , "\nIn routine \"toggle_overlays\":\n"
                            "bad value in case statement: %d\n" ,
                            overlay ) ;
         return ;
   }

   ss_window_changed = 1 ;
   MPEUtil_copy_area ( w , ( XtPointer ) ss_number , NULL , FALSE ) ;
}


/***************************************************************************
*       MODULE NUMBER:   2
*       MODULE NAME:   create_ss_interface_rfcwide()
*       PURPOSE:   Set up windows and buttons for single site option.
*                  Called by: (callback) OK button on select site popup
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   None
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                       HEADER FILE                DESCRIPTION
*   get_pixel_by_name          globals.h                  Sets the color for the
*							  item to be drawn.
*   xs_create_menu_buttons  xs_create_menu_buttons.h      Creates menu buttons
*                                                         for single site window.
*   missing_data_rfcwide    create_ss_interface_rfcwide.h Writes "Missing Data"
*                                                         message in single site
*                                                         window.
*   label_rawrad               post_functions.h           Writes "Raw Radar"
*                                                         label.
*   label_unbrad               post_functions.h           Writes
*							 "Stage I Adjusted Radar"
*                                                         label.
*   label_radclim              post_functions.h           Writes "Radar Climatology"
*						          label.
*   label_radcov               post_functions.h           Writes "Radar Coverage Map"
*							  label.
*   read_adapt_param_RFCW      read_adapt_param_RFCW.h    Reads adaptable parameters
*                                                         from the Informix
*                                                         DPAadapt table.
*   read_suppl_data            display_suppl_data.h       Reads the supplemental
*                                                         data from the DPARadar
*                                                         table for display by
*                                                         the supplemental data
*							  viewer.
*  (callback) fill_pixmap_radar_site  drawa.h
*  (callback) copy_area               post_functions.h
*  (callback) show_states             post_functions.h
*  (callback) show_county             post_functions.h
*  (callback) show_cities_and_towns   post_functions.h
*  (callback) show_basin_boundaries   post_functions.h
*  (callback) show_rivers             post_functions.h
*  (callback) show_ss_gages_RFCW      create_ss_interface_rfcwide.h
*  (callback) show_radar_rings        create_ss_interface_rfcwide.h
*  (callback) locate_ss_RFCW          create_ss_interface_rfcwide.h
*
* DATA FILES AND/OR DATABASE:
* This routine reads adaptable parameters from the Informix DPAadapt table
* the supplemental data from the DPARadar table.
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if the following
* condition occurs:
*
* 1) Failed dynamically allocate memory for data array ds_array [ MsData ].
* 2) Failed dynamically allocate memory for data array ds_array [ GgData ].
* 3) Failed dynamically allocate memory for data array ds_array [ St1iData ].
* 4) Failed dynamically allocate memory for data array ds_array [ St1iiData ].
* 5) Failed dynamically allocate memory for data structure widget_struct.
* 6) Failed to read adaptable parameters from DPAadapt table.
********************************************************************************
*/

void create_ss_interface_rfcwide ( )
{
   Display             * display = NULL ;
   draw_struct         * pNewDrawArray = NULL ;
   Window               root;
   char                 str[6];
   int                  overlay_and_ss_number ;
   int                  screen;
   int                  i , j , pixel , background , n = 0;
   int                  status;
   Arg                  wargs[20];
   Atom 	        WM_DELETE_WINDOW ;

   Widget               form, menuBar;
   Widget               Control_mainMenuItem, Options_mainMenuItem;
   Widget               Overlays_mainMenuItem;
   //Widget Help_mainMenuItem;
   Widget               stagei_frame,stagei_canvas,stageii_frame,stageii_canvas;
   Widget               drawing_area_rc, multi_sensor_frame, gageonly_frame;
   Widget               multi_sensor_canvas, gageonly_canvas;
   Widget               bottom_rc, site_ID_frame, site_ID_label,site_ID_rc;
   Widget               date_frame, date_label, date_rc;
   Widget               sibias_frame, sibias_label, sibias_rc;
   Widget               cascade[3];
   Widget               close_widget, adap_widget, suppl_widget;
   Widget               editbias_widget, ignore_widget;
   rfcwide_widget_struct *   pNewWidgetArray = NULL ;
   xs_menu_widget_struct *overlays_widgetStruct;
   XmString 		xm_str;

 /*-----------------------------------------------------*/
 /*  malloc arrays                                      */
 /*-----------------------------------------------------*/

 /* Search to determine if this radar site is already open. */
 /* If it is, set the focus on that radar site window. */

 display = XtDisplay(toplevel);

 if ( ssnum <= num_draw_array_elements )
 {
    if ( widget_struct_ss [ ssnum - 1 ].single_site_popupShell != NULL )
    {
       XSetInputFocus ( display ,
           XtWindow ( widget_struct_ss [ ssnum - 1 ].single_site_popupShell ) ,
           RevertToParent ,
           CurrentTime ) ;

           /* That's it.  There is nothing left to do. */
           return ;
    }
 }

 /* This radar has not been loaded yet.  Read the radar grids which
    correspond to it. */
 read_radar_grids ( ssnum - 1 ) ;

 mSetCursor ( M_WATCH ) ;

 /* Allocate memory for the draw_struct arrays.  An array must be
    created for the raw radar precipitation estimate, the mean field
    bias precipitation estimate, the radar coverage map, and the
    radar climatology map. */

 ++ single_site_window_open_count ;

 for ( i = MsData ; i <= St1iiData ; ++ i )
 {
    if ( ssnum > num_draw_array_elements )
    {
       pNewDrawArray = ( draw_struct * )
                        malloc ( ssnum * sizeof ( draw_struct ) ) ;

       if ( pNewDrawArray == NULL )
       {
          flogMessage ( stderr , "In routine \"create_ss_interface_rfcwide\":\n"
                             "An error was encountered while attempting\n"
                             "to dynamically allocate memory for a new\n"
                             "draw_struct array.\n" ) ;
          return ;
       }

       memset ( pNewDrawArray , 0 , ssnum * sizeof ( draw_struct ) ) ;

       /* Copy the former array into this new array. */
       for ( j = 0 ; j < num_draw_array_elements ; ++ j )
       {
          pNewDrawArray [ j ] = ds_array [ i ] [ j ]  ;
       }

       if ( ds_array [ i ] != NULL )
       {
          free ( ds_array [ i ] ) ;
       }

       ds_array [ i ] = pNewDrawArray ;

    }

    /* Initialize the radar number. Ssnum indicates the position of the
       radar name in the Single Site Radar Selection List.*/
    ds_array [ i ] [ ssnum - 1 ].ssnum = ssnum - 1 ;
    ds_array [ i ] [ ssnum - 1 ].levels = NULL ;
    ds_array [ i ] [ ssnum - 1 ].data_array = NULL ;
    ds_array [ i ] [ ssnum - 1 ].gc = NULL ;
 }

 /* Manage the array widget structures.  There needs to be one
    widget structure for each of the single site windows which are open. */
 if ( ssnum > num_draw_array_elements )
 {
    pNewWidgetArray = ( rfcwide_widget_struct * )
                      malloc ( ssnum * sizeof ( rfcwide_widget_struct ) ) ;

    if ( pNewWidgetArray == NULL )
    {
       flogMessage ( stderr , "\nIn routine \"create_ss_rfcwide_interface\":\n"
                          "Could not allocate memory for an array of %d\n"
                          "rfcwide_widget_struct structures. Could not\n"
                          "allocate %d bytes.\n" , ssnum ,
                           ssnum * sizeof ( rfcwide_widget_struct ) ) ;
       return ;
    }

    memset ( pNewWidgetArray , 0 , ssnum * sizeof ( rfcwide_widget_struct ) ) ;

    /* Copy the old widget structure into the new one. */
    for ( j = 0 ; j < num_draw_array_elements ; ++ j )
    {
       pNewWidgetArray [ j ] = widget_struct_ss [ j ] ;
    }

    if ( widget_struct_ss != NULL )
    {
       free ( widget_struct_ss ) ;
    }

    widget_struct_ss = pNewWidgetArray ;

    num_draw_array_elements = ssnum ;
 }

 -- ssnum ;

 /*-------------------------------------------------------------*/
 /* Start building the single site radar window.                */
 /*-------------------------------------------------------------*/

 root = DefaultRootWindow(display);
 screen = DefaultScreen(display);

 XtSetArg(wargs[n], XmNheight, 872); n++;
 XtSetArg(wargs[n], XmNwidth, 805); n++;
 XtSetArg(wargs[n], XmNmaxHeight, 872); n++;
 XtSetArg(wargs[n], XmNmaxWidth, 805); n++;
 XtSetArg(wargs[n], XmNminHeight, 872); n++;
 XtSetArg(wargs[n], XmNminWidth, 805); n++;

 single_radar_popup = XtCreatePopupShell("rfcwide_ss_shell",
 	                                 transientShellWidgetClass,
                                         toplevel, wargs, n);
 SetTitle(single_radar_popup, "Single Radar Site");

 widget_struct_ss [ ssnum ].single_site_popupShell = single_radar_popup;

 form = XtCreateManagedWidget("singleSite_main_form", xmFormWidgetClass,
 	single_radar_popup, NULL, 0);

 XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
 XtSetArg(wargs[1], XmNleftAttachment, XmATTACH_FORM);
 XtSetArg(wargs[2], XmNrightAttachment, XmATTACH_FORM);
 menuBar = XmCreateMenuBar(form, "singleSite_main_menuBar", wargs, 3);
 XtManageChild(menuBar);

 pixel = get_pixel_by_name(toplevel, "black");

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, menuBar); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNpacking, XmPACK_COLUMN); n++;
 XtSetArg(wargs[n], XmNnumColumns, 2); n++;
 drawing_area_rc = XtCreateManagedWidget("drawing_area_rc",
 				xmRowColumnWidgetClass, form, wargs, n);


 stagei_frame = XtCreateManagedWidget ( "stagei_frame" , xmFrameWidgetClass ,
 				drawing_area_rc , NULL , 0 ) ;
 stageii_frame = XtCreateManagedWidget("stageii_frame", xmFrameWidgetClass,
 				drawing_area_rc, NULL, 0);
 gageonly_frame = XtCreateManagedWidget("gageonly_frame", xmFrameWidgetClass,
 				drawing_area_rc, NULL, 0);
 multi_sensor_frame = XtCreateManagedWidget("multi_sensor_frame",
 				xmFrameWidgetClass, drawing_area_rc, NULL, 0);

 n = 0;
 XtSetArg(wargs[n], XtNbackground, pixel); n++;
 XtSetArg(wargs[n], XmNheight, 393); n++;
 XtSetArg(wargs[n], XmNwidth, 393); n++;
 XtSetArg(wargs[n], XmNmaxHeight, 393); n++;
 XtSetArg(wargs[n], XmNmaxWidth, 393); n++;
 multi_sensor_canvas = XtCreateManagedWidget("multi_sensor_canvas",
 			xmDrawingAreaWidgetClass, multi_sensor_frame, wargs, n);
 widget_struct_ss [ ssnum ].multi_sensor_canvas = multi_sensor_canvas;

 n = 0;
 XtSetArg(wargs[n], XtNbackground, pixel); n++;
 XtSetArg(wargs[n], XmNheight, 393); n++;
 XtSetArg(wargs[n], XmNwidth, 393); n++;
 XtSetArg(wargs[n], XmNmaxHeight, 393); n++;
 XtSetArg(wargs[n], XmNmaxWidth, 393); n++;
 gageonly_canvas = XtCreateManagedWidget("gageonly_canvas",
 			xmDrawingAreaWidgetClass, gageonly_frame, wargs, n);
 widget_struct_ss [ ssnum ].gageonly_canvas = gageonly_canvas;

 n = 0;
 XtSetArg(wargs[n], XtNbackground, pixel); n++;
 XtSetArg(wargs[n], XmNheight, 393); n++;
 XtSetArg(wargs[n], XmNwidth, 393); n++;
 XtSetArg(wargs[n], XmNmaxHeight, 393); n++;
 XtSetArg(wargs[n], XmNmaxWidth, 393); n++;
 stagei_canvas = XtCreateManagedWidget("stagei_canvas",
 			xmDrawingAreaWidgetClass, stagei_frame, wargs, n);
 widget_struct_ss [ ssnum ].stagei_canvas = stagei_canvas;

 n = 0;
 XtSetArg(wargs[n], XtNbackground, pixel); n++;
 XtSetArg(wargs[n], XmNheight, 393); n++;
 XtSetArg(wargs[n], XmNwidth, 393); n++;
 XtSetArg(wargs[n], XmNmaxHeight, 393); n++;
 XtSetArg(wargs[n], XmNmaxWidth, 393); n++;
 stageii_canvas = XtCreateManagedWidget("stageii_canvas",
 			xmDrawingAreaWidgetClass, stageii_frame, wargs, n);
 widget_struct_ss [ ssnum ].stageii_canvas = stageii_canvas;

 initialize_draw_data_rfcwide ( & ds_array [ MsData ] [ ssnum ] , 0 ,
                                multi_sensor_canvas ) ;
 initialize_draw_data_rfcwide ( & ds_array [ GgData ] [ ssnum ] , 1 ,
                                gageonly_canvas ) ;
 initialize_draw_data_rfcwide ( & ds_array [ St1iData ] [ ssnum ] , 2 ,
                                stageii_canvas ) ;
 initialize_draw_data_rfcwide ( & ds_array [ St1iiData ] [ ssnum ] , 3 ,
                                stagei_canvas ) ;
 ds_array [ MsData ] [ ssnum ].w = multi_sensor_canvas;
 ds_array [ GgData ] [ ssnum ].w = gageonly_canvas;
 ds_array [ St1iData ] [ ssnum ].w = stagei_canvas;
 ds_array [ St1iiData ] [ ssnum ].w = stageii_canvas;

 //XtAddEventHandler ( multi_sensor_canvas , ExposureMask , FALSE , copy_area ,
 //                    ( XtPointer ) ssnum ) ;
 //XtAddEventHandler ( gageonly_canvas , ExposureMask , FALSE , copy_area ,
 //                    ( XtPointer ) ssnum ) ;
 //XtAddEventHandler ( stagei_canvas , ExposureMask , FALSE , copy_area ,
  //                   ( XtPointer ) ssnum ) ;
 //XtAddEventHandler ( stageii_canvas , ExposureMask , FALSE , copy_area ,
  //                   ( XtPointer ) ssnum ) ;

 XtAddEventHandler(multi_sensor_canvas, ButtonPressMask, FALSE, locate_ss_RFCW,
 					& ds_array [ MsData ] [ ssnum ] ) ;
 XtAddEventHandler(gageonly_canvas,     ButtonPressMask, FALSE, locate_ss_RFCW,
 					& ds_array [ GgData ] [ ssnum ] ) ;
 XtAddEventHandler(stagei_canvas,     ButtonPressMask, FALSE, locate_ss_RFCW,
 					& ds_array [ St1iData ] [ ssnum ] ) ;
 XtAddEventHandler(stageii_canvas,     ButtonPressMask, FALSE, locate_ss_RFCW,
 					& ds_array [ St1iiData ] [ ssnum ] ) ;

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, drawing_area_rc); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
 bottom_rc = XtCreateManagedWidget("singleSite_bottom_rc",
 			xmRowColumnWidgetClass, form, wargs, n);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, bottom_rc); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XtNbackground, pixel); n++;

 XtSetArg(wargs[0], XtNbackground, &background);
 XtGetValues(bottom_rc, wargs, 1);

/*-----------------------------------------------*/
/*  set up site ID frame                         */
/*-----------------------------------------------*/
 site_ID_frame = XtCreateManagedWidget("site_ID_frame", xmFrameWidgetClass,
 			bottom_rc, NULL, 0);
 n = 0;
 XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
 site_ID_rc = XtCreateManagedWidget("site_ID_rc", xmRowColumnWidgetClass,
 			site_ID_frame, wargs, n);

 n = 0;
 xm_str = XmStringCreateLtoR("Radar:", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 XtCreateManagedWidget("site_ID_heading", xmLabelWidgetClass, site_ID_rc,
 			wargs, n);
 XmStringFree(xm_str);

 xm_str = XmStringCreate ( nexrad [ ssnum ].id ,
 			   XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 site_ID_label = XtCreateManagedWidget("site_ID_label",
 			xmLabelWidgetClass, site_ID_rc, wargs, n);
 widget_struct_ss [ ssnum ].site_ID_label = site_ID_label;
 XmStringFree(xm_str);

/*-----------------------------------------------*/
/*  set up date frame                            */
/*-----------------------------------------------*/
 date_frame = XtCreateManagedWidget("date_frame", xmFrameWidgetClass,
 			bottom_rc, NULL, 0);
 n = 0;
 XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
 date_rc = XtCreateManagedWidget("date_rc", xmRowColumnWidgetClass,
 			date_frame, wargs, n);

 n = 0;
 xm_str = XmStringCreate(date_st3.lldate, XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 date_label = XtCreateManagedWidget("date_label", xmLabelWidgetClass,
 			date_rc, wargs, n);
 widget_struct_ss [ ssnum ].date_label = date_label;
 XmStringFree(xm_str);

/*-----------------------------------------------*/
/*  set up stageI bias frame                     */
/*-----------------------------------------------*/
 sibias_frame = XtCreateManagedWidget("sibias_frame", xmFrameWidgetClass,
 			bottom_rc, NULL, 0);
 n = 0;
 XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
 sibias_rc = XtCreateManagedWidget("sibias_rc", xmRowColumnWidgetClass,
 			sibias_frame, wargs, n);

 n = 0;
 xm_str = XmStringCreateLtoR("Bias Value:", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 XtCreateManagedWidget("sibias_heading", xmLabelWidgetClass, sibias_rc, wargs, n);
 sprintf(str, "%5.2f",siibiasu[ssnum]);
 XmStringFree(xm_str);

 xm_str = XmStringCreate(str, XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 sibias_label = XtCreateManagedWidget("sibias_label", xmLabelWidgetClass,
 			sibias_rc, wargs, n);
 widget_struct_ss [ ssnum ].sibias_label = sibias_label;
 XmStringFree(xm_str);
 /*--------------------------------------------------------------*/
 /*     Create the pulldown main menus...                        */
 /*--------------------------------------------------------------*/

 Control_mainMenuItem = XmCreatePulldownMenu(menuBar,
 			"singleSite_Control_mainMenuItem", NULL, 0);
 Options_mainMenuItem = XmCreatePulldownMenu(menuBar,
 			"singleSite_Options_mainMenuItem", NULL, 0);
 Overlays_mainMenuItem = XmCreatePulldownMenu(menuBar,
 			"singleSite_Overlays_mainMenuItem", NULL, 0);

 /* -------------------------------------------------------------*/
 /*     Create cascade buttons the main menu items...            */
 /* -------------------------------------------------------------*/

 XtSetArg(wargs[0], XmNsubMenuId, Control_mainMenuItem);
 xm_str = XmStringCreate("Control", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[1], XmNlabelString, xm_str);
 cascade[0] = XmCreateCascadeButton(menuBar, "singleSite_Control_cascade",
 			wargs, 2);
 XmStringFree(xm_str);

 XtSetArg(wargs[0], XmNsubMenuId, Options_mainMenuItem);
 xm_str = XmStringCreate("Options", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[1], XmNlabelString, xm_str);
 cascade[1] = XmCreateCascadeButton(menuBar, "singleSite_Options_cascade",
 			wargs, 2);
 XmStringFree(xm_str);

 XtSetArg(wargs[0], XmNsubMenuId, Overlays_mainMenuItem);
 xm_str = XmStringCreate("Overlays", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[1], XmNlabelString, xm_str);
 cascade[2] = XmCreateCascadeButton(menuBar, "singleSite_Overlays_cascade",
 			wargs, 2);
 XtManageChildren(cascade, 3);
 XmStringFree(xm_str);

/* xm_str = XmStringCreate("Help", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[0], XmNlabelString, xm_str);
 XtSetArg(wargs[1], XmNmnemonic, 'H');
 Help_mainMenuItem = XmCreateCascadeButton(menuBar,
 			"singleSite_Help_mainMenuItem", wargs, 2);
 XmStringFree(xm_str);

 XtSetArg(wargs[0], XmNmenuHelpWidget,Help_mainMenuItem );
 XtSetValues(menuBar, wargs, 1);*/
// XtAddCallback(Help_mainMenuItem, XmNactivateCallback,
//	   popup_help_window, "SINGLESITE");

//XtManageChild(Help_mainMenuItem);

 /* -------------------------------------------------------------*/
 /*     Create pulldown menu items for 'Control'...              */
 /* -------------------------------------------------------------*/

 for(i = 0; i < XtNumber(ss_Control_menu_struct); i++) ss_Control_menu_struct[i].data = (caddr_t) ssnum ;
 xs_create_menu_buttons("", Control_mainMenuItem, ss_Control_menu_struct, XtNumber(ss_Control_menu_struct));

 /*--------------------------------------------------------------*/
 /*     Create pulldown menu items for 'Overlays'...             */
 /*--------------------------------------------------------------*/

 for(i = 0; i < XtNumber(ss_Overlays_menu_struct); i++)
 {
    overlay_and_ss_number = ssnum ;
    overlay_and_ss_number <<= 8 ;
    overlay_and_ss_number |= i ;
    ss_Overlays_menu_struct[i].data = (caddr_t) overlay_and_ss_number ;
 }

 overlays_widgetStruct =
    xs_create_menu_buttons("", Overlays_mainMenuItem, ss_Overlays_menu_struct,
    			XtNumber(ss_Overlays_menu_struct));

 /*--------------------------------------------------------------*/
 /*     Create pulldown menu items for 'Options'...              */
 /*--------------------------------------------------------------*/

 for(i = 0; i < XtNumber(ss_Options_menu_struct); i++)
    ss_Options_menu_struct[i].data = (caddr_t) ssnum ;

 xs_create_menu_buttons("", Options_mainMenuItem, ss_Options_menu_struct,
 			XtNumber(ss_Options_menu_struct));

 close_widget = XtNameToWidget(Control_mainMenuItem, "Close");

 if(close_widget != NULL)
 {
    widget_struct_ss [ ssnum ].close_widget = close_widget;
 }

 editbias_widget = XtNameToWidget(Options_mainMenuItem, "Edit Bias Value");
 widget_struct_ss [ ssnum ].edit_bias_value_button = editbias_widget;

 ignore_widget = XtNameToWidget(Options_mainMenuItem, "Ignore Radar");
 widget_struct_ss [ ssnum ].ignore_widget = ignore_widget;

 if ( iflign [ ssnum ] == IgnoreRadar )
 {
    xm_str = XmStringCreateLocalized ( "Unignore Radar" ) ;
    XtVaSetValues ( ignore_widget , XmNlabelString , xm_str , NULL ) ;
    XmStringFree(xm_str);
 }

 /* Test to determine if the radar site that this single site radar has been
    launched for is currently being ignored. */

 adap_widget = XtNameToWidget(Options_mainMenuItem,
		                   "Display Adaptable Param");
 widget_struct_ss [ ssnum ].adap_param_widget = adap_widget;

 suppl_widget = XtNameToWidget(Options_mainMenuItem,
                "Display Supplemental Data");
 widget_struct_ss [ ssnum ].suppl_data_widget = suppl_widget;


 /* Load the data into the base pixmaps for each of the 4 window panes. */
 fill_pixmap_radar_site ( stagei_canvas ,
		          & ds_array [ St1iData ] [ ssnum ] ,
                          NULL ) ;
 fill_pixmap_radar_site ( stageii_canvas ,
		           & ds_array [ St1iiData ] [ ssnum ] ,
                          NULL ) ;
 fill_pixmap_radar_site ( gageonly_canvas ,
		          & ds_array [ GgData ] [ ssnum ] ,
                          NULL ) ;
 fill_pixmap_radar_site ( multi_sensor_canvas ,
                          & ds_array [ MsData ] [ ssnum ] ,
			  NULL) ;

 cv_duration = cv_duration_mainwin;
 strcpy(cv_use,cv_use_mainwin);

 XtPopup(single_radar_popup, XtGrabNone);

 XGrabButton(XtDisplay(multi_sensor_canvas), AnyButton, AnyModifier,
	     XtWindow(multi_sensor_canvas),TRUE,
	     ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
	     GrabModeAsync, GrabModeAsync,
	     XtWindow(multi_sensor_canvas),
	     XCreateFontCursor(XtDisplay(multi_sensor_canvas),XC_crosshair));
 XGrabButton(XtDisplay(gageonly_canvas), AnyButton, AnyModifier,
	     XtWindow(gageonly_canvas),TRUE,
	     ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
	     GrabModeAsync, GrabModeAsync,
	     XtWindow(gageonly_canvas),
	     XCreateFontCursor(XtDisplay(gageonly_canvas),XC_crosshair));
 XGrabButton(XtDisplay(stagei_canvas), AnyButton, AnyModifier,
	     XtWindow(stagei_canvas),TRUE,
	     ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
	     GrabModeAsync, GrabModeAsync,
	     XtWindow(stagei_canvas),
	     XCreateFontCursor(XtDisplay(stagei_canvas),XC_crosshair));
 XGrabButton(XtDisplay(stageii_canvas), AnyButton, AnyModifier,
	     XtWindow(stageii_canvas),TRUE,
	     ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
	     GrabModeAsync, GrabModeAsync,
	     XtWindow(stageii_canvas),
	     XCreateFontCursor(XtDisplay(stageii_canvas),XC_crosshair));

 /*---------------------------------------------------*/
 /*   set up for submenu under Options                */
 /*   check for availability of adaptable parameters  */
 /*     and  supplemental data                        */
 /*---------------------------------------------------*/

 if (datafile[ssnum][2]==1)
    missing_data_rfcwide( & ds_array [ St1iData ] [ ssnum ] );

 if (datafile[ssnum][2]==1)
 {
    missing_data_rfcwide( & ds_array [ St1iiData ] [ ssnum ] );
    XtSetSensitive(adap_widget,FALSE);
    XtSetSensitive(suppl_widget,FALSE);
 }
 else
 {
    read_adapt_param_RFCW ( nexrad [ ssnum ].id,
                            datetime_radar_prod [ ssnum ],
                            & status );

    if ( status != 0 )

    {
       flogMessage ( stderr, "Informix error encounterd attempting to read\n"
                         "adaptable parameters from dpaadapt table.\n" );
       XtSetSensitive ( adap_widget, FALSE );
    }

    read_suppl_data ( nexrad [ ssnum ].id,
                      datetime_radar_prod [ssnum ],
                      & status );

    if ( status == 0 )
    {
       XtSetSensitive ( suppl_widget, FALSE );
    }

 }

 label_rawrad(stagei_canvas, & ds_array [ St1iData ] [ ssnum ] , NULL) ;
 label_unbrad(stageii_canvas,  & ds_array [ St1iiData ] [ ssnum ] , NULL) ;
 label_radclim(gageonly_canvas, & ds_array [ GgData ] [ ssnum ] , NULL) ;
 label_radcov(multi_sensor_canvas, & ds_array [ MsData ] [ ssnum ] , NULL) ;

 mSetCursor ( M_NORMAL ) ;

 WM_DELETE_WINDOW = XmInternAtom ( XtDisplay ( single_radar_popup ) ,
		                   "WM_DELETE_WINDOW" , False ) ;

 XmAddWMProtocolCallback ( single_radar_popup ,
		           WM_DELETE_WINDOW , popdown_window_single_site ,
                           ( XtPointer ) ssnum ) ;

 /* Indicate to the copy_area routine that the overlays will need
    to be drawn.  Force the overlays to be drawn. */
 ss_window_changed = 1 ;

 copy_area ( NULL , ( XtPointer ) ssnum , NULL , NULL ) ;
}

/***************************************************************************
*  MODULE NUMBER:   3
*  MODULE NAME:   initialize_draw_data_rfcwide
*  PURPOSE:   Prepares each panel in single site window for display.
*             Called by routine: create_ss_interface_rfcwide
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME      DESCRIPTION/UNITS
*   Input   draw_struct * data     User-supplied draw_struct structure.
*   Input  int            type     User-supplied data.
*   Input  Widget         w        The widget in which the callback originated.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE        DESCRIPTION
*
*   get_pixel_by_name             globals.h          Returns numeric color
*                                                    value by color name.
*   set_colorvalues               rfcwide.h          Reads color and value
*                                                    for each level in legend.
*                                                    from database or use
*                                                    default (hard coded)
*                                                    values.
*   init_draw_data_free_memory   create_ss_interface_rfcwide.h
*                                                    Frees dynamically allocated
*                                                    memory used by four panes
*                                                    "Single Site Radar"
*                                                    pop down window when the
*                                                    application is closed.
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if any of the following
* conditions occur:
*
* 1) Could not dynamically allocate blocks of memory of needed size for
*    data->data_array.
* 2) Could not dynamically allocate blocks of memory of needed size for
*    data->data_array[i].
* 3) Could not dynamically allocate blocks of memory of needed size for
*    data->gc data array.
*
********************************************************************************
*/

void initialize_draw_data_rfcwide (draw_struct * data , int type , Widget w )

{
   Display * pDisplay = NULL ;
   int          i, j;
   XGCValues    gcv;
   int          mask = GCForeground;
   double       dx, dy, dist;
   NamedColorSetGroup * pColors = NULL;

 data->maximum_columns = 131;
 data->maximum_rows = 131;

 data->w = w;

 if ( data->data_array == NULL )
 {
    data->data_array =
           (int **)malloc((data->maximum_columns)*sizeof(int *));

    if ( data->data_array == NULL)
    {
   	   flogMessage ( stderr , "In routine \"initialize_draw_data_rfcwide\":\n"
                              "An error was encountered while attempting to\n"
	   		      "dynamically allocate memory for "
                              "\"data->data_array\".\n"
                              "Aborting the display of the "
                              "\"Single Radar Site\" \n"
                              "popup window.\n");

	   init_draw_data_free_memory ( ) ;
	   return ;
    }

    for (i=0; i<data->maximum_columns; i++)
    {
       data->data_array[i] = (int *)malloc((data->maximum_rows)
                                                 *sizeof(int));
       if ( data->data_array[i] == NULL)
       {
    	   flogMessage ( stderr , "In routine \"initialize_draw_data_rfcwide\":\n"
                              "An error was encountered while attempting to\n"
			      "dynamically allocate memory for\n"
                              "\"data->data_array[i]\" where i is %d.\n"
                              "Aborting the display of the \"Single Radar \n"
                              "Site\" popup window.\n" , i ) ;

	   init_draw_data_free_memory ( ) ;
	   return ;
       }
    }

 }

 pColors = get_mpe_default_colors ( );

 if(type > 1)
 {
    strcpy(cv_use,"RMOSAIC");
    strcpy(data->cv_use, "RMOSAIC" );
    cv_duration = 3600;
    MPEGui_set_colorvalues(data, pColors);
 }
 else
 {
    strcpy(cv_use,"RADCLIM");
    strcpy(data->cv_use, "RADCLIM" );
    cv_duration = 0;
    MPEGui_set_colorvalues(data, pColors);
 }

 pDisplay = XtDisplay ( w ) ;

 if ( data->gc != NULL )
 {
    for ( i = 0 ; i < data->num_levels ; ++ i )
    {
       XFreeGC ( pDisplay , data->gc [ i ] ) ;
    }

    free ( data->gc ) ;
    data->gc = NULL ;
 }

 data->gc = (GC *)malloc(data->num_levels*sizeof(GC));

 if ( data->gc == NULL)
 {
    flogMessage ( stderr , "In routine \"initialize_draw_data_rfcwide\":\n"
                        "An error was encountered while attempting to\n"
                        "dynamically allocate memory for \"data->gc\"\n"
                        "data array.\n"
                        "Aborting the display of the \"Single Radar Site\" \n"
                        "popup window.\n");

    init_draw_data_free_memory ( ) ;
    return ;
 }

    data->rfc_on    = rad_data[0].rfc_on;
    data->states_on = rad_data[0].states_on;
    data->rivers_on = rad_data[0].rivers_on;
    data->basins_on = rad_data[0].basins_on;
    data->rings_on  = rad_data[0].rings_on;
    data->cities_on = rad_data[0].cities_on;
    data->county_on = rad_data[0].county_on;
    data->gages_on  = rad_data[0].gages_on;

 data->origin.x  = nexrad[data->ssnum].ctr.x - 65;
 data->origin.y  = nexrad[data->ssnum].ctr.y - 65;

 for (i=0;i<131;i++)
 for (j=0;j<131;j++)
 {
    dx = i - 66;
    dy = j - 66;
    dist = sqrt(dx*dx + dy*dy);

    if (dist <= nexrad[data->ssnum].ngrd)
    {
      if (type == 0) data->data_array[i][j] = radcov[data->ssnum][j][i];
      if (type == 1) data->data_array[i][j] = gageonly[data->ssnum][j][i];
      if (type == 2) data->data_array[i][j] = stage1i[data->ssnum][j][i];
      if (type == 3) data->data_array[i][j] = stage1u[data->ssnum][j][i];
    }
    else
       data->data_array[i][j] = -1;
    }

 data->pix = (Pixmap) NULL;
 data->pixbase = (Pixmap) NULL;


 for ( i = 0 ; i < data->num_levels ; i++ )
 {
    gcv.foreground = get_pixel_by_name ( w , color_list_levels [ i ] ) ;
    data->gc [ i ] = XCreateGC ( pDisplay , DefaultRootWindow ( pDisplay ) ,
                                 mask , & gcv ) ;
 }

}

/***************************************************************************
*  MODULE NUMBER:   4
*  MODULE NAME:   missing_data_rfcwide()
*  PURPOSE:   Writes "Missing Data" message in single site window.
*             Called by routine: create_ss_interface_rfcwide.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME      DESCRIPTION/UNITS
*   Input   draw_struct * data     User-supplied draw_struct structure.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE        DESCRIPTION
*
*   get_pixel_by_name             globals.h          Returns numeric color
*                                                    value by color name.
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/

void missing_data_rfcwide ( draw_struct * data )

{
   char        *string="Missing Data";
   char        *fontname="*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*";
   XFontStruct *font_info = NULL ;
   Display     *dpy = NULL ;
   GC           textgc;
   XGCValues    gcv;
   int          mask = GCForeground ;

   gcv.foreground = get_pixel_by_name(data->w,"yellow");
   dpy = XtDisplay(data->w);
   textgc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

   font_info = XLoadQueryFont(dpy, fontname);

   if ( font_info != NULL )
   {
      XSetFont(dpy, textgc, font_info->fid);
   }

   XDrawString(dpy, data->pix, textgc, 100, 180, string, strlen(string));
   XDrawString(dpy, data->pixbase, textgc, 100, 180, string, strlen(string));

   if ( font_info != NULL )
   {
      XFreeFont ( dpy , font_info ) ;
   }

   XFreeGC ( dpy , textgc ) ;

}

/*********************************************************************
*  MODULE NUMBER:   6
*  MODULE NAME:   locate_ss_RFCW()
*  PURPOSE:   Pop up locator shell displaying  lat/lon, HRAP coord,
*             raw StageI, unbiased StageI, radar coverage
*             and radclim values of cursor location
***************************************************************************
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME      DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XEvent *    event       The pointer to the event structure.
*   Input  Boolean *continue_to_dispatch_return
*                                  The pointer to the boolean variable.
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                           HEADER FILE        DESCRIPTION
*   HrapToLatLongMpe               stage3.h           Converts HRAP
*						      x,y  coordinates of the
*                                                     cursor location to
*						      lat / lon.
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/

void locate_ss_RFCW(Widget w, XtPointer clientdata,  XEvent * event ,
			Boolean *continue_to_dispatch_return )
{
   int          x, y, n, dx, dy;
   Arg          wargs[3];
   Dimension    width, height;
   int          x_pixels_per_bin, y_pixels_per_bin;
   float        value;
   point        hrap;
   HRAP         ll;
   char         str[80];
   XmString     msg;
   Widget       loc_shell, bb;
   Widget       text1, text2, text3, text4, text5, text6, text7;

   draw_struct * data = (draw_struct *) clientdata ;

 /*-------------------------------------------------------------------------*/
 /*     check to see if right button has been pushed                        */
 /*-------------------------------------------------------------------------*/

 if (event->xbutton.button != 3) return;

 /*-------------------------------------------------------------------------*/
 /*     check location of button in screen coordinates                      */
 /*-------------------------------------------------------------------------*/

 x = event->xbutton.x;
 y = event->xbutton.y;

 /*-------------------------------------------------------------------------*/
 /*     find location in HRAP coordinates                                   */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(w, wargs, n);

 x_pixels_per_bin = (float)width/(float)data->maximum_columns;
 y_pixels_per_bin = (float)height/(float)data->maximum_rows;
 if (x_pixels_per_bin > y_pixels_per_bin)
	x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin)
	y_pixels_per_bin = x_pixels_per_bin;

 hrap.x = data->origin.x + (x/x_pixels_per_bin);
 hrap.y = data->origin.y + (data->maximum_rows - y/y_pixels_per_bin - 1);

 loc_shell = XtCreatePopupShell("locator_shell",
	     transientShellWidgetClass, single_radar_popup, NULL, 0);

 SetTitle (loc_shell, "Location Data");

 bb = XtCreateManagedWidget("loc_bb",xmRowColumnWidgetClass,loc_shell,NULL,0);

 n=0;
 sprintf(str,"National HRAP: x=%d  y=%d", hrap.x, hrap.y );
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 text1 = XtCreateManagedWidget("loc_nhp",xmLabelWidgetClass, bb, wargs, n);
 XmStringFree(msg);

 ll=HrapToLatLongMpe(hrap);
 n=0;
 sprintf(str,"Latitude: %.2f   Longitude: %.2f",ll.y,ll.x );
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text2 = XtCreateManagedWidget("loc_ll",xmLabelWidgetClass, bb, wargs, n);
 XmStringFree(msg);

 dx = hrap.x - nexrad[data->ssnum].ctr.x + 66;
 dy = hrap.y - nexrad[data->ssnum].ctr.y + 66;
 if(dx == 0) dx=1;
 if(dy == 0) dy=1;

 n=0;
 sprintf(str,"Local HRAP: x=%d  y=%d", dx, dy );
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 text3 = XtCreateManagedWidget("loc_lhp",xmLabelWidgetClass, bb, wargs, n);
 XmStringFree(msg);

/*  value divided by 100 and divided by 25.4 to convert to inches in locator popup */

   value = (float)stage1i[data->ssnum][dy][dx]/100/25.4;
   if (value >= 0)
     sprintf(str,"Raw Radar: %.3f in", value);
   else
     sprintf(str,"Raw Radar: missing");

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text4 = XtCreateManagedWidget("loc_st1",xmLabelWidgetClass, bb, wargs, n);
 XmStringFree(msg);

   /* value = (float)stage1u[data->ssnum][dy][dx]; */
   value = (float)stage1u[data->ssnum][dy][dx]/100/25.4;
   if (value >= 0)
     sprintf(str,"Unbiased Radar: %.3f in", value);
   else
     sprintf(str,"Unbiased Radar: missing");

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text5 = XtCreateManagedWidget("loc_st2",xmLabelWidgetClass, bb, wargs, n);
 XmStringFree(msg);

   /*  value = (float)gageonly[data->ssnum][dy][dx];  */
   value = 0;
   if (value >= 0)
     sprintf(str,"Misbin: %.3f", value);
   else
     sprintf(str,"Misbin: missing");

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text6 = XtCreateManagedWidget("loc_gag",xmLabelWidgetClass, bb, wargs, n);
 XmStringFree(msg);

   /*  value = (float)Multi[data->ssnum][dy][dx];  */
   value = 0;
   if (value >= 0)
     sprintf(str,"Rad Clim: %.3f", value);
   else
     sprintf(str,"Rad Clim: missing");

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text7 = XtCreateManagedWidget("loc_mlt",xmLabelWidgetClass, bb, wargs, n);
 XmStringFree(msg);

 XtPopup(loc_shell,XtGrabNone);
 //XtAddEventHandler(w, ButtonReleaseMask, FALSE, close_locate, loc_shell);
}

/***************************************************************************
*  MODULE NUMBER:   7
*  MODULE NAME:   edit_bias_value_RFCW
*  PURPOSE:   Calls the function to display the edit window for
*             editing the bias value.
*             Called by routine: (callback) Edit Bias Value button.
***************************************************************************
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME      DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                           HEADER FILE        DESCRIPTION
*   create_editbias_popup_RFCW     edit_bias_RFCW.h   Creates popup window
*                                                     for editing bias value.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/

void edit_bias_value_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata)

{
   editbias_struct * data = ( editbias_struct * ) clientdata ;

   create_editbias_popup_RFCW ( w , data , calldata ) ;
}

/****************************************************************************
*  MODULE NUMBER:   8
*  MODULE NAME:   show_ss_gages_RFCW
*  PURPOSE:   Callback to display gage locations on all panels
*             of  single site window.
*             Called by routine: (callback) Precip Gages option under
*             Overlay menu item.
*****************************************************************************
* ARGUMENTS:
*   TYPE   DATA TYPE       NAME        DESCRIPTION/UNITS
*   Input  Widget          w           The widget in which the callback
*				       originated.
*   Input  draw_struct **  data        User-supplied calling data.
*   Input  caddr_t *       call_data   Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                           HEADER FILE        DESCRIPTION
*   get_pixel_by_name             globals.h          Returns numeric color
*                                                    value by color name.
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE                  NAME             DESCRIPTION
*      int 		      i                    An index for the array of
*      						   the structures.
*      int		      j	                   A loop index variable for
*                                                  number of gages.
*      Arg                 wargs [ ]               Arg structure, an array of
*						   arguments used for setting
* 						   widget resources,
*						   dimensioned to 5.
*     GC                    gc                     A pointer to the Graphic
*                                                  Context structure.
*     Display *             dpy                    A pointer to the Display
*                                                  structure.
*     int             mask = GCForeground          Mask to set the foreground
*					           component of GC.
*     Dimension            width                   Dimension structure, width
*                                                  of drawing area canvas.
*     Dimension            height                  Dimension structure, height
*                                                  of drawing area canvas.
*       int                  x,y                   Location of gage in pixel
*     						   units.
*     XGCValues              gcv                   Specifies the values
*						   for the fields in
*                                                  value_mask.
*       int               xloc, yloc               Location of gage on the
*                                                  single site window panel.
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*****************************************************************************
*/

void show_ss_gages_RFCW ( Widget w , XtPointer client_data ,
                          XtPointer call_data )

{
   char         str[2];
   int          i, j, n, xloc = 0, yloc = 0;
   int          ss_number = ( int ) client_data ;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   XGCValues    gcv;

 dpy = _get_map_display ( ) ;

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(ds_array[ MsData ] [ ss_number ].w, wargs, n);

 x = (float)width/(float) ds_array [ MsData ] [ ss_number ].maximum_columns;
 y = (float)height/(float) ds_array [ MsData ] [ ss_number ].maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 gcv.foreground = get_pixel_by_name(w,"SandyBrown");
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);
 XSetFont(dpy, gc, XLoadFont(dpy,
                   "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*"));

 sprintf(str,"+");

 for ( i = MsData ; i <= St1iiData ; ++ i )
 {
   /* Make sure that there is gage data loaded. */
   if ( gage == NULL )
   {
      ReadGageData_RFCW ( ) ;
   }

   for ( j = 0 ; j < ngages ;++j )
   {
     xloc = ( int ) ( ( gage[j].hrap.x - ( float ) nexrad[ss_number].ctr.x
             + 66.0 - 2.0 ) * ( float ) x );
     yloc = ( int ) ( ( ( float ) ds_array[i] [ ss_number ].maximum_rows -
             ( gage[j].hrap.y - ( float ) nexrad[ss_number].ctr.y
	     + 66.0 - 2.0 )) * ( float ) y );
     XDrawString(dpy,ds_array[i][ss_number].pix,gc,xloc,yloc,str,strlen(str));
   }
 }

 XFreeGC ( dpy , gc ) ;

}

/***************************************************************************
*  MODULE NUMBER:   9
*  MODULE NAME:   popdown_window_single_site
*  PURPOSE:   Popdown single radar site display window.
*             Called by routine: (callback) Close option of the menu.
***************************************************************************
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME      DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                           HEADER FILE        DESCRIPTION
*   init_draw_data_free_memory     create_ss_interface_rfcwide.h
*                                                    Frees dynamically allocated
*                                                    memory used by four panes
*                                                    "Single Site Radar"
*                                                    pop down window when the
*                                                    application is closed.
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/

void popdown_window_single_site ( Widget w , XtPointer clientdata ,
                                  XtPointer calldata )
{
   Display * pDisplay = NULL ;
   draw_struct * pDrawStruct = NULL ;
   int i ;
   int j ;
   int ss_number = ( int ) clientdata ;

   pDisplay = _get_map_display ( ) ;

   -- single_site_window_open_count ;

   XtPopdown(widget_struct_ss [ ss_number ].single_site_popupShell);
   XtDestroyWidget(widget_struct_ss [ ss_number ].single_site_popupShell);

   /* For this single site radar window, null out the memory
      used by it in the widget_struct_ss array and in the data array. */
   for ( j = MsData ; j <= St1iiData ; ++ j )
   {
      if ( ds_array [ j ] != NULL )
      {
         pDrawStruct = & ds_array [ j ] [ ss_number ] ;

         /* Free the data->data_array */
         if ( pDrawStruct->data_array != NULL )
         {
       	    for ( i = 0 ; ( pDrawStruct->data_array [ i ] != NULL ) &&
                          ( i < pDrawStruct->maximum_columns ) ; ++ i )
	    {
	       free ( pDrawStruct->data_array [ i ] ) ;
	       pDrawStruct->data_array [ i ] = NULL ;
	    }

	    free ( pDrawStruct->data_array ) ;
	    pDrawStruct->data_array = NULL ;
         }

         /* Deallocate the memory used by the graphics context. */
         if ( pDrawStruct->gc != NULL )
         {
            for ( i = 0 ; i < pDrawStruct->num_levels ; ++ i )
            {
               XFreeGC ( pDisplay , pDrawStruct->gc [ i ] ) ;
            }

            free ( pDrawStruct->gc ) ;
            pDrawStruct->gc = NULL ;
         }

         /* Deallocate the pixmaps associated with this particular
            single site window. */
         if ( pDrawStruct->pix != 0 )
         {
            XFreePixmap ( pDisplay , pDrawStruct->pix ) ;
         }

         if ( pDrawStruct->pixbase != 0 )
         {
            XFreePixmap ( pDisplay , pDrawStruct->pixbase ) ;
         }

         /* Null out the memory that was used by the draw_struct structure
            containing the data for the window being closed. */
            memset ( pDrawStruct , 0 , sizeof ( draw_struct ) ) ;
      }

   }

   /* Null out the memory that was used by the rfcwide_widget_struct
      structure corresponding to the window being closed. */
   memset ( & widget_struct_ss [ ss_number ] , 0 ,
            sizeof ( rfcwide_widget_struct ) ) ;

   /* If there are no more windows open, then free everything! */
   if ( single_site_window_open_count == 0 )
   {
      for ( j = MsData ; j <= St1iiData ; ++ j )
      {
         if ( ds_array [ j ] != NULL )
         {
            free ( ds_array [ j ] ) ;
            ds_array [ j ] = NULL ;
         }
      }

      if ( widget_struct_ss != NULL )
      {
         free ( widget_struct_ss ) ;
         widget_struct_ss = NULL ;
      }

      num_draw_array_elements = 0 ;
   }
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:   popdown_all_windows_single_site
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void popdown_all_windows_single_site ( Widget w , XtPointer clientdata ,
                                       XtPointer calldata )
{
   Display * pDisplay = NULL ;
   draw_struct * pDrawStruct = NULL ;
   rfcwide_widget_struct * pWidgetStruct = NULL ;
   int i ;
   int j ;
   int k ;
   int m ;

   pDisplay = _get_map_display ( ) ;

   if ( num_draw_array_elements == 0 )
   {
      return ;
   }

   single_site_window_open_count = 0 ;

   pWidgetStruct = widget_struct_ss ;

   for ( i = 0 ; i < num_draw_array_elements ; ++ i , ++ pWidgetStruct )
   {

      if ( pWidgetStruct->single_site_popupShell == NULL )
      {
         continue ;
      }

      XtPopdown(pWidgetStruct->single_site_popupShell);
      XtDestroyWidget(pWidgetStruct->single_site_popupShell);

      /* For this single site radar window, deallocate the memory
         used by it in the widget_struct_ss array and in the data array. */
      for ( j = MsData ; j <= St1iiData ; ++ j )
      {
         if ( ds_array [ j ] != NULL )
         {
            pDrawStruct = & ds_array [ j ] [ i ] ;

            /* Free the data->data_array */
            if ( pDrawStruct->data_array != NULL )
            {
               for ( k = 0 ; ( pDrawStruct->data_array [ k ] != NULL ) &&
                             ( k < pDrawStruct->maximum_columns ) ; ++ k )
               {
                  free ( pDrawStruct->data_array [ k ] ) ;
                  pDrawStruct->data_array [ k ] = NULL ;
               }

               free ( pDrawStruct->data_array ) ;
               pDrawStruct->data_array = NULL ;
            }

            /* Deallocate the memory used by the graphics context. */
            if ( pDrawStruct->gc != NULL )
            {
               for ( m = 0 ; m < pDrawStruct->num_levels ; ++ m )
               {
                  if ( pDrawStruct->gc [ m ] != NULL )
                  {
                     XFreeGC ( pDisplay , pDrawStruct->gc [ m ] ) ;
                  }
               }

               free ( pDrawStruct->gc ) ;
               pDrawStruct->gc = NULL ;
            }

            /* Deallocate the pixmaps associated with this particular
               single site window (just a reminder). */
            if ( pDrawStruct->pix != 0 )
            {
               XFreePixmap ( pDisplay , pDrawStruct->pix ) ;
            }

            if ( pDrawStruct->pixbase != 0 )
            {
               XFreePixmap ( pDisplay , pDrawStruct->pixbase ) ;
            }

            /* Null out the memory that was used by the draw_struct structure
               containing the data for the window being closed. */
            memset ( pDrawStruct , 0 , sizeof ( draw_struct ) ) ;
         }
      }
   }

   for ( j = MsData ; j <= St1iiData ; ++ j )
   {
      if ( ds_array [ j ] != NULL )
      {
         free ( ds_array [ j ] ) ;
         ds_array [ j ] = NULL ;
      }
   }

   if ( widget_struct_ss != NULL )
   {
      free ( widget_struct_ss ) ;
      widget_struct_ss = NULL ;
   }

   num_draw_array_elements = 0 ;
}

/****************************************************************************
*  MODULE NUMBER:   10
*  MODULE NAME:   cancel_window_single_site
*  PURPOSE:   This routine cancels single radar site display window from
*	      select single site list.
*             Called by routine: (callback) Cancel option of the menu.
*****************************************************************************
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME      DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                           HEADER FILE        DESCRIPTION
*   Sensitize
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/

void cancel_window_single_site ( Widget w, XtPointer clientdata, XtPointer calldata)
{
	Sensitize (  widget_struct->radar_site_widget ) ;
}

/**************************************************************************
*  MODULE NUMBER:   11
*  MODULE NAME:   init_draw_data_free_memory
*  PURPOSE:   This routine frees dynamically allocated memory used
*             by four panes "Single Site Radar" popup window
*             when the application is closed.
***************************************************************************
* ARGUMENTS:
*   None
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
*******************************************************************************
*/

void init_draw_data_free_memory ( )
{
    draw_struct * data = NULL ;
    Display * pDisplay = NULL ;
    int i ;
    int j ;

    pDisplay = _get_map_display ( ) ;

    for ( j = MsData ; j <= St1iiData ; ++ j )
    {
       if ( ds_array [ j ] != NULL )
       {
          data = ds_array [ j ] ;

          /* Free the data->data_array */
          if ( data->data_array != NULL )
          {
       	      for ( i = 0 ; ( data->data_array [ i ] != NULL)
			      && (i < data->maximum_columns ) ; ++ i )
	      {
	         free ( data->data_array [ i ] ) ;
	         data->data_array [ i ] = NULL ;
	      }

	      free ( data->data_array ) ;
	      data->data_array = NULL ;
          }

          /* Deallocate the memory used by the graphics context. */
          if ( data->gc != NULL )
          {
             for ( i = 0 ; i < data->num_levels ; ++ i )
             {
                XFreeGC ( pDisplay , data->gc [ i ] ) ;
             }

             free ( data->gc ) ;
             data->gc = NULL ;
          }

          free ( ds_array [ j ] ) ;
          ds_array [ j ] = NULL ;
       }

    }

    ds_array [ MsData ] = ds_array [ GgData ] = ds_array [ St1iData ] =
    			  ds_array [ St1iiData ] = NULL ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/create_ss_interface_rfcwide.c,v $";
 static char rcs_id2[] = "$Id: create_ss_interface_rfcwide.c,v 1.37 2007/02/22 16:05:24 lawrence Exp $";}
/*  ===================================================  */

}

