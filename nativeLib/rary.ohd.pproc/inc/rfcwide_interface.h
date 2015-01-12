/*=========================================================================*/
/*                              FILE NAME:   rfcwide_interface.h           */
/*                                                                         */
/*                              HEADER FILE                                */
/*=========================================================================*/

#ifndef RFCWIDE_INTERFACE_H
#define RFCWIDE_INTERFACE_H

#include <stdio.h>
#include <time.h>
#include <X11/Intrinsic.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/DialogS.h>
#include <Xm/Text.h>
#include <Xm/Xm.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/DrawingA.h>
#include <Xm/Separator.h>
#include <Xm/MessageB.h>
#include <Xm/Scale.h>
#include "ifp_help.h"
#include "help.h"
#include "stage3.h"
#include "stage3_interface.h"

typedef struct {
		Widget          main_menuBar;
		Widget          main_canvas;
		Widget          control_mainMenuItem;
		Widget          display_mainMenuItem;
		Widget          display1_mainMenuItem;
		Widget          display2_mainMenuItem;
		Widget          overlays_mainMenuItem;
		Widget          options_mainMenuItem;
		Widget          drawprecip_mainMenuItem;
		Widget          quit_widget;
		Widget          gage_table_widget;
		Widget          radar_site_widget;
		Widget          gage_widget;
		Widget          adap_widget;
		Widget          suppl_widget;
		Widget          zoom_widget;
		Widget          states_widget;
		Widget          county_widget;
		Widget          cities_widget;
		Widget          basin_boundaries_widget;
		Widget          radar_rings_widget;
		Widget          rivers_widget;
		Widget          time_lapse_shell;
		Widget          time_delay_shell;
                Widget          stoptime_widget ;
                Widget          timelapse6_widget ;
                Widget          timelapse12_widget ;
                Widget          timelapse24_widget ;
                Widget          timelapseother_widget ;
		Widget          single_site_popupShell;
		Widget          multi_sensor_canvas;
		Widget          gageonly_canvas;
                Widget          stagei_canvas;
                Widget          stageii_canvas;
		Widget          site_ID_label;
		Widget          sibias_label;
		Widget          ss_gages_widget;
		Widget          legend_area;
		Widget          date_label;
		Widget          color_bar_canvas;
		Widget          close_widget;
		Widget          close_viewer_widget;
		Widget          single_gage_widget;
		Widget          adap_param_widget;
		Widget          suppl_data_widget;
		Widget          edit_multi_sensor_field_button;
		Widget          edit_bias_value_button;
		Widget          ignore_widget;
		Widget          ignore_dp_widget;
		Widget          edit_popupShell;
		Widget          edit_canvas;
		Widget          save_widget;
		Widget          rerun_widget;
		Widget          refresh_precip_widget;
		Widget          cancel_widget;
		Widget          undo_widget;
		Widget          pseudo_widget;
                Widget          next_widget;
                Widget          prev_widget;
                Widget          clear_widget;
                Widget          choose_widget;
                Widget          display_bias_widget;
                Widget          qc_precipitation;
                Widget          qc_temperature;
                Widget          qc_freezing;
                Widget          save_level2_data;
                Widget          gage_ids;
                Widget          gage_triangles;
                Widget          gage_values;
                Widget          gage_missing_menu;
                Widget          gage_color_menu;
                Widget          monthly_max_temp;
                Widget          monthly_min_temp;
                Widget          rfc_qpe_mosaic;
                Widget          transmit_rfc_qpe;
                Widget          transmit_rfc_bias;
		Widget          post_analysis_widget;

		/* added by zhan */
		Widget          restart_widget;

	        } rfcwide_widget_struct;

typedef struct {
		Pixmap          pixmap [ 24 ] ;
                date_struct     dates [ 24 ] ;
		int             max ;
		int             icnt ;
		Dimension       width , height ;
		GC             * gc ;
	       } loop_struct;

rfcwide_widget_struct          *widget_struct;
Widget                          toplevel, tophelp;
Display                         *display;
int                             ssnum;
float                           time_delay;
Widget                          main_shell;
Widget                          rerun_stii_widget;
Widget                          savemaintop_widget;
Widget                          savemainbottom_widget;

Widget                          drawpoly_widget;
Widget                          deletepoly_widget;
Widget                          editpoly_widget;
Widget                          deletepoly_widget;
Widget                          applyexit_widget;
Widget                          noapplyexit_widget;
Widget                          showids_widget;
Widget                          showval_widget;

Widget                          rmosaic_widget;
Widget                          avgrmosaic_widget;
Widget                          maxrmosaic_widget;
Widget                          bmosaic_widget;
Widget                          mmosaic_widget;
Widget                          mlmosaic_widget;
Widget                          p3lmosaic_widget;
Widget                          rawq2mosaic_widget;
Widget                          locbiasq2mosaic_widget;
Widget                          multiq2mosaic_widget;
Widget                          localfield1_widget;
Widget                          localfield2_widget;
Widget                          localfield3_widget;
Widget                          gageonly_widget;
Widget                          xmrg_widget;
Widget                          multihour_widget;
Widget                          height_widget;
Widget                          index_widget;
Widget                          locspan_widget;
Widget                          locbias_widget;
Widget                          prism_widget;
Widget                          satprecip_widget;
Widget                          lsatprecip_widget;
Widget                          sgmosaic_widget;
Widget                          srmosaic_widget;
Widget                          srgmosaic_widget;
Widget                          rfcbmosaic_widget;
Widget                          rfcmmosaic_widget;

//dual pol widgets
Widget rdmosaic_widget;
Widget  avgrdmosaic_widget;
Widget  maxrdmosaic_widget;
Widget  bdmosaic_widget;
Widget  ldmosaic_widget;
Widget  mdmosaic_widget;
Widget  mldmosaic_widget;
Widget  srdmosaic_widget;
Widget  srdgmosaic_widget;

Widget                          bias_widget;
Widget                          bias_shell;
Widget                          applyInfoPb[40];
Widget				timelapse_widget;
Widget                          fullscreen_widget;
Widget                          splitscreen_widget;
int                             applyInfoWidget;

/* Function prototypes. */
void create_rfcwide_interface ( Widget working_widget ) ;
void create_rfcwide_legend ( Widget w , XtPointer clientdata ,
                             XtPointer calldata ) ;
void create_time_lapse_popup_RFCW ( rfcwide_widget_struct * widgetStruct ) ;

#endif /* #ifndef RFCWIDE_INTERFACE_H */
