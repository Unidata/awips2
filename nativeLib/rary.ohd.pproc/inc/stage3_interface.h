/*=========================================================================*/
/*                    FILE PATH/NAME:   st3_includes/stage3_interface.h    */
/*                                                                         */
/*                              HEADER FILE                                */
/*=========================================================================*/
#ifndef STAGE3_INTERFACE_H
#define STAGE3_INTERFACE_H

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
#include "map_convert.h"

typedef struct {
		Widget          main_menuBar;
		Widget          main_canvas;
		Widget          control_mainMenuItem;
		Widget          display_mainMenuItem;
		Widget          overlays_mainMenuItem;
		Widget          options_mainMenuItem;
		Widget          drawprecip_mainMenuItem;
		Widget          mosaic_mainMenuItem;
		Widget          quit_widget;
		Widget          gage_table_widget;
		Widget          radar_site_widget;
		Widget          gage_widget;
		Widget          adap_widget;
		Widget          zoom_widget;
		Widget          states_widget;
		Widget          county_widget;
		Widget          cities_widget;
		Widget          basin_boundaries_widget;
		Widget          radar_rings_widget;
		Widget          rivers_widget;
		Widget          time_lapse_shell;
		Widget          time_delay_shell;
		Widget          single_site_popupShell;
		Widget          ss_adap_popupShell;
		Widget          multi_sensor_canvas;
		Widget          gageonly_canvas;
                Widget          stagei_canvas;
                Widget          stageii_canvas;
		Widget          site_ID_label;
		Widget          siibias_label;
		Widget          sibias_label;
		Widget          ss_gages_widget;
		Widget          legend_area;
		Widget          date_label;
		Widget          color_bar_canvas;
		Widget          field_type_label;
		Widget          ggfield_widget;
		Widget          mlfield_widget;
		Widget          s2field_widget;
		Widget          use_widget;
		Widget          close_widget;
		Widget          close_viewer_widget;
		Widget          single_gage_widget;
		Widget          adap_param_widget;
		Widget          edit_multi_sensor_field_button;
		Widget          edit_bias_value_button;
		Widget          edit_popupShell;
		Widget          edit_canvas;
		Widget          save_widget;
		Widget          rerun_widget;
		Widget          cancel_widget;
		Widget          undo_widget;
		Widget          pseudo_widget;
                Widget          next_widget;
                Widget          prev_widget;
                Widget          choose_widget;		
	       } stage3_widget_struct;

GC                              xs_create_xor_gc();
int                             ssnum;
float                           time_delay;

Widget				toplevel;
Widget                          main_shell;
Widget                          rerun_stii_widget;
Widget                          mulmean_widget;
Widget                          mulmax_widget;
Widget                          radm_widget;
Widget                          ggom_widget;

Widget                          drawpoly_widget;
Widget                          editpoly_widget;
Widget                          deletepoly_widget;
Widget                          applyexit_widget;
Widget                          noapplyexit_widget;

#endif /* #ifndef STAGE3_INTERFACE_H */
