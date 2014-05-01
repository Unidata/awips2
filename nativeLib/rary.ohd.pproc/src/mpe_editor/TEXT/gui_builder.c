
/*******************************************************************************
* FILENAME:            gui_builder.c
* NUMBER OF MODULES:   19
* GENERAL INFORMATION:
*      MODULE 2:  _create_menu_referenceData
* DESCRIPTION:    This routine creates the Reference Data menu and its
*                 associated subelements.
*      MODULE 3:  _create_menu_product
* DESCRIPTION:    This routine creates the Product menu and its associated
*                 menu items.
*      MODULE 4:  _create_menu_mpe_control
* DESCRIPTION:    This routine creates the MpeControl menu and its associated
*                 items.
*      MODULE 5:  _create_menu_field
* DESCRIPTION:    This routine creates the Field menu and its associated
*                 items.
*      MODULE 6:  input
* DESCRIPTION:    This routine manages the popup menu that is displayed
*                 by right clicking on the mouse.
*      MODULE 7:  process_clicks
* DESCRIPTION:    This routine listens for double mouse clicks and performs
*                 the appropriate function based on which of the mouse
*                 buttons was clicked.
*      MODULE 8:  select_cb
* DESCRIPTION:    This routine monitors mouse button clicks and calls the
*                 appropriate callback routines based upon which mouse buttons
*                 are pressed and whether or not they are "double clicked".
*      MODULE 9:  exposelegend
* DESCRIPTION:    This is the map expose and redraw routine that is supplied
*                 to the map handling routines of the map library.
*      MODULE 10:  toggle_latlon
* DESCRIPTION:     This routine turns the lat/lon pointer tracking box on or
*                  off.
*      MODULE 11:  toggle_mpemenu
* DESCRIPTION:     This routine toggles the additional Mpe information on the
*                  the Mpe legend.  This additional information includes
*                  the HRAP bin coordinates, the latitude/longitude of the
*                  HRAP bin, the precipitation value of the bin, and the basin
*                  and county containing the Hrap bin.
*      MODULE 12:  popup_cb
* DESCRIPTION:     Contains logic to invoke the appropriate callbacks when
*                  the user selects an item in the popup menu.
*      MODULE 13:  _create_popup_menu
* DESCRIPTION:     Creates the popup menu.
*      MODULE 14:  mouse_motion_callback
* DESCRIPTION:     This routine provides additional functionality in addition
*                  to that povided by the map library for mouse motion
*                  event handling.
*      MODULE 15:  leave_window_callback
* DESCRIPTION:     This routine provides user-supplied functionality
*                  when the pointer leaves the map drawing area.
*
*      MODULE 16:  about_app_cb
* DESCRIPTION:     This routine produces a dialog window which contains
*                  information about Hydroview.  Specifically, it indicates
*                  the version and the date the version was implemented.
*                  the Hydroview launch GUI.  It is the workhorse when it
*                  comes to defining and building the Hydroview GUI.
*      MODULE 17:  redrawMap
* DESCRIPTION:     This routine redraws the Hydroview geography information.
*      MODULE 18:  enter_window
* DESCRIPTION:     The callback for when an EnterNotify event is generated.
*      MODULE 19:  draw_launch_gui
* DESCRIPTION:     This routine creates the Hydroview Launch GUI and defines
*                  the actions that are taken when its buttons are pressed.
*      MODULE 21:  init_mpe_data
* DESCRIPTION:     This routine initialized the MPE portion of the
*                  Mpe Editor application.
*      MODULE 22:  popdown_dialog_window
* DESCRIPTION:     This routine unmanages the working dialog box which
*                  keeps the user informed of the state of the start-up
*                  of the application.
*      MODULE 23:  set_parent_widget
* DESCRIPTION:     If the application will be a child of an already existing
*                  widget, then the parent widget of the map must be set
*                  by calling this routine.
*
* ORIGINAL AUTHOR:     Unknown
* CREATION DATE:       Unknown
* ORGANIZATION:        HSEB OHD
* MACHINE:             HP9000 - UNIX / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*       All         10/30/01     Bryon Lawrence    Restructured the code,
*                                                  modified the documentation.
*     12 & 13       06/30/04     Russell Erb
*                                                  modified the documentation.
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>

#include "add_pseudo_RFCW.h"
#include "arrow_button_callbacks.h"
#include "choose_date.h"
#include "choose_rfcwide_date.h"
#include "clear_data_RFCW.h"
#include "create_ss_interface_rfcwide.h"
#include "DbmsAccess.h"
#include "delete_polygons_show.h"
#include "display_bias_table.h"
#include "display_field.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "drawMpeLegend.h"
#include "draw_precip_poly_RFCW.h"
#include "gageqc_gui.h"
#include "GeneralUtil.h"
#include "get_mpe_product_state.h"
#include "gui_builder.h"
#include "HvCallbacks.h"
#include "HvColorList.h"
#include "HvDisplayControlProto.h"
#include "hv_mainCallbacks.h"
#include "HydroStatus.h"
#include "initMpeControl.h"
#include "launch_colormanager.h"
#include "launch_fieldgen_dialog.h"
#include "map.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_menubar.h"
#include "map_menubar_cb.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "newhour_RFCW.h"
#include "overlay.h"
#include "pointcontrol_mgr.h"
#include "polygon_RFCW.h"
#include "post_functions.h"
#include "read_overlay_configuration.h"
#include "rerun_rfcwgen.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "save_rfcwide.h"
#include "select_site_rfcwide.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "TechInfo.h"
#include "time_lapse_RFCW.h"
#include "version_info.h"
#include "arrow_button_callbacks.h"
#include "mpe_bad_gage_list_show.h"



/* Enum mpe_control_items defines the menu items created on the
   "MpeControl" menu. */
enum mpe_tool_items
{ MpeViewMenu, SplitScreenItem, FullScreenItem,
   SetMPEcolorsItem, MPEColorManagerItem,
   RestoreInitialItem, FindStationItem,
   MpeDisplayMenu, DisplayImageItem,
   DisplayContourItem, NumToolItems
};

enum mpe_control_items
{ NextHourItem, PrevHourItem, ChooseHourItem,
   UpdateBestQPETopItem, UpdateBestQPEBottomItem,
   RegenerateHourFieldsItem, ClearDataItem,
   TransmitQpeItem, TransmitBiasItem,
   NumMpeControlItems
};

/* Enum ref_menu_items defines the menu items created on the
   "ReferenceData" menu. */
enum ref_menu_items
{ StaffGage, ImpactStatement, RatingCurve,
   DataSources, Contacts, CrestHistory,
   TextReports, NumRefItems
};

/* Enum field_items defines the menu items created on the
   "FieldData" menu. */
enum precip_field_menu_items
{ RadarMosaicItem, AvgRadarMosaicItem,
   MaxRadarMosaicItem, FieldBiasMosaicItem,
   LocalBiasMosaicItem, MultiMosaicItem,
   LocalMultiMosaicItem, TriLocalMosaicItem,
   BestEstimQPEItem, SatellitePrecipItem,
   LocalSatPrecipItem, SatRadarPrecipItem,
   SatGagePrecipItem, SatRadarGagePrecipItem,
   GageOnlyItem, RfcBMosaicItem, RfcMMosaicItem,
   RfcQpeItem, NumFieldItems
};

enum base_field_menu_items
{ LocalSpanItem, LocalBiasItem, HeightFieldItem,
   IndexFieldItem, GageTrianglesItem, NumBaseFieldItems
};

/*enum climo_menu_items { MonthlyPrecipItem, MonthlyMaxTempItem,
                        MonthlyMinTempItem, NumClimoItems }; */

enum polygons_menu_items
{ DrawPolygonItem, DeletePolygonItem, NumPolygonItems };

enum misc_menu_items
{ DisplayBiasTableItem, ReviewHourlyRadarItem,
   TimeLapseMenu, TimeLapse6, TimeLapse12, TimeLapse24,
   TimeLapseOther, TimeLapseEndLoop, MultiHourQPEItem,
   NumMiscItems
};

enum gages_menu_items
{ QCPrecipitationItem, QCTemperatureItem, QCFreezingItem,
   PseudoGageItem, GageTableItem, Display7x7Item,
   GageColorMenu, GageColorSolid, GageColorContrast,
   GageColorQC, GageColorByValue, GageMissingMenu,
   GageMissingAll, GageMissingReported, GageMissingNone,
   GageValuesItem, BadGagesItem, GageIdentifiersItem,
   SaveLevel2DataItem, NumGagesItems
};

/* Widget definitions. */
Widget cascade;
Widget gage_cascade;
Widget gage_color_cascade;
Widget gage_missing_cascade;
Widget timelapse_cascade;
Widget menu;
Widget mpeBase[NumBaseFieldItems];
/*Widget mpeClimo [ NumClimoItems ];*/
Widget mpeControl[NumMpeControlItems];
Widget mpeFields[NumFieldItems];
Widget mpeGages[NumGagesItems];
Widget mpeMisc[NumMiscItems];
Widget mpePolygons[NumPolygonItems];
Widget mpeTools[NumToolItems];
static Widget parent = NULL;
Widget popup_menu;
Widget product_menu;

Widget view_cascade;

Widget toplevel;

/* Global variables involved with the production of the latitude/longitude
   box. */
int copy = 0;
int image_x_pos;		/* Contains the x position of the last lat_lon_image. */
int image_y_pos;		/* Contains the y position of the last lat_lon_image. */
int show_latlon;		/* A flag indicating whether or not to display the
				   latitude / longitude pointer tracking box. */
extern int toolbar;		/* Contains the flag indicating whether or not the
				   user wants the lat/lon information displayed. */
Pixmap last_pixmap;		/* This pixmap contains a "snapshot" of the map
				   area overwritten by the lat/lon box. */

/* Variables related to the mouse click functionality. */
static Boolean button_one_down = False;
static int click_count = 0;
const int time_interval = 300;
static clicks mouse_clicks;

/* Variable to keep track of when a focus event has just occurred. */
enum MapState focus_flag = M_ON;

/* Variables related to the lat/lon tracking box. */
const int LAT_LON_IMAGE_WIDTH = 75;
const int LAT_LON_IMAGE_HEIGHT = 45;
const int LAT_LON_X_OFFSET = 10;
const int LAT_LON_Y_OFFSET = 0;
const int SIZE_OF_LAT_LON_STRING = 10;
const int SIZE_OF_HRAP_POINT_STRING = 256;


extern int prev_hour_action_check;
extern int next_hour_action_check;


extern int flf_on;
extern int group_edit;
extern int maxmin_on;
extern int qpf_on;


static void
_update_map_lib_tools ()
{
   extern Widget map_menu_items[num_of_menu_items];

   /* Move the clear data item to the Tools menu. */
   mCreateMenuItem (map_menu_items[map_tool_pane],
		    &mpeTools[MPEColorManagerItem], "MPE Color Manager ...",
		    'M', NULL, NULL);

   mCreateMenuItem (map_menu_items[map_tool_pane],
		    &mpeTools[SetMPEcolorsItem], "Set Colors (deprecated)...",
		    'C', NULL, NULL);

   mCreateMenuSeparator (map_menu_items[map_tool_pane]);

   _create_cascade (map_menu_items[map_tool_pane], &mpeTools[MpeViewMenu],
		    &view_cascade, "View...", 'V');
   mCreateMenuItem (mpeTools[MpeViewMenu],
		    &mpeTools[FullScreenItem],
		    "Full Screen", 'F', NULL, NULL);
   mCreateMenuItem (mpeTools[MpeViewMenu],
		    &mpeTools[SplitScreenItem],
		    "Split Screen", 'S', NULL, NULL);

   _create_cascade (map_menu_items[map_tool_pane], &mpeTools[MpeDisplayMenu],
		    &view_cascade, "Display Data As...", 'D');
   _create_menu_item_toggle (mpeTools[MpeDisplayMenu],
			     &mpeTools[DisplayImageItem], "Image", 'I', NULL,
			     NULL);
   _create_menu_item_toggle (mpeTools[MpeDisplayMenu],
			     &mpeTools[DisplayContourItem], "Contour", 'C',
			     NULL, NULL);

   mCreateMenuItem (map_menu_items[map_tool_pane],
		    &mpeTools[RestoreInitialItem], "Restore Initial", 'R',
		    NULL, NULL);

   mCreateMenuSeparator (map_menu_items[map_tool_pane]);

   mCreateMenuItem (map_menu_items[map_tool_pane], &mpeTools[FindStationItem],
		    "Find Station...", 'i', NULL, NULL);

   /* Add the callbacks for the Tools menu items. */
   XtAddCallback (mpeTools[FullScreenItem], XmNactivateCallback,
		  full_screen_callback, NULL);
   XtAddCallback (mpeTools[SplitScreenItem], XmNactivateCallback,
		  split_screen_callback, NULL);
   XtAddCallback (mpeTools[SetMPEcolorsItem], XmNactivateCallback,
		  launchColorManager, NULL);
   XtAddCallback (mpeTools[MPEColorManagerItem], XmNactivateCallback,
		  launch_color_manager_callback, NULL);
   XtAddCallback (mpeTools[DisplayImageItem], XmNvalueChangedCallback,
		  display_image_callback, (XtPointer) DISPLAY_AS_IMAGE);
   XtAddCallback (mpeTools[DisplayContourItem], XmNvalueChangedCallback,
		  display_image_callback, (XtPointer) DISPLAY_AS_CONTOUR);

   /* Set the intial states of the image and contour toggle buttons. */
   XmToggleButtonSetState (mpeTools[DisplayImageItem], True, False);
   XmToggleButtonSetState (mpeTools[DisplayContourItem], False, False);

   /* Still need callbacks for the clear data, restore initial, and find station
      items. */

   return;
}


/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   _create_menu_mpe_control
* PURPOSE:       This routine creates the MPE Control Menu.  This was added to
*                Hydromap as the first step towards integrating the Hydromap
*                and MPE applications.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                HEADER FILE         DESCRIPTION
*   mCreateMenu         map_library.h       Creates a drop-down menu.
*   mCreateMenuItem     map_library.h       Creates an item on a drop-down
*                                           menu.
*   XtAddCallback       Xm/Xm.h             Adds a callback to a Widget.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*   None
*
********************************************************************************
*/
static void
_create_menu_mpe_control ()
{
   mCreateMenu (&menu, &cascade, "MPEcontrol", 'M');
   mCreateMenuItem (menu, &mpeControl[NextHourItem],
		    "Next Hour", 'N', "Ctrl<Key>N", "Ctrl+N");
   mCreateMenuItem (menu, &mpeControl[PrevHourItem],
		    "Previous Hour", 'P', "Ctrl<Key>P", "Ctrl+P");
   mCreateMenuItem (menu, &mpeControl[ChooseHourItem],
		    "Choose Hour...", 'C', NULL, NULL);

   mCreateMenuSeparator (menu);

   mCreateMenuItem (menu, &mpeControl[UpdateBestQPETopItem],
		    "Save Best Estimate Top", 'S', "Ctrl<Key>S", "Ctrl+S");
   mCreateMenuItem (menu, &mpeControl[UpdateBestQPEBottomItem],
		    "Save Best Estimate Bottom", 'B', "Ctrl<Key>U", "Ctrl+U");
   mCreateMenuItem (menu, &mpeControl[RegenerateHourFieldsItem],
		    "Regenerate Hour Fields...", 'R', "Ctrl<Key>F", "Ctrl+F");
   mCreateMenuSeparator (menu);
   mCreateMenuItem (menu, &mpeControl[ClearDataItem], "Clear MPE Data",
		    'l', NULL, NULL);
   mCreateMenuSeparator (menu);
   mCreateMenuItem (menu, &mpeControl[TransmitQpeItem],
		    "Transmit Best Estimate QPE", 'T', "Ctrl<Key>T",
		    "Ctrl+T");
   mCreateMenuItem (menu, &mpeControl[TransmitBiasItem], "Transmit RFC Bias",
		    'a', "Ctrl<Key>A", "Ctrl+A");


   /* Add the callbacks to the MPEcontrol menu items. */
   XtAddCallback (mpeControl[NextHourItem], XmNactivateCallback, newhour_RFCW,
		  (XtPointer) NextHour);
   XtAddCallback (mpeControl[PrevHourItem], XmNactivateCallback, newhour_RFCW,
		  (XtPointer) PrevHour);
   XtAddCallback (mpeControl[ChooseHourItem], XmNactivateCallback,
		  display_rfcwide_date_window, NULL);
   XtAddCallback (mpeControl[UpdateBestQPETopItem], XmNactivateCallback,
		  save_rfcwide, (XtPointer) 0);
   XtAddCallback (mpeControl[UpdateBestQPEBottomItem], XmNactivateCallback,
		  save_rfcwide, (XtPointer) 1);
   XtAddCallback (mpeControl[RegenerateHourFieldsItem], XmNactivateCallback,
		  launch_fieldgen_dialog, NULL);
   XtAddCallback (mpeControl[ClearDataItem], XmNactivateCallback,
		  clear_data_RFCW, NULL);
   XtAddCallback (mpeControl[TransmitQpeItem], XmNactivateCallback,
		  transmit_rfc_qpe, NULL);
   XtAddCallback (mpeControl[TransmitBiasItem], XmNactivateCallback,
		  transmit_rfc_bias, NULL);

}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   _create_menu_precip_fields
* PURPOSE:       This routine creates the Precip Fields Menu and its associated
*                subelements.  The callbacks for the field menu items are
*                assigned within this routine as well.This menu was added to
*                Hydromap from MPE application.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                   HEADER FILE       DESCRIPTION
*   mCreateMenu            map_library.h     Creates a menu.
*   mCreateMenuItem        map_library.h     Creates an item on the menu.
*   XtAddCallback          Xm/Xm.h           Adds a callback to a Widget.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/
static void
_create_menu_precip_fields ()
{
   static int map_number = 0;

   mCreateMenu (&menu, &cascade, "PrecipFields", 'P');
   mCreateMenuItem (menu, &mpeFields[RadarMosaicItem],
		    "Radar Mosaic", 'R', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[AvgRadarMosaicItem],
		    "Average Radar Mosaic", 'A', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[MaxRadarMosaicItem],
		    "Max Radar Mosaic", 'x', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[FieldBiasMosaicItem],
		    "Field Bias Radar Mosaic", 'F', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[LocalBiasMosaicItem],
		    "Local Bias Radar Mosaic", 'L', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[GageOnlyItem],
		    "Gage Only Analysis", 'G', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[MultiMosaicItem],
		    "Multisensor Mosaic", 'M', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[LocalMultiMosaicItem],
		    "Local Bias Multisensor Mosaic", 'a', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[SatellitePrecipItem],
		    "Satellite Precip", 'S', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[LocalSatPrecipItem],
		    "Local Bias Satellite Precip", 'o', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[SatRadarPrecipItem],
		    "Satellite Radar Mosaic", 'e', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[SatGagePrecipItem],
		    "Satellite Gage Mosaic", 't', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[SatRadarGagePrecipItem],
		    "Satellite Radar Gage Mosaic", 'l', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[TriLocalMosaicItem],
		    "Triangulated Local Bias Mosaic", 'T', NULL, NULL);

   mCreateMenuSeparator (menu);
   mCreateMenuItem (menu, &mpeFields[BestEstimQPEItem],
		    "Best Estimate QPE", 'Q', NULL, NULL);

   mCreateMenuSeparator (menu);
   mCreateMenuItem (menu, &mpeFields[RfcBMosaicItem],
		    "RFC Field Bias Mosaic", 'P', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[RfcMMosaicItem],
		    "RFC Multisensor Mosaic", 'i', NULL, NULL);
   mCreateMenuItem (menu, &mpeFields[RfcQpeItem], "RFC QPE Mosaic", 'C',
		    NULL, NULL);

   XtAddCallback (mpeFields[RadarMosaicItem], XmNactivateCallback,
		  display_rmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[AvgRadarMosaicItem], XmNactivateCallback,
		  display_avgrmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[MaxRadarMosaicItem], XmNactivateCallback,
		  display_maxrmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[FieldBiasMosaicItem], XmNactivateCallback,
		  display_bmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[LocalBiasMosaicItem], XmNactivateCallback,
		  display_lmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[MultiMosaicItem], XmNactivateCallback,
		  display_mmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[LocalMultiMosaicItem], XmNactivateCallback,
		  display_mlmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[TriLocalMosaicItem], XmNactivateCallback,
		  display_p3lmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[BestEstimQPEItem], XmNactivateCallback,
		  display_xmrg_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[GageOnlyItem], XmNactivateCallback,
		  display_gageonly_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[SatellitePrecipItem], XmNactivateCallback,
		  display_satpre_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[LocalSatPrecipItem], XmNactivateCallback,
		  display_lsatpre_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[SatGagePrecipItem], XmNactivateCallback,
		  display_sgmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[SatRadarPrecipItem], XmNactivateCallback,
		  display_srmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[SatRadarGagePrecipItem], XmNactivateCallback,
		  display_srgmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[RfcBMosaicItem], XmNactivateCallback,
		  display_rfcbmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[RfcMMosaicItem], XmNactivateCallback,
		  display_rfcmmosaic_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeFields[RfcQpeItem], XmNactivateCallback,
		  display_rfcmosaic_RFCW, (XtPointer) & map_number);
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   _create_menu_base_fields
* PURPOSE:       This routine creates the BaseFields Menu and its associated
*                subelements.  The callbacks for these menu items are
*                assigned within this routine as well. The items on this menu
*                are the base products used in the computation of the MPE QPE mosaics
*                and fields.  The are often created before the QPE fields are generated
*                and then stored to disk.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                   HEADER FILE       DESCRIPTION
*   mCreateMenu            map_library.h     Creates a menu.
*   mCreateMenuItem        map_library.h     Creates an item on the menu.
*   XtAddCallback          Xm/Xm.h           Adds a callback to a Widget.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

static void
_create_menu_base_fields ()
{
   static int map_number = 0;

   mCreateMenu (&menu, &cascade, "BaseFields", 'B');

   mCreateMenuItem (menu, &mpeBase[LocalSpanItem],
		    "Local Span", 'L', NULL, NULL);
   mCreateMenuItem (menu, &mpeBase[LocalBiasItem],
		    "Local Bias", 'o', NULL, NULL);

   mCreateMenuItem (menu, &mpeBase[HeightFieldItem],
		    "Height Field", 'H', NULL, NULL);
   mCreateMenuItem (menu, &mpeBase[IndexFieldItem],
		    "Radar Coverage Field", 'R', NULL, NULL);

   _create_menu_item_toggle (menu, &mpeBase[GageTrianglesItem],
			     "Gage Triangles", 'G', NULL, NULL);

   /* Add the callbacks to these menu items. */
   XtAddCallback (mpeBase[LocalSpanItem], XmNactivateCallback,
		  display_locspan_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeBase[LocalBiasItem], XmNactivateCallback,
		  display_locbias_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeBase[HeightFieldItem], XmNactivateCallback,
		  display_height_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeBase[IndexFieldItem], XmNactivateCallback,
		  display_index_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeBase[GageTrianglesItem], XmNvalueChangedCallback,
		  set_mpe_gage_triangles, NULL);
}


/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   _create_menu_climo
* PURPOSE:       Contains the climatological reference fields for precipitation,
*                temperature, and freezing level.  These are from the Parameter
*                Regressions on Independent Slopes Model (PRISM) developed by
*                Dr. Daly at Oregon State University.
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
static void
_create_menu_climo ()
{
   static int map_number = 0;


   mCreateMenu (&menu, &cascade, "Climo", 'C');


   /* make this items as toggle button */

   _create_menu_item_toggle (menu, &mpeClimo[MonthlyPrecipItem],
			     "Monthly Prism Precip", 'P', NULL, NULL);

   _create_menu_item_toggle (menu, &mpeClimo[MonthlyMaxTempItem],
			     "Monthly Prism Max Temp", 'x', NULL, NULL);

   _create_menu_item_toggle (menu, &mpeClimo[MonthlyMinTempItem],
			     "Monthly Prism Min Temp", 'n', NULL, NULL);

   XtAddCallback (mpeClimo[MonthlyPrecipItem], XmNvalueChangedCallback,
		  display_prism_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeClimo[MonthlyMaxTempItem], XmNvalueChangedCallback,
		  display_max_temp_prism_RFCW, (XtPointer) & map_number);
   XtAddCallback (mpeClimo[MonthlyMinTempItem], XmNvalueChangedCallback,
		  display_min_temp_prism_RFCW, (XtPointer) & map_number);





}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   _create_menu_polygons
* PURPOSE:       Creates the Polygons menu which contains the options to draw
*                edit polygons and delete edit polygons.

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
static void
_create_menu_polygons ()
{
   mCreateMenu (&menu, &cascade, "Polygons", 'P');

   mCreateMenuItem (menu, &mpePolygons[DrawPolygonItem],
		    "Draw Polygons", 'D', NULL, NULL);
   mCreateMenuItem (menu, &mpePolygons[DeletePolygonItem],
		    "Delete Polygons", 'P', NULL, NULL);

   /* Add the callbacks to these menu items. */
   XtAddCallback (mpePolygons[DrawPolygonItem], XmNactivateCallback,
		  setup_draw_precip_RFCW, NULL);
   XtAddCallback (mpePolygons[DeletePolygonItem], XmNactivateCallback,
		  show_deletepolygonDS, NULL);

   return;
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   _create_menu_misc
* PURPOSE:       Creates the Misc menu with the options to display the Bias Table,
*                Review Hourly Radar, Timelapse fields, and display Multi-Hour
*                precipitation accumulations.
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

static void
_create_menu_misc ()
{
   static int map_number = 0;

   extern time_lapse_struct tldata;
   static Boolean reset_to_initial_date = True;

   mCreateMenu (&menu, &cascade, "Misc", 'M');

   mCreateMenuItem (menu, &mpeMisc[DisplayBiasTableItem],
		    "Display Bias Table ...", 'D', NULL, NULL);

   mCreateMenuItem (menu, &mpeMisc[ReviewHourlyRadarItem],
		    "Review Hourly Radar...", 'R', "Alt<Key>R", "Alt + R");

   _create_cascade (menu, &mpeMisc[TimeLapseMenu],
		    &timelapse_cascade, "Time Lapse", 'T');

   mCreateMenuItem (mpeMisc[TimeLapseMenu],
		    &mpeMisc[TimeLapse6], "6 Hr", '6', NULL, NULL);

   mCreateMenuItem (mpeMisc[TimeLapseMenu],
		    &mpeMisc[TimeLapse12], "12 Hr", '1', NULL, NULL);
   mCreateMenuItem (mpeMisc[TimeLapseMenu],
		    &mpeMisc[TimeLapse24], "24 Hr", '2', NULL, NULL);
   mCreateMenuItem (mpeMisc[TimeLapseMenu],
		    &mpeMisc[TimeLapseOther], "Other ...", 'O', NULL, NULL);
   mCreateMenuItem (mpeMisc[TimeLapseMenu],
		    &mpeMisc[TimeLapseEndLoop], "End Loop", 'E', NULL, NULL);

   mCreateMenuItem (menu, &mpeMisc[MultiHourQPEItem],
		    "Multi-Hour QPE ...", 'M', NULL, NULL);

   /* Add the callbacks to these menu items. */
   XtAddCallback (mpeMisc[DisplayBiasTableItem], XmNactivateCallback,
		  display_bias_table, NULL);
   XtAddCallback (mpeMisc[ReviewHourlyRadarItem], XmNactivateCallback,
		  select_site_rfcwide, NULL);
   XtAddCallback (mpeMisc[TimeLapse6], XmNactivateCallback,
		  do_time_lapse_6_RFCW, &tldata);
   XtAddCallback (mpeMisc[TimeLapse12], XmNactivateCallback,
		  do_time_lapse_12_RFCW, &tldata);
   XtAddCallback (mpeMisc[TimeLapse24], XmNactivateCallback,
		  do_time_lapse_24_RFCW, &tldata);
   XtAddCallback (mpeMisc[TimeLapseOther], XmNactivateCallback,
		  do_time_lapse_RFCW, &tldata);
   XtAddCallback (mpeMisc[TimeLapseEndLoop], XmNactivateCallback,
		  end_time_lapse_RFCW, (XtPointer) & reset_to_initial_date);
   XtAddCallback (mpeMisc[MultiHourQPEItem], XmNactivateCallback,
		  precipAccumCallback, (XtPointer) & map_number);
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   _create_menu_gages
* PURPOSE:       Creates the Gages menu.  This menu contains the gauge editing tools
*                which includes the DailyQC tools for edting gage, temperature and
*                freezing level as well as the standard MPE gage editing tools (pseudo
*                gage, gage table, display 7x7).
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

static void
_create_menu_gages ()
{
   static const char *mpe_show_missing_gage_token = "mpe_show_missing_gage";
   static const char *mpe_default_missing = "None";

   char reply[LEN_REPLY];
   int request_len;
   int reply_len;
   int status;

   mCreateMenu (&menu, &cascade, "Gages", 'G');
   mCreateMenuItem (menu, &mpeGages[QCPrecipitationItem],
		    "QC Precipitation...", 'P', "Alt<key>P", "Alt+P");
   mCreateMenuItem (menu, &mpeGages[QCTemperatureItem],
		    "QC Temperatures...", 'T', "Alt<key>T", "Alt+T");
   mCreateMenuItem (menu, &mpeGages[QCFreezingItem],
		    "QC Freezing Level...", 'F', "Alt<key>F", "Alt+F");

   mCreateMenuSeparator (menu);
   mCreateMenuItem (menu, &mpeGages[SaveLevel2DataItem], "Save Level 2 Data",
		    'S', NULL, NULL);

   mCreateMenuSeparator (menu);

   mCreateMenuItem (menu, &mpeGages[PseudoGageItem],
		    "Add Pseudo Gage", 'A', NULL, NULL);
   mCreateMenuItem (menu, &mpeGages[GageTableItem],
		    "Gage Table ...", 'G', "Ctrl<Key>G", "Ctrl+G");
   mCreateMenuItem (menu, &mpeGages[Display7x7Item],
		    "Display 7 x 7 ...", 'D', NULL, NULL);

   mCreateMenuSeparator (menu);

   mCreateMenuItem (menu, &mpeGages[BadGagesItem],
		    "Show Bad Gages", 'B', NULL, NULL);

   _create_menu_item_toggle (menu, &mpeGages[GageIdentifiersItem],
			     "Show Gage Identifiers", 'I', NULL, NULL);
   _create_menu_item_toggle (menu, &mpeGages[GageValuesItem],
			     "Show Gage Values", 'V', NULL, NULL);

   _create_cascade (menu, &mpeGages[GageMissingMenu],
		    &gage_missing_cascade, "Show Missing Gages", 'M');
   _create_menu_item_toggle (mpeGages[GageMissingMenu],
			     &mpeGages[GageMissingAll],
			     "All Missing", 'A', NULL, NULL);
   _create_menu_item_toggle (mpeGages[GageMissingMenu],
			     &mpeGages[GageMissingReported],
			     "Reported Missing", 'R', NULL, NULL);
   _create_menu_item_toggle (mpeGages[GageMissingMenu],
			     &mpeGages[GageMissingNone],
			     "No Missing", 'N', NULL, NULL);

   /* Create the Gage Color Scheme submenu.  This menu provides
      several options for setting the colors of the gage value and
      identifier labels. These include setting these labels to the
      standard solid sandy brown color, a constrasting color scheme which
      ensures the background color doesn't wash out the labels, a
      color scheme according to QC value, and a color by gage value. */

   _create_cascade (menu, &mpeGages[GageColorMenu],
		    &gage_color_cascade, "Gage Color", 'C');
   _create_menu_item_toggle (mpeGages[GageColorMenu],
			     &mpeGages[GageColorSolid],
			     "Solid Color", 'S', NULL, NULL);
   _create_menu_item_toggle (mpeGages[GageColorMenu],
			     &mpeGages[GageColorContrast],
			     "Contrast Color", 'C', NULL, NULL);
   _create_menu_item_toggle (mpeGages[GageColorMenu],
			     &mpeGages[GageColorQC],
			     "Color By QC", 'Q', NULL, NULL);
   _create_menu_item_toggle (mpeGages[GageColorMenu],
			     &mpeGages[GageColorByValue],
			     "Color By Value", 'V', NULL, NULL);

   /* Add the callbacks for these menu items. */
   XtAddCallback (mpeGages[QCPrecipitationItem], XmNactivateCallback,
		  edit_precip_gages_callback, (XtPointer) & area_values_TB);
   XtAddCallback (mpeGages[QCTemperatureItem], XmNactivateCallback,
		  edit_temp_gages_callback, (XtPointer) & area_values_TB);
   XtAddCallback (mpeGages[QCFreezingItem], XmNactivateCallback,
		  edit_freezing_gages_callback, (XtPointer) & area_values_TB);
   XtAddCallback (mpeGages[GageIdentifiersItem], XmNvalueChangedCallback,
		  set_mpe_gage_ids, NULL);
   XtAddCallback (mpeGages[GageValuesItem], XmNvalueChangedCallback,
		  set_mpe_gage_values, NULL);
   XtAddCallback (mpeGages[BadGagesItem], XmNactivateCallback,
		  show_mpe_bad_gage_list, NULL);
   XtAddCallback (mpeGages[PseudoGageItem], XmNactivateCallback,
		  add_pseudo_RFCW, &rad_data[0]);
   XtAddCallback (mpeGages[GageTableItem], XmNactivateCallback,
		  show_gage_table_RFCW, NULL);
   XtAddCallback (mpeGages[Display7x7Item], XmNactivateCallback,
		  init_single_gage_RFCW, &rad_data[0]);

   XtAddCallback (mpeGages[GageMissingAll], XmNvalueChangedCallback,
		  set_mpe_gage_missing, (XtPointer) MPEgageMissingAll);
   XtAddCallback (mpeGages[GageMissingReported], XmNvalueChangedCallback,
		  set_mpe_gage_missing, (XtPointer) MPEgageMissingReported);
   XtAddCallback (mpeGages[GageMissingNone], XmNvalueChangedCallback,
		  set_mpe_gage_missing, (XtPointer) MPEgageMissingNone);
   XtAddCallback (mpeGages[SaveLevel2DataItem],
		  XmNactivateCallback, send_dbase, NULL);


   /* Set the default gage missing option based on the mpe_show_missing_gage
    * token. */
   request_len = strlen (mpe_show_missing_gage_token);
   status = get_apps_defaults ((char *) mpe_show_missing_gage_token,
			       &request_len, (char *) reply, &reply_len);

   if ((status != 0) || (reply_len == 0))
   {
      flogMessage (stderr, "\nIn routine '_create_menu_mpe_control':\n"
	       "Couldn't retrieve a value for token '%s'.\n"
	       "Using '%s' as the default mpe gage missing value.\n",
	       mpe_show_missing_gage_token, mpe_default_missing);
      strcpy (reply, mpe_default_missing);
   }

   switch (reply[0])
   {
   case 'A':
   case 'a':

      set_mpe_gage_missing (mpeGages[GageMissingAll],
			    (XtPointer) MPEgageMissingAll, NULL);
      break;

   case 'R':
   case 'r':

      set_mpe_gage_missing (mpeGages[GageMissingReported],
			    (XtPointer) MPEgageMissingReported, NULL);
      break;

   case 'N':
   case 'n':

      set_mpe_gage_missing (mpeGages[GageMissingNone],
			    (XtPointer) MPEgageMissingNone, NULL);
      break;

   default:

      flogMessage (stderr, "\nIn routine '_create_menu_mpe_control':\n"
	       "Unrecognized gage missing option '%s'. Setting\n"
	       "the gage missing option to '%s'.\n",
	       reply, mpe_default_missing);
      set_mpe_gage_missing (mpeGages[GageMissingNone],
			    (XtPointer) MPEgageMissingNone, NULL);
      break;
   }


   XtAddCallback (mpeGages[GageColorSolid], XmNvalueChangedCallback,
		  set_mpe_gage_color, (XtPointer) MPEgageColorSolid);
   XtAddCallback (mpeGages[GageColorContrast], XmNvalueChangedCallback,
		  set_mpe_gage_color, (XtPointer) MPEgageColorContrast);
   XtAddCallback (mpeGages[GageColorQC], XmNvalueChangedCallback,
		  set_mpe_gage_color, (XtPointer) MPEgageColorQC);
   XtAddCallback (mpeGages[GageColorByValue], XmNvalueChangedCallback,
		  set_mpe_gage_color, (XtPointer) MPEgageColorByValue);

   /* Set the default MPE gage label and color option to contrast. */
   set_mpe_gage_color (mpeGages[GageColorContrast],
		       (XtPointer) MPEgageColorContrast, NULL);
}

/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   input
* PURPOSE:       This routine positions and manages the popup menu associated
*                with a single click of the right mouse button.  This feature
*                was added to emulate the behavior of D2D.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input   Widget       wid               The widget in which the button click
*                                          event was sensed.
*   Input   XtPointer    none              The client data.
*   Input   XtPointer    none2             The data specific to the callback
*                                          list and object class.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*    Only Motif APIs are utilized.  Do a "man" on these routines for
*    further information.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None (although the Motif routines do report error messages to standard
*          error)
********************************************************************************
*/
static void
input (Widget widget, XtPointer client_data, XtPointer call_data)
{
   /* Place the code to create the "popup" menu here. */
   XmMenuPosition (popup_menu, (XButtonPressedEvent *) call_data);
   XtManageChild (popup_menu);

}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   product_menu_cb
* PURPOSE:       This routine defines the callbacks for selected menu items
*                in the popup menu.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input   Widget       wid               The widget in which the button click
*                                          event was sensed.
*   Input   XtPointer    none              The client data.
*   Input   XtPointer    none2             The data specific to the callback
*                                          list and object class.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                  HEADER FILE         DESCRIPTION
*   buttonpress_cb        hv_mainCallbacks.h  Selects a station on the screen
*                                             and launches the timeseries
*                                             control gui for it.
*   mSetCursor            map_library.h       Set the cursor to a user-
*                                             specified symbol.
*   _pan_map_down         map_convert.h       Recenter the map down.
*   _pan_map_left         map_convert.h       Recenter the map left.
*   _pan_map_right        map_convert.h       Recenter the map right.
*   _pan_map_up           map_convert.h       Recenter the map up.
*   _set_recenter_flag    map_menubar_cb.h    Recenter the map.
*   _zoom_map_in          map_convert.h       Increase the magnification of the
*                                             map.  View the map at a smaller
*                                             scale.
*   _zoom_map_out         map_convert.h     Decrease the magnification of the
*                                           map.  View the map at a larger
*                                           scale.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                 DESCRIPTION
*   int        item                 Contains the position number of the item
*                                   selected in the popup menu.
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None.   This routine will write an error message out to the standard
*    error stream if an unsupported item is selected from the popup menu.
*
********************************************************************************
*/
/* popup_cb() -- invoked when the user selects an item in the popup menu. */
static void product_menu_cb (Widget menu_item, XtPointer client_data, XtPointer call_data)
{
   enum DisplayFieldData fieldToDisplay = display_rMosaic;
   int item_no = (int) client_data;

   switch ( item_no )
   {
      case 0:
         /* Display Radar Mosaic */
         fieldToDisplay = display_rMosaic;
         break;

      case 1:
         /* Display Average Radar Mosaic */
         fieldToDisplay = display_avgrMosaic;
         break;

      case 2:
         /* Display Max Radar Mosaic */
         fieldToDisplay = display_maxrMosaic;
         break;

      case 3:
         /* Display Field Bias Mosaic */
         fieldToDisplay = display_bMosaic;
         break;

      case 4:
         /* Display Local Bias Mosaic */
         fieldToDisplay = display_lMosaic;
         break;

      case 5:
         /* Display Gage Only */
         fieldToDisplay = display_gageOnly;
         break;

      case 6:
         /* Display Multisensor Mosaic */
         fieldToDisplay = display_mMosaic;
         break;

      case 7:
         /* Display Local Bias Multisensor Mosaic */
         fieldToDisplay = display_mlMosaic;
         break;

      case 8:
         /* Display Satellite Precip */
         fieldToDisplay = display_satPrecip;
         break;

      case 9:
         /* Display Local Bias Satellite Precip. */
         fieldToDisplay = display_lsatPrecip;
         break;

      case 10:
         /* Display Satellite Radar Mosaic */
         fieldToDisplay = display_srMosaic;
         break;

      case 11:
         /* Display Satellite Gage Mosaic */
         fieldToDisplay = display_sgMosaic;
         break;

      case 12:
         /* Display Satellite Radar Gage Mosaic */
         fieldToDisplay = display_srgMosaic;
         break;

      case 13:
         /* Display Triangulate Mosaic */
         fieldToDisplay = display_p3Mosaic;
         break;

      case 14:
         /* Display RFC Mean Field Bias Mosaic */
         fieldToDisplay = display_Xmrg;
         break;

      case 15:
         /* Display RFC MultiSensor Mosaic */
         fieldToDisplay = display_rfcbMosaic;
         break;

      case 16:
         /* Display RFC Mosaic */
         fieldToDisplay = display_rfcmMosaic;
         break;

      case 17:
         /* Display Best Estimate QPE */
         fieldToDisplay = display_rfcMosaic;
         break;

      default:

         flogMessage (stderr, "Unsupported option selected in popup menu.\n"
	                      "Button number %d.\n", item_no);
         break;
   }

   rad_data[1].field_type = fieldToDisplay;
   display_mpe_data (1);
   return;

}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   _create_product_menu
* PURPOSE:       This routine builds the popup menu that is launched by
*                a right click of the mouse.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*    Routines from XMotif and its toolkit are used.  Do a "man" each of the
*    routines to learn more about it.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME            DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
static void _create_product_menu ()
{
   static int first = 1;
   static Boolean sensitizeButton [NUM_BEST_PRODUCTS + 1];

   char buttonName[20];
   int i;
   int length;
   int product_state;
   int status;
   int verbose = 1;

   Widget buttonWidget = NULL;

   /* These correspond one-to-one to the options on the product
      popup menu. */
   static const char * mpeFieldNames [ NUM_BEST_PRODUCTS + 1 ] =
                 {  "RMOSAIC",
                    "AVGRMOSAIC",
                    "MAXRMOSAIC",
                    "BMOSAIC",
                    "LMOSAIC",
                    "GAGEONLY",
                    "MMOSAIC",
                    "MLMOSAIC",
                    "SATPRE",
                    "LSATPRE",
                    "SRMOSAIC",
                    "SGMOSAIC",
                    "SRGMOSAIC",
                    "P3LMOSAIC",
                    "XMRG",
                    "RFCBMOSAIC",
                    "RFCMMOSAIC",
                    "RFCMOSAIC" };

   XmString rmosaic;
   XmString avgrmosaic;
   XmString maxrmosaic;
   XmString bmosaic;
   XmString lmosaic;
   XmString gageonly;
   XmString mmosaic;
   XmString mlmosaic;
   XmString satpre;
   XmString lsatpre;
   XmString satrmosaic;
   XmString satgagemosaic;
   XmString satrgagemosaic;
   XmString p3lmosaic;
   XmString xmrg;
   XmString rfcbmosaic;
   XmString rfcmmosaic;
   XmString rfcxmrg;

   /* Create the labels for the push buttons on the popup menu. */
   rmosaic = XmStringCreateLocalized ("Radar Mosaic");
   avgrmosaic = XmStringCreateLocalized ("Average Radar Mosaic");
   maxrmosaic = XmStringCreateLocalized ("Max Radar Mosaic");
   bmosaic = XmStringCreateLocalized ("Field Bias Radar Mosaic");
   lmosaic = XmStringCreateLocalized ("Local Bias Radar Mosaic");
   gageonly = XmStringCreateLocalized ("Gage Only Analysis");
   mmosaic = XmStringCreateLocalized ("Multisensor Mosaic");
   mlmosaic = XmStringCreateLocalized ("Local Bias Multisensor Mosaic");
   satpre = XmStringCreateLocalized ("Satellite Precip");
   lsatpre = XmStringCreateLocalized ("Local Bias Satellite Precip");
   satrmosaic = XmStringCreateLocalized ("Satellite Radar Mosaic");
   satgagemosaic = XmStringCreateLocalized ("Satellite Gage Mosaic");
   satrgagemosaic = XmStringCreateLocalized ("Satellite Radar Gage Mosaic");
   p3lmosaic = XmStringCreateLocalized ("Triangulated Local Bias Mosaic");
   xmrg = XmStringCreateLocalized ("Best Estimate QPE");
   rfcbmosaic = XmStringCreateLocalized ("RFC Field Bias Mosaic");
   rfcmmosaic = XmStringCreateLocalized ("RFC Multisensor Mosaic");
   rfcxmrg = XmStringCreateLocalized ("RFC QPE Mosaic");

   /* Create the popup menu. */
   product_menu = XmVaCreateSimplePopupMenu (_get_map_widget (1),
                                             "Select Product",
                                             product_menu_cb,
                                             XmVaPUSHBUTTON,
                                             rmosaic, 'R', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             avgrmosaic, 'A', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             maxrmosaic, 'M', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             bmosaic, 'F', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             lmosaic, 'L', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             gageonly, 'G', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             mmosaic, 'u', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             mlmosaic, 'o', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             satpre, 'S', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             lsatpre, 'c', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             satrmosaic, 't', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             satgagemosaic, 'e', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             satrgagemosaic, 'l', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             p3lmosaic, '3', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             xmrg, 'Q', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             rfcbmosaic, 'C', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             rfcmmosaic, 'i', NULL, NULL,
                                             XmVaPUSHBUTTON,
                                             rfcxmrg, 'E', NULL, NULL,
                                             NULL);

   /* Free the strings used in creating the labels on the pushbuttons
      of the popup menu. */
   XmStringFree (rmosaic);
   XmStringFree (avgrmosaic);
   XmStringFree (maxrmosaic);
   XmStringFree (bmosaic);
   XmStringFree (lmosaic);
   XmStringFree (gageonly);
   XmStringFree (mmosaic);
   XmStringFree (mlmosaic);
   XmStringFree (satpre);
   XmStringFree (lsatpre);
   XmStringFree (satrmosaic);
   XmStringFree (satgagemosaic);
   XmStringFree (satrgagemosaic);
   XmStringFree (p3lmosaic);
   XmStringFree (xmrg);
   XmStringFree (rfcbmosaic);
   XmStringFree (rfcmmosaic);
   XmStringFree (rfcxmrg);

   /* Build an array of the widgets corresponding to each button on the
      popup menu. Only do this once. */
   if ( first == 1 )
   {
      first = 0;

      for ( i = 0; i < NUM_BEST_PRODUCTS + 1; ++i )
      {
         length = strlen ( mpeFieldNames[i] );
         get_mpe_product_state ( mpeFieldNames[i], &length,
                                 &verbose, &product_state, &status );

         sensitizeButton[i] = True;

         if ( status != 0 )
         {
            status = strcmp ( mpeFieldNames[i], "XMRG" );

            /* The XMRG product is always on. */
            if ( status != 0 )
            {
               flogMessage ( stderr, "In _create_product_menu, error retrieving state "
                                     "for product %s.\n", mpeFieldNames[i] );
            }
         }
         else if ( product_state == 0 )
         {
            sensitizeButton[i] = False;
         }
      }
   }

   for ( i = 0; i < NUM_BEST_PRODUCTS + 1; ++i )
   {
      sprintf ( buttonName, "button_%d", i );
      buttonWidget = XtNameToWidget ( product_menu, buttonName );

      if ( sensitizeButton[i] == False )
      {
         DeSensitize ( buttonWidget );
      }
   }
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   product_select
* PURPOSE:       This routine positions and manages the popup menu associated
*                with a single click of the right mouse button on map 1.
*                This feature allows a MPE product to be selected and
*                displayed on Map 1.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input   Widget       wid               The widget in which the button click
*                                          event was sensed.
*   Input   XtPointer    none              The client data.
*   Input   XtPointer    none2             The data specific to the callback
*                                          list and object class.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*    Only Motif APIs are utilized.  Do a "man" on these routines for
*    further information.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None (although the Motif routines do report error messages to standard
*          error)
********************************************************************************
*/
static void
product_select (Widget widget, XtPointer client_data, XtPointer call_data)
{
   /* Check to determine if the product menu has been created.
      If not create it. The product menu cannot be created until
      the split screen mode is selected in Hydroview/MPE. */
   if (product_menu == NULL)
   {
      _create_product_menu ();
   }

   /* Place the code to create the "popup" menu here. */
   XmMenuPosition (product_menu, (XButtonPressedEvent *) call_data);
   XtManageChild (product_menu);
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   process_clicks
* PURPOSE:       This routine performs the appropriate actions based upon
*                which mouse button has been "clicked" and whether or not
*                a double mouse click as occured.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  XtPointer   client_data          Contains a true (1) or false (0)
*                                           value indicating whether or not
*                                           there has been a double mouse
*                                           click.
*   Input  XtInterval *id                   Contains information about the
*                                           interval of time used to discern
*                                           a single mouse click from a
*                                           double mouse click.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                HEADER FILE         DESCRIPTION
*   buttonpress_cb      hv_mainCallbacks.h  The action to take on a double
*                                           mouse click.
*   mConvertXY2LatLon   map_library.h       Convert Cartesian Coordinates to
*                                           latitude and longitude values.
*   mSetCenterLatLon    map_library.h       Set the center latitude and
*                                           longitude of the map screen.
*   _zoom_map_in        map_convert.h       Increase the magnification of the
*                                           map screen.
*   _zoom_map_out       map_convert.h       Decrease the magnification of the
*                                           map screen.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   float      lat                          Contains a value representing a
*                                           latitude coordinate.
*   float      lon                          Contains a value representing a
*                                           longitude coordinate.
*   int        clicks                       Contains a numeric value indicating
*                                           a single or a double click.  A
*                                           value of "0" represents a single
*                                           mouse click.  A value of "1"
*                                           represents a double mouse click.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    This routine will print a text message to standard error if
*    an unrecognized or unsupported mouse button is pressed.
********************************************************************************
*/
extern rubber_poly_data polydata;

void
process_clicks (XtPointer client_data, XtIntervalId * id)
{
   static Boolean continue_to_dispatch_return = False;
   static Boolean return_to_initial_time = False;
   float lat, lon;
   extern int add_pseudo_flag;
   extern int display_7x7_flag;
   extern int draw_poly_flag;
   extern rubber_band_data rbdata;
   Widget map_widget;
   XEvent event;

   int clicks = (int) client_data;

   click_count = 0;

   if (clicks)
   {
      /* Do nothing on a double mouse click. */
   }
   else
   {
      switch (mouse_clicks.first_button_press)
      {
      case 1:

	 if (button_one_down == True)
	 {
	    /* The user is holding down the left mouse button.
	       Enter the functionality which allows the user to
	       draw a rubber band for zooming. */
	    map_widget = _get_map_widget (0);
	    rbdata.last_x = rbdata.start_x = 0;
	    rbdata.last_y = rbdata.start_y = 0;
	    event.xbutton.x = mouse_clicks.x;
	    event.xbutton.y = mouse_clicks.y;

	    mpe_start_rubber_band (map_widget, (XtPointer) & rbdata,
				   &event, &continue_to_dispatch_return);
	 }
	 else if (add_pseudo_flag == 1)
	    locate_pseudo_RFCW (mouse_clicks.wid,
				(XtPointer) & rad_data[0],
				(XEvent *) & mouse_clicks,
				&continue_to_dispatch_return);

	 else if (display_7x7_flag == 1)
	    display_single_gage_RFCW (mouse_clicks.wid,
				      (XtPointer) & rad_data[0],
				      (XEvent *) & mouse_clicks,
				      &continue_to_dispatch_return);

	 else
	 {
	    mSaveOriginalCenterLatLon ();
	    mConvertXY2LatLon (mouse_clicks.x, mouse_clicks.y, &lat, &lon);
	    mSetCenterLatLon (lat, lon);
	    _zoom_map_out (mouse_clicks.wid, &return_to_initial_time, NULL);
	 }

	 break;

      case 2:

	 if (draw_poly_flag == 1)
	    start_end_rubber_poly_RFCW (mouse_clicks.wid,
					(XtPointer) & polydata,
					(XEvent *) & mouse_clicks,
					&continue_to_dispatch_return);
	 else
	 {
	    mSaveOriginalCenterLatLon ();
	    mConvertXY2LatLon (mouse_clicks.x, mouse_clicks.y, &lat, &lon);
	    mSetCenterLatLon (lat, lon);
	    _zoom_map_in (mouse_clicks.wid, &return_to_initial_time, NULL);
	 }

	 break;

      case 3:

/*	    if(qpf_on==1 && edit_dialog==NULL)
            {
      		edit_stations ( mouse_clicks.x,
			        mouse_clicks.y,
                                mouse_clicks.map_num );
            }

   	  else if(flf_on==1 && edit_dialog==NULL)
      		edit_zstations(mouse_clicks.map_num,
			       mouse_clicks.x,
			       mouse_clicks.y);

   	  else if(maxmin_on==1 && edit_dialog==NULL)
      		edit_maxminstations(mouse_clicks.map_num,
				    mouse_clicks.x,
				    mouse_clicks.y);
*/

      default:

	 flogMessage (stderr, "Button %d is unsupported.\n",
		  mouse_clicks.first_button_press);

      }

   }

}

/*******************************************************************************
* MODULE NUMBER: 9
* MODULE NAME:   select_cb
* PURPOSE:       This routine "listens" for mouse button clicks and keeps
*                a count of them to determine if the user is double clicking
*                or single clicking.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      wid         The widget in which an event is triggering
*                                  the execution of this routine.
*   Input  XtPointer   client_data The data being passed into this routine by
*                                  the client.
*   Input  XEvent *    event       A structure containing information
*                                  specific to the event that is triggering
*                                  the callback of this routine.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                HEADER FILE         DESCRIPTION
*   input               defined above       This routine creates the popup
*                                           menu associated with a right
*                                           click of the mouse (third
*                                           mouse button).
*   process_clicks      defined above       This routine process the click
*                                           information determined by this
*                                           routine.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE       NAME         DESCRIPTION
*   XtIntervalId    id           Defines an interval which represents
*                                the maximum amount of time to elapse between
*                                mouse clicks before they are treated as
*                                individual single clicks as opposed to
*                                a double click.
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/
void
select_cb (Widget wid, XtPointer client_data, XEvent * event)
{
   Boolean continue_to_dispatch_return = False;
   extern int draw_poly_flag;
   int split_screen;
   extern rubber_band_data rbdata;
   XtAppContext app;
   static XtIntervalId id;

   split_screen = is_screen_split ();

   app = _get_map_context ();

   if (event->type == ButtonRelease)
   {
      /* Check to see if it was button 1 that was released. */
      if (event->xbutton.button == 1)
      {
	 button_one_down = False;

	 /* If this was in zoom mode, then end the rubber band. */
	 if (rbdata.rubber_band_zoom_mode == True)
	 {
	    mpe_end_rubber_band (wid, (XtPointer) & rbdata, event,
				 &continue_to_dispatch_return);
	 }
      }

      return;
   }

   /* Initialize the elements of the clicks structure. */
   mouse_clicks.x = event->xbutton.x;
   mouse_clicks.y = event->xbutton.y;


   mouse_clicks.map_num = (int) client_data;


   /* Set the widget id in the mouse_clicks structure. */
   mouse_clicks.wid = wid;

   /* Test to determine of button 3 has been pressed.  If it has been
      pressed, then process the callback right away. */
   if ((event->xbutton.button == 1) && (draw_poly_flag == 1))
   {
      mouse_clicks.first_button_press = event->xbutton.button;

      start_end_rubber_poly_RFCW (mouse_clicks.wid,
				  (XtPointer) & polydata,
				  (XEvent *) & mouse_clicks,
				  &continue_to_dispatch_return);
   }
   else if (event->xbutton.button == 3)
   {
      if (draw_poly_flag == 1)
      {
	 /* Initialize the elements of the clicks structure. */
	 mouse_clicks.first_button_press = event->xbutton.button;

	 id = XtAppAddTimeOut (app, time_interval, process_clicks, False);

	 start_end_rubber_poly_RFCW (mouse_clicks.wid,
				     (XtPointer) & polydata,
				     (XEvent *) & mouse_clicks,
				     &continue_to_dispatch_return);
      }
      else if (split_screen == 0)
      {
	 if ((qpf_on == 1) && (group_edit != 1))
	 {
	    edit_stations (mouse_clicks.x,
			   mouse_clicks.y, mouse_clicks.map_num);
	 }
	 else if ((flf_on == 1) && (group_edit != 1))
	 {
	    edit_zstations (mouse_clicks.x,
			    mouse_clicks.y, mouse_clicks.map_num);
	 }
	 else if ((maxmin_on == 1) && (group_edit != 1))
	 {
	    edit_maxminstations (mouse_clicks.x,
				 mouse_clicks.y, mouse_clicks.map_num);
	 }
	 else if (group_edit == 1 && qpf_on == 1)
	 {
	    group_edit_precip_stations (mouse_clicks.map_num,
					mouse_clicks.x, mouse_clicks.y);
	 }
	 else if (group_edit == 1 && maxmin_on == 1)
	 {
	    group_edit_temperature_stations (mouse_clicks.map_num,
					     mouse_clicks.x, mouse_clicks.y);
	 }
	 else
	 {
	    input (wid, client_data, (XtPointer) event);
	 }
      }
      else
      {
	 input (wid, client_data, (XtPointer) event);
      }
   }
   else
   {
      click_count++;

      if (event->xbutton.button == 1)
      {
	 button_one_down = True;
      }


      if (click_count == 1)
      {
	 /* Initialize the elements of the clicks structure. */
	 mouse_clicks.first_button_press = event->xbutton.button;

	 id = XtAppAddTimeOut (app, time_interval, process_clicks, False);

      }
      else if (click_count >= 2)
      {
	 button_one_down = False;

	 /* Initialize the elements of the clicks structure. */
	 mouse_clicks.second_button_press = event->xbutton.button;

	 XtRemoveTimeOut (id);
	 process_clicks ((XtPointer) True, &id);
      }
   }
}

/*******************************************************************************
* MODULE NUMBER: 10
* MODULE NAME:   map1_select_cb
* PURPOSE:       This routine "listens" for mouse button clicks and keeps
*                a count of them to determine if the user is double clicking
*                or single clicking.  This routine is for mouse events on map1.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      wid         The widget in which an event is triggering
*                                  the execution of this routine.
*   Input  XtPointer   client_data The data being passed into this routine by
*                                  the client.
*   Input  XEvent *    event       A structure containing information
*                                  specific to the event that is triggering
*                                  the callback of this routine.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                HEADER FILE         DESCRIPTION
*   input               defined above       This routine creates the popup
*                                           menu associated with a right
*                                           click of the mouse (third
*                                           mouse button).
*   process_clicks      defined above       This routine process the click
*                                           information determined by this
*                                           routine.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE       NAME         DESCRIPTION
*   XtIntervalId    id           Defines an interval which represents
*                                the maximum amount of time to elapse between
*                                mouse clicks before they are treated as
*                                individual single clicks as opposed to
*                                a double click.
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/
static void
map1_select_cb (Widget wid, XtPointer client_data, XEvent * event)
{
   static Boolean return_to_initial_time = False;
   int split_screen;
   float lat;
   float lon;
   XtAppContext app;

   app = _get_map_context ();

   /* Initialize the elements of the clicks structure. */
   mouse_clicks.x = event->xbutton.x;
   mouse_clicks.y = event->xbutton.y;

   /* Set the widget id in the mouse_clicks structure. */
   mouse_clicks.wid = wid;

   split_screen = is_screen_split ();

   /* Test to determine of button 3 has been pressed.  If it has been
      pressed, then process the callback right away. */
   if ((event->xbutton.button == 1) && (event->type == ButtonPress))
   {
      mSaveOriginalCenterLatLon ();
      mConvertXY2LatLon (mouse_clicks.x, mouse_clicks.y, &lat, &lon);
      mSetCenterLatLon (lat, lon);
      _zoom_map_out (mouse_clicks.wid, &return_to_initial_time, NULL);
   }
   else if ((event->xbutton.button == 2) && (event->type == ButtonPress))
   {
      mSaveOriginalCenterLatLon ();
      mConvertXY2LatLon (mouse_clicks.x, mouse_clicks.y, &lat, &lon);
      mSetCenterLatLon (lat, lon);
      _zoom_map_in (mouse_clicks.wid, &return_to_initial_time, NULL);
   }
   else if (event->xbutton.button == 3)
   {
      if (split_screen == 1)
      {
	 if ((qpf_on == 1) && (group_edit != 1))
	 {
	    edit_stations (mouse_clicks.x,
			   mouse_clicks.y, mouse_clicks.map_num);
	 }
	 else if ((flf_on == 1) && (group_edit != 1))
	 {
	    edit_zstations (mouse_clicks.x,
			    mouse_clicks.y, mouse_clicks.map_num);
	 }
	 else if (maxmin_on == 1 && (group_edit != 1))
	 {
	    edit_maxminstations (mouse_clicks.x,
				 mouse_clicks.y, mouse_clicks.map_num);
	 }
	 else if (group_edit == 1 && qpf_on == 1)
	 {
	    group_edit_precip_stations (mouse_clicks.map_num,
					mouse_clicks.x, mouse_clicks.y);
	 }
	 else if (group_edit == 1 && maxmin_on == 1)
	 {
	    group_edit_temperature_stations (mouse_clicks.map_num,
					     mouse_clicks.x, mouse_clicks.y);
	 }
	 else
	 {
	    product_select (wid, client_data, (XtPointer) event);
	 }
      }
      else
      {
	 product_select (wid, client_data, (XtPointer) event);
      }
   }
}


/*******************************************************************************
* MODULE NUMBER: 12
* MODULE NAME:   toggle_latlon
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
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
void
toggle_latlon ()
{
   if (show_latlon == 0)
   {
      show_latlon = 1;
   }
   else
   {
      show_latlon = 0;

      /* Copy the the lat / lon pixmap onto the screen pixmap. */
      XCopyArea (_get_map_display (),
		 last_pixmap,
		 XtWindow (_get_map_widget (0)),
		 _get_map_gc (),
		 0,
		 0,
		 LAT_LON_IMAGE_WIDTH,
		 LAT_LON_IMAGE_HEIGHT, image_x_pos, image_y_pos);
      copy = 0;
   }

}

/*******************************************************************************
* MODULE NUMBER: 13
* MODULE NAME:   toggle_mpemenu
* PURPOSE:       This routine toggles the additional "bin specific" information
*                on the Mpe menu.  This additional information includes
*                the x, y coordinate of the bin on the HRAP grid, the
*                latitude / longitude of the bin, the precipitation value in
*                the bin, and the county/basin containing the bin.
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
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
void
toggle_mpemenu ()
{
   int mpe_data_flag;
   enum MapState mpe_legend_point_info;

   /* Test to determine if there is Mpe data displayed. */
   mpe_data_flag = isThereMpeData ();

   if (mpe_data_flag != 0)
   {
      /* There is mpe data. Toggle the "display_data_flag" to "on"
         or "off" depending on its current state. */
      mpe_legend_point_info = getMpeDrawLegendStatus ();

      if (mpe_legend_point_info == M_OFF)
      {
	 _set_legend_height (0, MPE_LEGEND_HEIGHT + 60, 0);
	 drawMpeLegendPointInfo ();
      }
      else
      {
	 _set_legend_height (0, MPE_LEGEND_HEIGHT, 0);
	 dontDrawMpeLegendPointInfo ();
      }

   }

}

/*******************************************************************************
* MODULE NUMBER: 14
* MODULE NAME:   popup_cb
* PURPOSE:       This routine defines the callbacks for selected menu items
*                in the popup menu.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input   Widget       wid               The widget in which the button click
*                                          event was sensed.
*   Input   XtPointer    none              The client data.
*   Input   XtPointer    none2             The data specific to the callback
*                                          list and object class.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                  HEADER FILE         DESCRIPTION
*   buttonpress_cb        hv_mainCallbacks.h  Selects a station on the screen
*                                             and launches the timeseries
*                                             control gui for it.
*   mSetCursor            map_library.h       Set the cursor to a user-
*                                             specified symbol.
*   _pan_map_down         map_convert.h       Recenter the map down.
*   _pan_map_left         map_convert.h       Recenter the map left.
*   _pan_map_right        map_convert.h       Recenter the map right.
*   _pan_map_up           map_convert.h       Recenter the map up.
*   _set_recenter_flag    map_menubar_cb.h    Recenter the map.
*   _zoom_map_in          map_convert.h       Increase the magnification of the
*                                             map.  View the map at a smaller
*                                             scale.
*   _zoom_map_out         map_convert.h     Decrease the magnification of the
*                                           map.  View the map at a larger
*                                           scale.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                 DESCRIPTION
*   int        item                 Contains the position number of the item
*                                   selected in the popup menu.
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None.   This routine will write an error message out to the standard
*    error stream if an unsupported item is selected from the popup menu.
*
********************************************************************************
*/
/* popup_cb() -- invoked when the user selects an item in the popup menu. */
static void
popup_cb (Widget menu_item, XtPointer client_data, XtPointer call_data)
{


   int item_no = (int) client_data;

   Widget map_widget;

   map_widget = _get_map_widget (0);

   switch (item_no)
   {

   case 0:

      _zoom_map_in (map_widget, NULL, call_data);
      break;

   case 1:

      _zoom_map_out (map_widget, NULL, call_data);
      break;

   case 2:

      _set_recenter_flag (M_ON);
      mSetCursor (M_SELECT);
      break;

   case 3:

      _pan_map_up (map_widget, NULL, call_data);
      break;

   case 4:

      _pan_map_down (map_widget, NULL, call_data);
      break;

   case 5:

      _pan_map_left (map_widget, NULL, call_data);
      break;

   case 6:

      _pan_map_right (map_widget, NULL, call_data);
      break;

   case 7:

      toggle_latlon ();
      break;

   case 8:

      toggle_mpemenu ();
      break;

   case 9:

      break;

   default:

      flogMessage (stderr, "Unsupported option selected in popup menu.\n"
	       "Button number %d.\n", item_no);
   }

}

/*******************************************************************************
* MODULE NUMBER: 15
* MODULE NAME:   _create_popup_menu
* PURPOSE:       This routine builds the popup menu that is launched by
*                a right click of the mouse.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*    Routines from XMotif and its toolkit are used.  Do a "man" each of the
*    routines to learn more about it.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME            DESCRIPTION
*   XmString   in              Used to label the "in" item of the popup menu.
*   XmString   out             Used to label the "out" item of the popup menu.
*   XmString   recenter        Used to label the "recenter" item of the popup
*                              menu.
*   XmString   up              Used to label the "up" item of the popup menu.
*   XmString   down            Used to label the "down" item of the popup
*                              menu.
*   XmString   left            Used to label the "left" item of the popup menu.
*   XmString   right           Used to label the "right" item of the popup
*                              menu.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
void
_create_popup_menu ()
{
   XmString in, out, recenter, up, down, latlon, left, right, mpeinfo;

   /* Create the labels for the push buttons on the popup menu. */
   in = XmStringCreateLocalized ("In");
   out = XmStringCreateLocalized ("Out");
   recenter = XmStringCreateLocalized ("Recenter");
   up = XmStringCreateLocalized ("Up");
   down = XmStringCreateLocalized ("Down");
   left = XmStringCreateLocalized ("Left");
   right = XmStringCreateLocalized ("Right");
   latlon = XmStringCreateLocalized ("Lat/Lon");
   mpeinfo = XmStringCreateLocalized ("Mpe Info");

   /* Create the popup menu. */
   popup_menu = XmVaCreateSimplePopupMenu (_get_map_widget (0), "popup",
					   popup_cb, XmVaPUSHBUTTON,
					   in, 'I', NULL, NULL,
					   XmVaPUSHBUTTON, out,
					   'O', NULL, NULL,
					   XmVaPUSHBUTTON, recenter,
					   'R', NULL, NULL,
					   XmVaSEPARATOR,
					   XmVaPUSHBUTTON, up,
					   'U', NULL, NULL,
					   XmVaPUSHBUTTON, down,
					   'D', NULL, NULL, XmVaPUSHBUTTON,
					   left, 'L', NULL, NULL,
					   XmVaPUSHBUTTON,
					   right, 'g', NULL, NULL,
					   XmVaSEPARATOR,
					   XmVaPUSHBUTTON,
					   latlon, 'T', NULL, NULL,
					   XmVaPUSHBUTTON,
					   mpeinfo, 'M', NULL, NULL, NULL);

   /* Free the strings used in creating the labels on the pushbuttons
      of the popup menu. */
   XmStringFree (in);
   XmStringFree (out);
   XmStringFree (recenter);
   XmStringFree (up);
   XmStringFree (down);
   XmStringFree (left);
   XmStringFree (right);
   XmStringFree (latlon);
   XmStringFree (mpeinfo);

}

/*******************************************************************************
* MODULE NUMBER: 16
* MODULE NAME:   mouse_motion_callback
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
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
static void mouse_motion_callback (Widget wid, XtPointer clientdata, XEvent * event)
{
   char str[SIZE_OF_LAT_LON_STRING];
   char str_value[SIZE_OF_HRAP_POINT_STRING];
   char str_precip[SIZE_OF_HRAP_POINT_STRING];
   float factor;
   float lat, lon;
   float value;
   HRAP pnt;
   static int first = 0;
   int index = (int) clientdata;
   int is_there_mpe_data;
   int mpe_image_x_pos;
   int mpe_image_y_pos;
   int n;
   enum MapState point_info_legend_state;
   point hrap;
   Widget hrap_basin_text = 0;
   Widget hrap_county_text = 0;
   Widget hrap_precip_text = 0;
   Widget hrap_xy_text = 0;

   /* Initialize variables to reflect the state of the additional MPE
      HRAP information and the existence of MPE data. */
   is_there_mpe_data = isThereMpeData ();
   point_info_legend_state = getMpeDrawLegendStatus ();

   /* Display the value underneath the mouse pointer in addition
      to the latitude and longitude. */
   if (is_there_mpe_data == 1)
   {
      /* Determine the "x" and "y" coordinates from the position of the
         pointer on the screen. */
      mpe_image_x_pos = event->xmotion.x;
      mpe_image_y_pos = event->xmotion.y;

      /* Compute the latitude and the longitude from this cartesian
         coordinate. */
      mConvertXY2LatLon (mpe_image_x_pos, mpe_image_y_pos, &lat, &lon);

      /* Convert the latitude/longitude information into an HRAP
         coordinate. */
      pnt = LatLongToHrapMpe (lat, (-1) * lon);
      hrap.x = (int) pnt.x;
      hrap.y = (int) pnt.y;

      if ((hrap.x >= XOR) && (hrap.x < (XOR + MAXX)) &&
	  (hrap.y >= YOR) && (hrap.y < (YOR + MAXY)))
      {
	 /* Retrieve the precipitation value underneath the mouse cursor. */
	 factor = units_factor * (float) scale_factor;

	 if ((rad_data[index].field_type == display_Locspan) ||
	     (rad_data[index].field_type == display_maxtempPrism) ||
	     (rad_data[index].field_type == display_mintempPrism) ||
	     (rad_data[index].field_type == display_Locbias))
	 {
	    factor = scale_factor;
	 }
	 else if (rad_data[index].field_type == display_Prism)
	 {
	    factor = units_factor;
	 }

	 if (rad_data[index].field_type == display_Index)
	 {
	    /* Reconfigure the "Value" label to read "Radar". */
	    n = rad_data[index].data_array[hrap.x - XOR][hrap.y - YOR] - 1;

	    if (n >= 0)
	    {
	       sprintf (str_precip, "%s", nexrad[n].id);
	    }
	    else
	    {
	       sprintf (str_precip, "Missing");
	    }

	 }
	 else
	 {
	    value = (float) rad_data[index].data_array[hrap.x - XOR]
	       [hrap.y - YOR] / factor;

	    if (value >= 0)
	    {
	       sprintf (str_precip, "%4.2fin", value);
	    }
	    else
	    {
	       sprintf (str_precip, "Missing");
	    }

	 }
      }
      else
      {
	 sprintf (str_precip, "Missing");
      }
   }

   /* Check to determine if the user wants the latitude / longitude
      information displayed. */
   if (show_latlon != 0)
   {

      /* Determine if this is the very first time in ... */
      if (first == 0)
      {
	 first = 1;
	 last_pixmap = XCreatePixmap (_get_map_display (),
				      RootWindowOfScreen (XtScreen (wid)),
				      LAT_LON_IMAGE_WIDTH,
				      LAT_LON_IMAGE_HEIGHT,
				      DefaultDepthOfScreen (XtScreen (wid)));
      }

      /* Check to see if this is the first time that this routine is being
         called since the lat/lon information was turned "on". */
      if (copy != 0)
      {
	 XCopyArea (_get_map_display (),
		    last_pixmap,
		    XtWindow (wid),
		    _get_map_gc (),
		    0,
		    0,
		    LAT_LON_IMAGE_WIDTH,
		    LAT_LON_IMAGE_HEIGHT, image_x_pos, image_y_pos);
      }

      /* Determine the "x" and "y" coordinates from the position of the
         pointer on the screen. */
      image_x_pos = event->xmotion.x;
      image_y_pos = event->xmotion.y;

      /* Compute the latitude and the longitude from this cartesian
         coordinate. */
      mConvertXY2LatLon (image_x_pos, image_y_pos, &lat, &lon);

      /* Compute the new "x" and "y" positions of the lat/lon information
         pixmap box. */
      image_x_pos += LAT_LON_X_OFFSET;
      image_y_pos += LAT_LON_Y_OFFSET;

      /* Copy the portion of the screen pixmap at the cartesian coordinate
         given by image_x_pos and image_y_pos into the last_image
         pixmap. */
      XCopyArea (_get_map_display (),
		 XtWindow (wid),
		 last_pixmap,
		 _get_map_gc (),
		 image_x_pos,
		 image_y_pos,
		 LAT_LON_IMAGE_WIDTH, LAT_LON_IMAGE_HEIGHT, 0, 0);
      copy = 1;

      /* Set the foreground color to white. */
      mSetColor ("White");

      /* Write the latitude and the longitude to the lat_lon_image pixmap. */
      memset (str, '\0', SIZE_OF_LAT_LON_STRING);
      sprintf (str, "%7.2f", lat);
      _area_draw_text (XtWindow (wid), image_x_pos, image_y_pos + 10, str);
      memset (str, '\0', SIZE_OF_LAT_LON_STRING);
      sprintf (str, "%7.2f", lon);
      _area_draw_text (XtWindow (wid), image_x_pos, image_y_pos + 20, str);

      if (is_there_mpe_data == 1)
      {
         _area_draw_text ( XtWindow(wid), image_x_pos, image_y_pos + 30, str_precip);
      }
   }

   if ((point_info_legend_state == M_ON) && (is_there_mpe_data == 1))
   {
      hrap_xy_text = getMpeLegendHrapXYText ();
      hrap_precip_text = getMpeLegendHrapValueText ();
      hrap_county_text = getMpeLegendHrapCountyText ();
      hrap_basin_text = getMpeLegendHrapBasinText ();

      if ((hrap.x >= XOR) && (hrap.x < (XOR + MAXX)) &&
	  (hrap.y >= YOR) && (hrap.y < (YOR + MAXY)))
      {
	 /* Print the values into the text area fields in the Mpe legend. */
	 sprintf (str_value, "%3d, %3d", hrap.x, hrap.y);
	 XmTextSetString (hrap_xy_text, str_value);

	 /* Use the predetermined MPE value under the mouse pointer. */
	 XmTextSetString (hrap_precip_text, str_precip);

	 /* Load the county and basin information.  The county is the
	    county containing the Hrap Bin the mouse pointer is currently
	    on. The basin is the basin containing the Hrap Bin the mouse
	    pointer is currently on. */
	 if (overlay_avail.gridtobasin == 0)
	 {
	    sprintf (str_value, "Not Defined");
	 }
	 else
	 {
	    if (strcmp (loc_basin[hrap.y - YOR][hrap.x - XOR], "") == 0)
	    {
	       sprintf (str_value, "Not Defined");
	    }
	    else
	    {
	       sprintf (str_value, "%s",
			loc_basin[hrap.y - YOR][hrap.x - XOR]);
	    }
	 }

	 XmTextSetString (hrap_basin_text, str_value);

	 if (overlay_avail.gridtocounty == 0)
	 {
	    sprintf (str_value, "Not Defined");
	 }
	 else
	 {
	    if (strcmp (loc_cty[hrap.y - YOR][hrap.x - XOR], "") == 0)
	    {
	       sprintf (str_value, "Not Defined");
	    }
	    else
	    {
	       sprintf (str_value, "%s", loc_cty[hrap.y - YOR][hrap.x - XOR]);
	    }
	 }

	 XmTextSetString (hrap_county_text, str_value);
      }
      else
      {
	 /* Out of range.  Cannot process the HRAP point information. */
	 sprintf (str_value, "----");
	 XmTextSetString (hrap_xy_text, str_value);
	 XmTextSetString (hrap_precip_text, str_value);
	 XmTextSetString (hrap_county_text, str_value);
	 XmTextSetString (hrap_basin_text, str_value);
      }

   }

   return;
}

/*******************************************************************************
* MODULE NUMBER: 17
* MODULE NAME:   leave_window_callback
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
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
static void
leave_window_callback (Widget wid, XtPointer clientdata, XEvent * event)
{
   int index = (int) clientdata;

   if (show_latlon == 1)
   {

      /* Copy the the lat / lon pixmap onto the screen pixmap. */
      XCopyArea (_get_map_display (),
		 last_pixmap,
		 XtWindow (_get_map_widget (index)),
		 _get_map_gc (),
		 0,
		 0,
		 LAT_LON_IMAGE_WIDTH,
		 LAT_LON_IMAGE_HEIGHT, image_x_pos, image_y_pos);
      copy = 0;
   }

}

/*******************************************************************************
* MODULE NUMBER: 18
* MODULE NAME:   about_app_cb
* PURPOSE:       This routine produces a GUI which provides information
*                about Hydroview.  Specifically, it provides the release
*                version and the date that it was implemented.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                DESCRIPTION/UNITS
*   Input   Widget       wid               The widget in which the button click
*                                          event was sensed.
*   Input   XtPointer    client_data       The client data.
*   Input   XtPointer    call_data         The data specific to the callback
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                      HEADER FILE         DESCRIPTION
*   techinfo_show             TechInfo.h          Creates a dialog box
*                                                 containing the Hydroview
*                                                 information.
*   get_hv_name               version_info.h      Retrieves the name of
*                                                 Hydroview.
*   get_hv_ver                version_info.h      Retrieves the version of
*                                                 Hydroview.
*   get_hv_ver_date           version_info.h      Retrieves the implementation
*                                                 date of this version.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE          NAME           DESCRIPTION
*   char *             hv_name        Contains the name of Hydroview as it
*                                     should appear in the "About" tech info
*                                     window.
*   char *             hv_ver         Contains the version of Hydroview as it
*                                     should appear in the "About" tech info
*                                     window.
*   char *             hv_ver_date    Contains the date of Hydroview as it
*                                     should appear in the "About" tech info
*                                     window.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
static void
about_app_cb (Widget wid, XtPointer client_data, XtPointer call_data)
{
   char *hv_name = NULL;
   char *hv_ver = NULL;
   char *hv_date = NULL;

   hv_name = get_hv_name ();
   hv_ver = get_hv_ver ();
   hv_date = get_hv_ver_date ();

   if ((hv_name != NULL) && (hv_ver != NULL) && (hv_date != NULL))
   {
      techinfo_show (wid, hv_name, hv_ver, hv_date);
   }
}


/*******************************************************************************
* MODULE NUMBER: 19
* MODULE NAME:   redrawMap
* PURPOSE:       This is the routine that redraws the entire map and any
*                overlays that are active.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME               HEADER FILE          DESCRIPTION
*   mUpdateMap         map_library.h        This map library routine updates
*                                           the appropriate map by calling the
*                                           "map expose" routine the user
*                                           supplied for it.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
void
redrawMap ()
{
   mUpdateMap (0);
}

/*******************************************************************************
* MODULE NUMBER: 21
* MODULE NAME:   draw_launch_gui
* PURPOSE:       This routine creates the MPE Editor application window
*                using various map library and Motif routines.  It is
*                designed specifically to be called from a Motif "time out".
*                This has been done to allow the manipulation of a
*                work dialog box while the application is being build and
*                data is being read in from the database.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME                DESCRIPTION/UNITS
*   Input   XtPointer    clientdata          Contains a pointer to the work
*                                            dialog box widget.
*   Input   XtIntervalId * id                A pointer to the identifier of
*                                            the Motif "time out" that
*                                            resulted in this routine being
*                                            called.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                      HEADER FILE         DESCRIPTION
*   _create_menu_liveData     Not Applicable      Creates the "Live Data" menu
*   _create_menu_mpe_control  Not Applicable      Creates the "Mpe Control"
*                                                 menu
*   _create_menu_product      Not Applicable      Creates the "Product" menu
*   _create_popup_menu        Not Applicable      Creates the popup menu
*   getHvColorList            HvColorList.h       Retrieves a list of colors
*                                                 used by the application.
*   mAddFocusChangeRoutine    map_library.h       Registers a routine with the
*                                                 map library that will be
*                                                 used to perform a specific
*                                                 action whenever the focus
*                                                 of the main window changes.
*   mAddLegendExpose          map_library.h       Adds the routine that
*                                                 exposes the map legend
*   mAddMapExpose             map_library.h       Adds the routine that exposes
*                                                 the map.
*   mAddMapSelectRoutine      map_library.h       Adds the routine that
*                                                 determines what needs to be
*                                                 done when a map is selected.
*   mAddMotionCallback        map_library.h       Adds an additional callback
*                                                 to the motion detection.
*   mCreateMapScreen          map_library.h       Creates the map screen.
*   mInitMap                  map_library.h       Initializes the properties of
*                                                 the map.
*   mInitMapScreen            map_library.h       Initializes the properties of
*                                                 the map screen.
*   mSetCenterLatLon          map_convert.h       Sets the latitude and
*                                                 longitude that the map is
*                                                 centered on.
*   mSetOverlayColor          map_library.h       Sets the color of the
*                                                 current overlay being drawn.
*   mSetRegionArea            map_library.h       Sets the name of the region
*                                                 displayed in the map.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE     NAME                   DESCRIPTION
*   HvColorList * hcl                    A pointer to a static HvColorList
*                                        structure which contains all of the
*                                        color information for Hydroview.
*   int           first                  This variable ensures that this
*                                        routine is only called once.
*   char *        center_lat_token_name  The name of the token containing
*                                        the center latitude for the map
*                                        displayed in MPE Editor.
*   char *        center_lon_token_name  The name of the token containing
*                                        the center longitude for the map
*                                        displayed in MPE Editor.
*   char [ ]      res_title              The name of the resource file
*                                        to retrieve resource information from.
*   char [ ]      reply                  The value of a token is returned in
*                                        this array by the get_apps_defaults
*                                        routine.
*   char *        width_token_name       The name of the token containing
*                                        the width in nautical miles of the
*                                        geographic viewer.
*   float         latitude               The center latitude of the viewer.
*   float         longitude              The center longitude of the viewer.
*   float         width_in_nmi           The width in nautical miles of the
*                                        viewing area.
*   int           first                  Indicates whether or not this routine
*                                        has already been called. A value of
*                                        "0" means it has not, a value of "1"
*                                        means it has.
*   int           reply_len              The length of the string of characters
*                                        contained within the "reply" array.
*   int           request_len            The length of the request (the number
*                                        of characters) of the requested token
*                                        name being sent to get_apps_defaults.
*   int           status                 Contains status codes returned
*                                        from calls to the "get_apps_defaults"
*                                        and the "read_overlay_configuration"
*                                        files.
*   Widget        shell                  The popup shell containing the
*                                        map.
*   Widget *      working                A pointer to the working dialog box
*                                        widget.
*   XtAppContext  app                    The application context.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
void
draw_launch_gui (XtPointer clientdata, XtIntervalId * id)
{
   static char *center_lat_token_name = "CENTER_LAT";
   static char *center_lon_token_name = "CENTER_LON";
   static char *hv_width_in_pixels_token_name = "hv_width_in_pixels";
   static char *hv_height_in_pixels_token_name = "hv_height_in_pixels";
   static char res_titlemap[] = "MPE Editor";
   char reply[LEN_REPLY];
   static char *width_token_name = "NMI_WIDE";
   float latitude;
   float longitude;
   float width_in_nmi;
   //HvColorList *hcl = getHvColorList ();
   static int first = 1;
   int hv_height_in_pixels;
   int hv_width_in_pixels;
   int reply_len;
   int request_len;
   int status;
   extern rubber_band_data rbdata;
   extern Widget map_menu_items[num_of_menu_items];
   Widget map_widget;
   Widget shell;
   XtAppContext app;

   Widget *working = (Widget *) clientdata;

   if (!first)
      return;
   first = 0;

   /* Allocate memory for the widget_struct pointer. */
   widget_struct = (rfcwide_widget_struct *)
      malloc (sizeof (rfcwide_widget_struct));

   if (widget_struct == NULL)
   {
      flogMessage (stderr, "In routine \"button_cb\":\n"
	       "The attempt to allocate memory for the\n"
	       "\"widget_struct\" data member failed.  The\n"
	       "construction of the \"MPE Editor\" application\n"
	       "is being aborted.\n");
      return;
   }

   /* Retrieve the station latitude and longitude from the
      "apps defaults" files. */
   request_len = strlen (center_lat_token_name);
   status = get_apps_defaults (center_lat_token_name, &request_len, reply,
			       &reply_len);

   if (status != 0)
   {
      flogMessage (stderr, "\nIn routine button_cb:\n"
	       "Couldn't retrieve the center latitude of the\n"
	       "MPE Editor viewer.  The construction of the viewer\n"
	       "is being stopped.\n");
      return;
   }

   latitude = (float) atof (reply);
   request_len = strlen (center_lon_token_name);
   status = get_apps_defaults (center_lon_token_name, &request_len, reply,
			       &reply_len);

   if (status != 0)
   {
      flogMessage (stderr, "\nIn routine button_cb:\n"
	       "Couldn't retrieve the center longitude of the\n"
	       "MPE Editor viewer.  The construction of the viewer\n"
	       "is being stopped.\n");
      return;
   }

   longitude = (float) atof (reply);

   /* Retrieve the desired width in nautical miles of the viewer. */
   request_len = strlen (width_token_name);
   status = get_apps_defaults (width_token_name, &request_len,
			       reply, &reply_len);

   if (status != 0)
   {
      flogMessage (stderr, "\nIn routine button_cb:\n"
	       "Couldn't retrieve the width in nautical miles of\n"
	       "the MPE Editor viewer.  Token %s is expected to\n"
	       "contain this value.  The construction of the\n"
	       "viewer is being stopped.\n", width_token_name);
      return;
   }

   width_in_nmi = (float) atof (reply);

   /* Configure the data viewer. */
   mSetCenterLatLon (latitude, longitude);

   /* Retrieve the width and height in pixels of the viewing area. */
   request_len = strlen (hv_width_in_pixels_token_name);
   status = get_apps_defaults (hv_width_in_pixels_token_name,
			       &request_len, reply, &reply_len);

   if (status != 0)
   {
      flogMessage (stderr, "\nIn routine button_cb:\n"
	       "Couldn't retrieve the width in pixels of\n"
	       "the viewing area from token %s.  Setting the\n"
	       "width to the default value %d.\n",
	       hv_width_in_pixels_token_name, HV_DEFAULT_WIDTH_IN_PIXELS);
      hv_width_in_pixels = HV_DEFAULT_WIDTH_IN_PIXELS;
   }
   else
   {
      hv_width_in_pixels = atoi (reply);

      if (hv_width_in_pixels <= 0)
      {
	 flogMessage (stderr, "\nIn routine button_cb:\n"
		  "Invalid width in pixels %d retrieved from token\n"
		  "%s.  Setting the width to the default value %d.\n",
		  hv_width_in_pixels, hv_width_in_pixels_token_name,
		  HV_DEFAULT_WIDTH_IN_PIXELS);
	 hv_width_in_pixels = HV_DEFAULT_WIDTH_IN_PIXELS;
      }
   }

   request_len = strlen (hv_height_in_pixels_token_name);
   status = get_apps_defaults (hv_height_in_pixels_token_name,
			       &request_len, reply, &reply_len);

   if (status != 0)
   {
      flogMessage (stderr, "\nIn routine button_cb:\n"
	       "Couldn't retrieve the height in pixels of\n"
	       "the viewing area from token %s.  Setting the\n"
	       "height to the default value %d.\n",
	       hv_height_in_pixels_token_name, HV_DEFAULT_HEIGHT_IN_PIXELS);
      hv_height_in_pixels = HV_DEFAULT_HEIGHT_IN_PIXELS;
   }
   else
   {
      hv_height_in_pixels = atoi (reply);

      if (hv_height_in_pixels <= 0)
      {
	 flogMessage (stderr, "\nIn routine button_cb:\n"
		  "Invalid height in pixels %d retrieved from token\n"
		  "%s.  Setting the height to the default\n"
		  "value %d.\n",
		  hv_height_in_pixels,
		  hv_height_in_pixels_token_name,
		  HV_DEFAULT_HEIGHT_IN_PIXELS);
	 hv_height_in_pixels = HV_DEFAULT_HEIGHT_IN_PIXELS;
      }
   }

   mInitMapScreen (hv_height_in_pixels, hv_width_in_pixels, 0, 0,
		   width_in_nmi, M_OFF, M_TOP, "MPE Editor 8.3");
   mInitLegend (M_OFF, M_BOTTOM, MPE_LEGEND_WIDTH, MPE_LEGEND_HEIGHT);
   //mInitMap (1, M_OFF, hcl->background_color, hcl->state_border_color);
   mCreateMapScreen (parent, res_titlemap);

   /* Read the overlay information. */
   status = read_overlay_configuration ();

   if (status != M_OK)
   {
      flogMessage (stderr, "\nIn routine button_cb:\n"
	       "An error was encountered by routine\n"
	       "\"read_overlay_configuration\".  The construction\n"
	       "of the viewer is being stopped.\n");
      return;
   }


   /* Unmanage the map legend on the Help menu. */
   XtUnmanageChild (map_menu_items[map_help_legend]);

   /* Set the amount of time that will be used to distinguish between
      a single mouse click and a double mouse click. */
   shell = _get_map_shell ();
   XtSetMultiClickTime (XtDisplay (shell), time_interval);

   /* Check to see if this application is being used at Guam.
      Because Guam is so far removed from the rest of the U.S.,
      it gets its own special box defined for it. */
   if (longitude > 0)
   {
      /* A positive longitude means that we are in the Eastern Hemisphere.
         Guam is the only WFO in this part of the world. */
      mSetRegionArea (HV_GUAM_REGION_TOP_LAT, HV_GUAM_REGION_LEFT_LON,
		      HV_GUAM_REGION_BOTTOM_LAT, HV_GUAM_REGION_RIGHT_LON);
   }
   else
   {
      mSetRegionArea (HV_REGION_TOP_LAT, HV_REGION_LEFT_LON,
		      HV_REGION_BOTTOM_LAT, HV_REGION_RIGHT_LON);
   }

   /* Add user-defined callback routines to the map library. */
   /* Add the callbacks for map 0. */
   mAddLegendExpose (0, drawMpeLegend);
   /*mAddLegendDrawCb ( 0 , ( MotionCallback ) stationLegendCallback ) ; */
   mAddMapExpose (0, redrawBase);
   mAddMapSelectRoutine (0, select_cb);
   mAddLegendSelectRoutine (0, dqc_legend_select_cb);
   mAddMotionCallback (0, mouse_motion_callback);
   mAddLeaveCallback (0, leave_window_callback);
   mAddAboutAppCallback ((MotionCallback) about_app_cb);
   mAddMapCleanUp (free_dynamic_memory);

   /* Add the callbacks for map 1. */
   mAddLegendExpose (1, drawMpeLegend);
   //mAddLegendDrawCb ( 1 , ( MotionCallback ) stationLegendCallback ) ;
   mAddMapExpose (1, redrawBase);
   mAddMapSelectRoutine (1, map1_select_cb);
   mAddLegendSelectRoutine (0, dqc_legend_select_cb);
   mAddMotionCallback (1, mouse_motion_callback);
   mAddLeaveCallback (1, leave_window_callback);

   /* Add the callback for the case when the user holds mouse button 1
      while and drags the mouse. This is used for drawing a zoom rectangle. */
   map_widget = _get_map_widget (0);
   XtAddEventHandler (map_widget, Button1MotionMask, FALSE,
		      mpe_track_rubber_band, (XtPointer) & rbdata);

   /* Define special actions that must be performed when changing the
      orientation of the map viewing area. */
   mAddPanRoutine (hv_pan_callback);
   mAddZoomRoutine (hv_zoom_callback);
   mAddRecenterRoutine (hv_recenter_callback);

   /* The File, Tools, Projections, Overlays, and Help menus are created by Map Library. */
   /* Create the MPE Editor-specific menus. */
   _create_menu_mpe_control ();
   _create_menu_precip_fields ();
   _create_menu_base_fields ();
   _create_menu_gages ();
   _create_menu_polygons ();
   _create_menu_climo ();
   _create_menu_misc ();
   _update_map_lib_tools ();

   /* Initialize the menu item widgets that the mpe software
      needs to be able to sensitize and desensitize. This is not
      the best solution, but it will work for now. */
   widget_struct->choose_widget = mpeControl[ChooseHourItem];
   widget_struct->next_widget = mpeControl[NextHourItem];
   widget_struct->prev_widget = mpeControl[PrevHourItem];
   widget_struct->clear_widget = mpeControl[ClearDataItem];
   widget_struct->radar_site_widget = mpeMisc[ReviewHourlyRadarItem];
   timelapse_widget = mpeMisc[TimeLapseMenu];

   widget_struct->rerun_widget = mpeControl[RegenerateHourFieldsItem];
   widget_struct->timelapse6_widget = mpeMisc[TimeLapse6];
   widget_struct->timelapse12_widget = mpeMisc[TimeLapse12];
   widget_struct->timelapse24_widget = mpeMisc[TimeLapse24];
   widget_struct->timelapseother_widget = mpeMisc[TimeLapseOther];
   widget_struct->stoptime_widget = mpeMisc[TimeLapseEndLoop];
   showids_widget = mpeGages[GageIdentifiersItem];
   showval_widget = mpeGages[GageValuesItem];
   widget_struct->rfc_qpe_mosaic = mpeFields[RfcQpeItem];
   widget_struct->transmit_rfc_qpe = mpeControl[TransmitQpeItem];
   widget_struct->transmit_rfc_bias = mpeControl[TransmitBiasItem];

   //Added by Ram
   // Commented out by Bryon
   //-----------
   // showval_widget = mpeGages [ GageTrianglesItem ] ;
   //----------


   /* Initialize the "Fields" menu items.  These will also need to be
      "Sensitized" or "Desensitized" depending on whether there is
      Mpe data being displayed on the screen or not. */
   rmosaic_widget = mpeFields[RadarMosaicItem];
   avgrmosaic_widget = mpeFields[AvgRadarMosaicItem];
   maxrmosaic_widget = mpeFields[MaxRadarMosaicItem];
   bmosaic_widget = mpeFields[FieldBiasMosaicItem];
   mmosaic_widget = mpeFields[MultiMosaicItem];
   mlmosaic_widget = mpeFields[LocalMultiMosaicItem];
   p3lmosaic_widget = mpeFields[TriLocalMosaicItem];
   gageonly_widget = mpeFields[GageOnlyItem];
   xmrg_widget = mpeFields[BestEstimQPEItem];
   multihour_widget = mpeMisc[MultiHourQPEItem];
   height_widget = mpeBase[HeightFieldItem];
   index_widget = mpeBase[IndexFieldItem];
   locspan_widget = mpeBase[LocalSpanItem];
   locbias_widget = mpeBase[LocalBiasItem];
   prism_widget = mpeClimo[MonthlyPrecipItem];
   satprecip_widget = mpeFields[SatellitePrecipItem];
   lsatprecip_widget = mpeFields[LocalSatPrecipItem];
   srmosaic_widget = mpeFields[SatRadarPrecipItem];
   sgmosaic_widget = mpeFields[SatGagePrecipItem];
   srgmosaic_widget = mpeFields[SatRadarGagePrecipItem];
   rfcbmosaic_widget = mpeFields[RfcBMosaicItem];
   rfcmmosaic_widget = mpeFields[RfcMMosaicItem];
   bias_widget = mpeFields[LocalBiasMosaicItem];
   widget_struct->display_bias_widget = mpeMisc[DisplayBiasTableItem];
   widget_struct->single_gage_widget = mpeGages[Display7x7Item];
   savemaintop_widget = mpeControl[UpdateBestQPETopItem];
   savemainbottom_widget = mpeControl[UpdateBestQPEBottomItem];
   drawpoly_widget = mpePolygons[DrawPolygonItem];
   deletepoly_widget = mpePolygons[DeletePolygonItem];
   fullscreen_widget = mpeTools[FullScreenItem];
   splitscreen_widget = mpeTools[SplitScreenItem];

   widget_struct->gage_triangles = mpeBase[GageTrianglesItem];
   widget_struct->qc_precipitation = mpeGages[QCPrecipitationItem];
   widget_struct->qc_temperature = mpeGages[QCTemperatureItem];
   widget_struct->qc_freezing = mpeGages[QCFreezingItem];
   widget_struct->save_level2_data = mpeGages[SaveLevel2DataItem];
   widget_struct->pseudo_widget = mpeGages[PseudoGageItem];
   widget_struct->gage_table_widget = mpeGages[GageTableItem];
   widget_struct->gage_missing_menu = mpeGages[GageMissingMenu];
   widget_struct->gage_color_menu = mpeGages[GageColorMenu];
   widget_struct->monthly_max_temp = mpeClimo[MonthlyMaxTempItem];
   widget_struct->monthly_min_temp = mpeClimo[MonthlyMinTempItem];

   /* Initialize the function to reset the Choose Data Period window. */
   set_reset_function (set_choose_hour_window_to_previous);

   XtVaSetValues (*working, XmNmessageString,
		  XmStringCreateLtoR ("Reading MPE Data ...",
				      XmSTRING_DEFAULT_CHARSET), NULL);

   app = XtWidgetToApplicationContext (*working);

   XtAppAddTimeOut (app,
		    GUI_BUILDER_INIT_PAUSE_DURATION,
		    init_mpe_data, (XtPointer) working);

}

/*******************************************************************************
* MODULE NUMBER: 21
* MODULE NAME:   init_mpe_data
* PURPOSE:       This routine initializes the MPE portion of the MPE Editor
*                application.  It is setup specifically to be called in a
*                chain of Motif "Time Out" callbacks while updating the
*                text message on the working dialog box to keep the user
*                informed of the status of the program's data gathering.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME         DESCRIPTION/UNITS
*   Input  XtPointer     clientdata   Contains a pointer to the value
*                                     representing the working dialog box
*                                     widget.
*   Input  XtIntervalId  * id         Contains a pointer to the id
*                                     of the Motif time out that is causing
*                                     this routine to be run.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                   HEADER FILE      DESCRIPTION
*   _create_popup_menu     NA	            Creates the popup menu accessed
*                                           by a right mouse button click.
*   _get_map_gc            map_resource.h
*   _get_map_widget        map.h            Returns the widget representing
*                                           the drawing area of the map.
*   initMpeControl         initMpeControl.h The routine which defines all of
*                                           the necessary MPE variables and
*                                           contstants.
*   mOpenMapScreen         map_library.h    This routine either "pops up" or
*                                           "realizes" the main map lib
*                                           gui depending upon whether it
*                                           is a stand alone application or
*                                           it is a child of some other
*                                           application.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE    NAME                          DESCRIPTION
*   char *       hv_map_projection_token_name  Contains the name of the
*                                              token to retrieve the
*                                              default map projection from.
*   char         reply [ ]                     Contains a token's value as
*                                              returned by get_apps_defaults.
*   GC           gc                            The graphics context of the
*                                              the main drawing area.
*   int          projection                    The default map projection.
*   int          reply_len                     The length of the reply
*                                              token value from the call
*                                              get_apps_defaults.
*   int          request_len                   The length of the token name
*                                              being sent into
*                                              get_apps_defaults.
*   int          status                        The return status of the attempt
*                                              to read the MPE data.
*   Widget       map_widget                    The widget representing the
*                                              drawbable map area.
*   Widget       overlay_widget                The widget representing the
*                                              hrap grid toggle button on the
*                                              grid submenu found on the
*                                              overlays menu.
*   XtAppContext app                           The application context.
*
* DATA FILES AND/OR DATABASE:
*   Not Applicable
*
* ERROR HANDLING:
*   An error message is printed out if the attempt to initialize the mpe
*   data fails.
********************************************************************************
*/

void
init_mpe_data (XtPointer clientdata, XtIntervalId * id)
{
   static char *hv_map_projection_token_name = "hv_map_projection";
   char reply[LEN_REPLY];
   GC gc;
   int reply_len;
   int request_len;
   int projection;
   int status;
   Widget map_widget;
   Widget overlay_widget;
   XtAppContext app;

   extern Widget map_menu_items[];

   Widget *working = (Widget *) clientdata;

   /* Initialize and define the Mpe (Multi-sensor Precipitation Estimator)
      control variables. */
   gc = _get_map_gc ();
   map_widget = _get_map_widget (0);
   status = (int) initMpeControl (map_widget, &gc);

   if (status != (int) HydroStatus_OK)
   {

      if (status != (int) HydroStatus_NoCoordFile)
      {
	 flogMessage (stderr, "\nIn routine \"gui_builder\":\n"
		  "The call to initMpeControl failed with a\n"
		  "HydroStatus error code of %d.  The construction\n"
		  "of the viewer is being stopped.\n", status);
	 exit (0);
      }

      /* The Mpe coordinate data file could not be found.  Desensitize the
         "Choose Hour" option on the MpeControl menu. Also, desensitze the
         HRAP grid overlay option */
      DeSensitize (widget_struct->choose_widget);

      overlay_widget = _get_menu_item_toggle_widget (map_hrap_grid);
      DeSensitize (overlay_widget);

   }

   /* Desensitize the widgets for the Find Stations and Restore Initial
      options. */
   DeSensitize (mpeTools[FindStationItem]);
   DeSensitize (mpeTools[RestoreInitialItem]);

   /* Create the popup menus that will be activated by the third mouse
      button. */
   _create_popup_menu ();

   /* Set the map projection based on the map projection token.  There
      are three supported map projections: Flat, Polar Stereographic, and
      HRAP.   This can only be done at this point because the HRAP
      projection needs data from the MPE coordinate file which are
      read in this routine. */
   /* Retrieve the station latitude and longitude from the
      "apps defaults" files. */
   request_len = strlen (hv_map_projection_token_name);
   status = get_apps_defaults (hv_map_projection_token_name, &request_len,
			       reply, &reply_len);

   if ((status == 0) && (reply_len > 0))
   {
      switch (reply[0])
      {
      case 'f':
      case 'F':
	 projection = M_FLAT;
	 break;

      case 'p':
      case 'P':
	 projection = M_POLAR;
	 break;

      case 'h':
      case 'H':
	 projection = M_HRAP;
	 break;

      default:
	 projection = M_FLAT;
	 break;
      }
   }
   else
   {
      projection = M_FLAT;
   }

   mSetMapProjection (projection);

   /* Open the map screen. */
   mOpenMapScreen ();

   XtVaSetValues (*working,
		  XmNmessageString,
		  XmStringCreateLtoR ("GUI Creating Complete ... ",
				      XmSTRING_DEFAULT_CHARSET), NULL);

   app = XtWidgetToApplicationContext (*working);
   XtAppAddTimeOut (app,
		    GUI_BUILDER_INIT_PAUSE_DURATION,
		    popdown_dialog_window, (XtPointer) working);

   /* Get rid of the Set Station Icon Size item on the MPE Editor menu.
      This is only needed by Hydroview. */
   XtUnmanageChild (map_menu_items[map_tool_pdsize_cascade]);


}

/*******************************************************************************
* MODULE NUMBER:  22
* MODULE NAME:    popdown_dialog_window
* PURPOSE:        This routine unmanages the working dialog box.  If needed
*                 the working dialog box can be managed at a later time
*                 to keep the user informed of the status of the process.
*
* ARGUMENTS:
*   TYPE   DATA TYPE    NAME                 DESCRIPTION/UNITS
*   Input  XtPointer    clientdata           Contains a pointer to the working
*                                            dialog widget.
*   Input  XtIntervalId * id                 A pointer to the value with
*                                            identifies the time out interval
*                                            which caused this routine to
*                                            be called.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   Only System and Motif routines are used.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME        DESCRIPTION
*   Widget *   working     A pointer to the widget identifier of theo
*                          working dialog box.
*
* DATA FILES AND/OR DATABASE:
*   Not applicable
*
* ERROR HANDLING:
*   None done.
********************************************************************************
*/
void
popdown_dialog_window (XtPointer clientdata, XtIntervalId * id)
{
   Widget *working = (Widget *) clientdata;
   XtUnmanageChild (*working);
}

/*******************************************************************************
* MODULE NUMBER:  23
* MODULE NAME:    set_parent_widget
*
* PURPOSE:        If the map will be a child of an already existing
*                 widget, then the parent widget of the map must be
*                 set by calling this routine.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget      w                    The widget identifier of the
*                                           parent.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None ("parent" is an external variable defined within the map library).
*
* DATA FILES AND/OR DATABASE:
*   Not Applicable
*
* ERROR HANDLING:
*   Not Applicable
*
********************************************************************************
*/

void
set_parent_widget (Widget w)
{
   parent = w;

/*  ==============  Statements containing RCS keywords:  */
   {
      static char rcs_id1[] =
	 "$Source: /fs/hseb/ob83/ohd/pproc/src/mpe_editor/RCS/gui_builder.c,v $";
      static char rcs_id2[] =
	 "$Id: gui_builder.c,v 1.88 2008/01/11 17:06:25 lawrence Exp $";
   }
/*  ===================================================  */

}
