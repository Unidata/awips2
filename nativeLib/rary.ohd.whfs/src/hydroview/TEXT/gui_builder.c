
/*******************************************************************************
* FILENAME:            gui_builder.c
* NUMBER OF MODULES:   19
* GENERAL INFORMATION:
*      MODULE 1:  _create_menu_liveData
* DESCRIPTION:    This routine creates the Live Data menu and its associated
*                 subelements. 
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
*      MODULE 20:  init_point_data
* DESCRIPTION:     This routine initializes the Hydromap portion of the
*                  hmap_mpe application.
*      MODULE 21:  init_mpe_data
* DESCRIPTION:     This routine initialized the MPE portion of the 
*                  hmap_mpe application.
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
*                  02/01/06      Jingtao Deng      Add low statement in ReferenceData
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Xm.h>

#include "add_pseudo_RFCW.h"
#include "choose_rfcwide_date.h"
#include "clear_data_RFCW.h"
#include "create_ss_interface_rfcwide.h"
#include "DbmsAccess.h"
#include "delete_polygons_show.h"
#include "display_bias_table.h"
#include "display_field.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "drawHvLegend.h"
#include "pointcontrol_legend.h"
#include "draw_precip_poly_RFCW.h"
#include "gage_table_RFCW.h"
#include "GeneralUtil.h"
#include "gui_builder.h"
#include "HvCallbacks.h"
#include "HvColorList.h"
#include "HvDisplayControlProto.h"
#include "hv_mainCallbacks.h"
#include "HydroStatus.h"
#include "initMpeControl.h"
#include "launchDamCrest.h"
#include "launch_timeserieslite.h"
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
#include "newhour_RFCW.h"
#include "overlay.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_loc_shift.h"
#include "polygon_RFCW.h"
#include "post_functions.h"
#include "read_overlay_configuration.h"
#include "read_precip_data.h"
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


/*
 *  HydroView's custom additions to the Map Library's tool menu
 */
enum hv_tool_items {  LocShiftMenu, 
                      EditLocShift,
                      RereadLocShift,
                      HvColorManager,
                      HvColorThresholds,
                      HvMPEColorManager,
                      NumHvToolItems };
    

/* Enum map data items defines the menu items created on the
   "MapData" menu. */
enum map_data_items { PointDisplay, StationSelection, RefreshData,
                      DamDisplay, ArealDisplay, BestQPE, ClearDataPDC, 
                      NumMapDataItems };
                      
/* Enum live_menu_items defines the menu items created on the
   "LiveData" menu. */
enum live_menu_items { TimeSeries , RiverMonitor, SiteSpecificModel ,
                       AlertAlarm ,  QuestionableData , RejectData ,
                       StationReporting , PointPrecipitation ,
                       StationProfile , RiverSummary , 
                       NumLiveItems } ;

/* Enum mpe_control_items defines the menu items created on the
   "MpeControl" menu. */
enum mpe_control_items { NextHourItem , PrevHourItem , ChooseHourItem,
                         SaveDataItemTop , SaveDataItemBottom, ClearDataItem , 
                         RerunFieldGenItem, DrawPolygonItem , 
                         DeletePolygonItem, ShowSingleRadarSite,
                         TimeLapseMenu , TimeLapse6 , TimeLapse12,
                         TimeLapse24 , TimeLapseOther , TimeLapseEndLoop,
                         MpeGageMenu , PseudoGageItem , GageTableItem,
                         GageIdentifierItem , GageValuesItem ,GageTrianglesItem,
			 GageMissingMenu,GageMissingAll, GageMissingReported, 
			 GageMissingNone,GageColorMenu , GageColorSolid, 
			 GageColorContrast , GageColorQC , GageColorByValue,
                         MpeViewMenu, ViewFullScreenItem, ViewSplitScreenItem,
			 NumMpeControlItems };

/* Enum ref_menu_items defines the menu items created on the
   "ReferenceData" menu. */
enum ref_menu_items { StaffGage , ImpactStatement , LowStatement ,
                      RatingCurve , DataSources , Contacts , 
		      CrestHistory , TextReports , NumRefItems } ;

/* Enum field_items defines the menu items created on the
   "FieldData" menu. */
enum field_menu_items { RadarMosaicItem , AvgRadarMosaicItem,
                        MaxRadarMosaicItem, FieldBiasMosaicItem ,
                        LocalBiasMosaicItem ,  MultiMosaicItem ,
			            LocalMultiMosaicItem , P3LocalMosaicItem,
                        BestEstimQPEItem , MultiHourQPEItem , 
                        SatellitePrecipItem , LocalSatPrecipItem , 
                        GageOnlyItem , HeightFieldItem , IndexFieldItem , 
                        LocalSpanItem , LocalBiasItem , PrismItem , 
                        DisplayBiasTableItem , Display7x7Item , 
			SetMPEcolorsItem , NumFieldItems } ;


enum popup_menu_items  { ZOOM_IN_PUM = 0,
                         ZOOM_OUT_PUM,
                         RECENTER_PUM,
                         PAN_UP_PUM,
                         PAN_DOWN_PUM,
                         PAN_LEFT_PUM,
                         PAN_RIGHT_PUM,
                         TOGGLE_LAT_LON_PUM,
                         TOGGLE_MPE_LEGEND_INFO_PUM,
                         LAUNCH_TIMESERIES_PUM,
                         LAUNCH_TIMERSERIES_LITE_PUM,
                         LAUNCH_DAMCREST_PUM,
                         NUM_POPUP_MENU_ITEMS_PUM
                       } ;


/* Widget definitions. */
Widget cascade;
Widget gage_cascade ;
Widget gage_color_cascade ;
Widget gage_missing_cascade ;
Widget timelapse_cascade ;

Widget liveData [ NumLiveItems ];
Widget mapData [ NumMapDataItems ];
Widget menu;
Widget mpeControl [ NumMpeControlItems ] ;
Widget hvToolsWidgetArray[NumHvToolItems];
Widget locationShiftCascadeMenuItem = NULL;
static Widget parent = NULL ;
Widget popup_menu ;
Widget product_menu = NULL ;
Widget product;
Widget referenceData [ NumRefItems ] ;
Widget field [ NumFieldItems ] ;
Widget view_cascade;

Widget toplevel ;

/* Global variables involved with the production of the latitude/longitude
   box. */
int copy = 0 ;
int image_x_pos ; /* Contains the x position of the last lat_lon_image. */
int image_y_pos ; /* Contains the y position of the last lat_lon_image. */
int show_latlon ; /* A flag indicating whether or not to display the
                     latitude / longitude pointer tracking box. */ 
extern int toolbar ; /* Contains the flag indicating whether or not the
                        user wants the lat/lon information displayed. */
Pixmap last_pixmap ; /* This pixmap contains a "snapshot" of the map
                        area overwritten by the lat/lon box. */

/* Variables related to the mouse click functionality. */
static Boolean button_one_down = False ;
static int click_count = 0 ;
const int time_interval = 300 ;
static clicks mouse_clicks ;

/* Variable to keep track of when a focus event has just occurred. */
enum MapState focus_flag = M_ON ;

/* Variables related to the lat/lon tracking box. */ 
const int LAT_LON_IMAGE_WIDTH = 75 ;
const int LAT_LON_IMAGE_HEIGHT  = 35 ;
const int LAT_LON_X_OFFSET  = 10 ;
const int LAT_LON_Y_OFFSET  = 0 ;
const int SIZE_OF_LAT_LON_STRING  = 10 ;
const int SIZE_OF_HRAP_POINT_STRING  = 256 ;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:
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

// ---------------------------------------------------------------------------

static void update_tools_menu ( )
{
   extern Widget map_menu_items [ num_of_menu_items ];
    
   
   mCreateMenuSeparator ( map_menu_items [ map_tool_pane ] );
   
   _create_cascade ( map_menu_items [ map_tool_pane ],
                     & hvToolsWidgetArray [ LocShiftMenu ], 
                     & view_cascade , "Location Shift", 'L' ) ;
                     
   mCreateMenuItem ( hvToolsWidgetArray [ LocShiftMenu ],
                     & hvToolsWidgetArray [ EditLocShift ],
                     "Edit Location Shifts...", 'E', NULL, NULL );
   mCreateMenuItem ( hvToolsWidgetArray [ LocShiftMenu ],
                     & hvToolsWidgetArray [ RereadLocShift ],
                     "Reread Location Shifts", 'R', NULL, NULL );
                     
                                          
   /* Add the callbacks for the Tools menu items. */                     
   XtAddCallback ( hvToolsWidgetArray [ EditLocShift ] , XmNactivateCallback ,
                   edit_shift_data_callback , NULL );
   XtAddCallback ( hvToolsWidgetArray [ RereadLocShift ] , XmNactivateCallback ,
                   reread_shift_data_callback , NULL );
  
  
   //   When the locationShift Menu is created have it desensitized.
   // It will be sensitized when the PDC window is opened up.
   locationShiftCascadeMenuItem = hvToolsWidgetArray[LocShiftMenu];
   
   DeSensitize(locationShiftCascadeMenuItem);
   
   
   // add the Color Manager window here.  This is the
   // Java version and replacement of the X-Motif
   // Color Thresholds window.          
   mCreateMenuItem ( map_menu_items [ map_tool_pane ],
                     & hvToolsWidgetArray [ HvColorManager ],
                     "Hydroview Color Manager...", 'H', NULL, NULL );
   XtAddCallback ( hvToolsWidgetArray [ HvColorManager ], XmNactivateCallback,
                   launch_color_manager_callback, NULL ); 

   //add the Color Thresholds window for HydroView here.
   // Remember, this is displayed differently than the Color Thresholds window in
   // MPE Editor.
   mCreateMenuItem ( map_menu_items [ map_tool_pane ],
                     & hvToolsWidgetArray [ HvColorThresholds ],
                     "HydroView Color Thresholds (Deprecated)...", 'C', NULL, NULL );
           
   XtAddCallback ( hvToolsWidgetArray [ HvColorThresholds ] , XmNactivateCallback ,
                   show_color_threshold_callback , NULL );

   //add the Color Manager window for MPE here. 
   //This will enable the color of the QPE fields to be set from Hydroview.
   mCreateMenuItem ( map_menu_items [ map_tool_pane ],
                   & hvToolsWidgetArray [HvMPEColorManager ],
                   "MPE Color Manager...", 'M', NULL, NULL );
   XtAddCallback ( hvToolsWidgetArray [ HvMPEColorManager ], 
                   XmNactivateCallback,
                   launch_mpe_color_manager_callback, NULL );
  
                
   return;
}

// ---------------------------------------------------------------------------

static void _create_menu_mapData ( )
{
   static int map_number = 0;

   mCreateMenu ( &menu , &cascade , "MapData" , 'M' );
   mCreateMenuItem ( menu , & mapData [ PointDisplay ] , 
                     "Point Data Control..." , 'S', NULL, NULL ) ;
   mCreateMenuItem ( menu , & mapData [ StationSelection ] , 
                     "Station Selection..." , 'n', NULL, NULL  ) ;
   mCreateMenuItem ( menu , &mapData[ RefreshData ] , "Refresh Data" , 
                     'f', NULL, NULL  ) ;

   /*-----------------------------------------------*/
   mCreateMenuSeparator ( menu ) ;
   mCreateMenuItem ( menu , & mapData [ DamDisplay ] , 
                     "Dam Locations..." , 'D', NULL, NULL ) ;

   /*-----------------------------------------------*/
   mCreateMenuSeparator ( menu ) ;
   mCreateMenuItem ( menu , & mapData [ BestQPE ] , 
                     "Best Estimate QPE..." , 'B', NULL, NULL  ) ;
   mCreateMenuItem ( menu , &mapData [ ArealDisplay ] , 
                     "Flash Flood Guidance..." , 'l', NULL, NULL  ) ;

   /*-----------------------------------------------*/
   mCreateMenuSeparator ( menu ) ;

   mCreateMenuItem ( menu , & mapData [ ClearDataPDC ] , 
                     "Clear Data" , 'C', NULL, NULL  ) ;
                                       
   
   /* Add the callback routines. */
   XtAddCallback ( mapData [ PointDisplay ] , XmNactivateCallback , 
                   pointdisplayCallback , NULL ) ;
   XtAddCallback( mapData [ StationSelection ] , XmNactivateCallback , 
                  showStationSelectDSCallback , NULL ) ;
   XtAddCallback( mapData [ RefreshData ] , XmNactivateCallback , 
                  refreshDataCallback , NULL ) ;
   XtAddCallback ( mapData [ DamDisplay ] , XmNactivateCallback , 
                   damDisplayCallback , NULL ) ;
   XtAddCallback ( mapData [ BestQPE ] , XmNactivateCallback , 
                   bestQpeCallback , ( XtPointer ) & map_number ) ;	
   XtAddCallback ( mapData [ ArealDisplay ] , XmNactivateCallback , 
                  arealdisplayCallback , NULL ) ;
   XtAddCallback ( mapData [ ClearDataPDC ] , XmNactivateCallback , 
                  clear_data_callback , NULL ) ;
              
}

/*******************************************************************************
* MODULE NUMBER: 1 
* MODULE NAME:   _create_menu_liveData 
* PURPOSE:       This routine creates the live data menu and its associated
*                subelements.  The callbacks for the live menu items are
*                assigned within this routine as well.
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
*   mCreateMenuSeparator   map_library.h     Creates a separator on the menu.
*   XtAddCallback          Xm/Xm.h           Adds a callback to a Widget.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None  (The Motif routines do perform some error messaging of their own.)
*
********************************************************************************
*/

static void _create_menu_liveData ( )
{
  mCreateMenu ( &menu , &cascade , "LiveData" , 'L' ) ;
  mCreateMenuItem ( menu , &liveData [ TimeSeries ] , 
                    "Time Series Graphs/Tables..." , 'T', NULL, NULL  ) ;     
                                 
  mCreateMenuItem ( menu , &liveData [ RiverMonitor ] , 
                    "RiverMonitor..." , 'M',  NULL, NULL  ) ;
                    
  mCreateMenuItem ( menu , &liveData[ SiteSpecificModel ] , 
                    "Site Specific Headwater Model..." , 'e', NULL, NULL  ) ;

  /*-----------------------------------------------------------*/
  mCreateMenuSeparator ( menu ) ;
  mCreateMenuItem ( menu , &liveData[AlertAlarm] , 
                    "Alert and Alarm Data..." , 'A', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &liveData [ QuestionableData ] , 
                    "Questionable and Bad Data..." , 'Q', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &liveData [ RejectData ] , 
                    "Rejected Data Trash Can..." , 'R', NULL, NULL  ) ;

  /*-----------------------------------------------------------*/
  mCreateMenuSeparator ( menu ) ;
  mCreateMenuItem ( menu , &liveData [ StationReporting ] , 
                    "Station Reporting Status/Latest Observations..." , 't', 
                    NULL, NULL  ) ;
  mCreateMenuItem ( menu , &liveData [ PointPrecipitation ] , 
                    "Point Precipitation Accumulations..." , 'o', NULL, 
                    NULL  ) ;

  /*-----------------------------------------------------------*/
  mCreateMenuSeparator ( menu ) ;
  mCreateMenuItem ( menu , &liveData[ StationProfile ] , 
                   "Station Profile..." , 'i', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &liveData[ RiverSummary ], "River Summary..." ,
                    'v', NULL, NULL  ) ;
  mCreateMenuSeparator ( menu ) ;

  /* Add the callback routines. */
  XtAddCallback ( liveData [ TimeSeries ] , XmNactivateCallback , 
                  tsControlCallback, NULL);
  XtAddCallback( liveData [ RiverMonitor ] , XmNactivateCallback , 
                  riverMonitorCallback , NULL ) ;                     
  XtAddCallback( liveData [ SiteSpecificModel ] , XmNactivateCallback , 
                  siteSpecificCallback , NULL ) ;
  XtAddCallback ( liveData [ AlertAlarm ] ,
                  XmNactivateCallback , 
                  alertalarmCallback ,
                  NULL);
  XtAddCallback ( liveData [ QuestionableData ] ,
                  XmNactivateCallback , outOfRangeCallback , NULL ) ;
  XtAddCallback ( liveData [ RejectData ] ,
                  XmNactivateCallback , rejectedDataCallback ,  NULL ) ;
  XtAddCallback ( liveData [ StationReporting ] ,
                  XmNactivateCallback , stationReportCallback , NULL ) ;
  XtAddCallback ( liveData [ PointPrecipitation ] , XmNactivateCallback , 
                  preaccumCallback , NULL );
  XtAddCallback ( liveData [ StationProfile ] , XmNactivateCallback , 
                  stationProfileCallback , NULL ) ;
  XtAddCallback( liveData [ RiverSummary ] , XmNactivateCallback , 
                  hydrobriefCallback , NULL ) ;


}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   _create_menu_referenceData
* PURPOSE:       This routine creates the Reference Data menu and its
*                associated subelements.  It also defines the callback
*                associated with each of the menu items.
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
*   mCreateMenuSeparator   map_library.h     Creates a separator on the menu.
*   XtAddCallback          Xm/Xm.h           Adds a callback to a Widget.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None (although the Xmotif routines do report error messages of their own)
*
********************************************************************************
*/
static void _create_menu_referenceData( )
{
  mCreateMenu ( &menu, &cascade, "ReferenceData" , 'R' ) ;
  mCreateMenuItem ( menu , &referenceData [ StaffGage ] , "Staff Gage..." ,
                    'S', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &referenceData [ ImpactStatement ] ,
                    "Impact Statement..." , 'I', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &referenceData [ LowStatement ] ,
                    "Low Water Statement..." , 'W', NULL, NULL  ) ; 		    
  mCreateMenuItem ( menu , &referenceData [ RatingCurve ] ,
                    "Rating Curve..." , 'R', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &referenceData [ DataSources ] ,
                    "Data Sources..." , 'D', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &referenceData [ Contacts ] , "Contacts..." , 
                    'C', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &referenceData [ CrestHistory ] ,
                    "Crest History..." , 'e', NULL, NULL  ) ;
  mCreateMenuItem ( menu , &referenceData [ TextReports ] ,
                    "Text Reports..." , 'T', NULL, NULL  ) ;

  XtAddCallback ( referenceData [ StaffGage ] ,
                  XmNactivateCallback , staffGageCallback , NULL ) ;
  XtAddCallback ( referenceData [ ImpactStatement ] ,
                  XmNactivateCallback , impactStatementCallback , NULL ) ;
  XtAddCallback ( referenceData [ LowStatement ] ,
                  XmNactivateCallback , lowStatementCallback , NULL ) ;		  
  XtAddCallback ( referenceData [ RatingCurve ] ,
                  XmNactivateCallback , ratingCallback , NULL ) ;
  XtAddCallback ( referenceData [ DataSources ] ,
                  XmNactivateCallback , dataSourcesCallback ,  NULL ) ;
  XtAddCallback ( referenceData [ Contacts ] , XmNactivateCallback ,
                  contactCallback , NULL ) ;
  XtAddCallback ( referenceData [ CrestHistory ] , XmNactivateCallback , 
                  crestHistoryCallback , NULL ) ;
  XtAddCallback ( referenceData [ TextReports ] , XmNactivateCallback ,
                  textreportsCallback , NULL ) ;
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   _create_menu_product 
* PURPOSE:       This routine creates the Hydroview Product menu and defines
*                the actions associated with its menu items.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                HEADER FILE       DESCRIPTION
*   mCreateMenu         map_library.h     Creates a simple menu.      
*   mCreateMenuItem     map_library.h     Creates an item on a simple menu.
*   XtAddCallback       Xm/Xm.h           Adds a callback to a Widget.  In
*                                         this application, it is used to
*                                         assign callbacks to the menu buttons.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None (except the Xmotif routines do report errors to standard error)
*
********************************************************************************
*/
static void _create_menu_product( )
{
  mCreateMenu(&menu,&cascade,"Product" , 'd') ;
  mCreateMenuItem(menu,&product,"Product Viewer..." , 'P', NULL, NULL  ) ;
  XtAddCallback ( product , XmNactivateCallback , productViewerCallback , 
                  NULL ) ;
}

/*******************************************************************************
* MODULE NUMBER: 6
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
static void input ( Widget widget , XtPointer client_data , 
                    XtPointer call_data )
{
   /* Place the code to create the "popup" menu here. */ 
   XmMenuPosition ( popup_menu ,
                    ( XButtonPressedEvent * ) call_data ) ;
   XtManageChild ( popup_menu ) ;
 
}

/*******************************************************************************
* MODULE NUMBER: 7
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

void process_clicks ( XtPointer client_data , XtIntervalId * id )
{
   static Boolean	continue_to_dispatch_return = False ;
   static Boolean       return_to_initial_time = False ;
   float        	lat , lon ;
   extern int   	add_pseudo_flag ;
   extern int   	display_7x7_flag ;
   extern int   	draw_poly_flag ;
   extern rubber_band_data rbdata ;
   Widget               map_widget ;
   XEvent               event ;
   
   int clicks = ( int ) client_data ;
   
   click_count = 0 ;

   if ( clicks )
   {
      if ( mouse_clicks.first_button_press 
           == mouse_clicks.second_button_press )
      {
           if ( mouse_clicks.first_button_press == 1 )
	  
	   { 
                buttonpress_cb (  & mouse_clicks , 0 ) ;
           }
           else if ( mouse_clicks.first_button_press == 2 )
           {
               /* Select the station and launch the time series
                  GUI here. */
                buttonpress_cb (  & mouse_clicks , 1 ) ;
           }
      }
   }
   else
   {
      switch ( mouse_clicks.first_button_press )
      {
         case 1:
	 
            if ( button_one_down == True )
            {
               /* The user is holding down the left mouse button.
                  Enter the functionality which allows the user to
                  draw a rubber band for zooming. */
               map_widget = _get_map_widget ( 0 ) ;
               rbdata.last_x = rbdata.start_x = 0 ;
               rbdata.last_y = rbdata.start_y = 0 ;
               event.xbutton.x = mouse_clicks.x ;
               event.xbutton.y = mouse_clicks.y ;
     
               mpe_start_rubber_band ( map_widget , ( XtPointer ) & rbdata , 
                                       & event , 
                                       & continue_to_dispatch_return ) ;
            }
	    else if ( add_pseudo_flag == 1 ) 
	        locate_pseudo_RFCW ( mouse_clicks.wid , 
                                     ( XtPointer ) & rad_data [ 0 ] ,    
				     ( XEvent * ) & mouse_clicks , 
                                     & continue_to_dispatch_return ) ;  
				     
	    else if ( display_7x7_flag == 1 ) 
	        display_single_gage_RFCW ( mouse_clicks.wid , 
                                         ( XtPointer ) & rad_data [ 0 ] ,    
				         ( XEvent * ) & mouse_clicks , 
                                         & continue_to_dispatch_return ) ;  
            else
	    {
                mSaveOriginalCenterLatLon ( ) ; 
            	mConvertXY2LatLon ( mouse_clicks.x , mouse_clicks.y , & lat ,
                                & lon ) ;
            	mSetCenterLatLon ( lat , lon ) ;
            	_zoom_map_out ( mouse_clicks.wid , & return_to_initial_time , 
                                NULL ) ; 
	    }

            break ;
		
         case 2:
            
            if ( draw_poly_flag == 1 )
                start_end_rubber_poly_RFCW ( mouse_clicks.wid ,
                                             ( XtPointer ) & polydata ,
                                             ( XEvent * ) & mouse_clicks ,
                                             & continue_to_dispatch_return ) ;
            else
            {
               mSaveOriginalCenterLatLon ( ) ; 
               mConvertXY2LatLon( mouse_clicks.x , mouse_clicks.y , & lat ,
                               & lon ) ; 
               mSetCenterLatLon ( lat , lon ) ;
               _zoom_map_in ( mouse_clicks.wid , & return_to_initial_time , 
                              NULL ) ;
            }

            break ;
	   
 	 case 3:
      
         default:
        
            fprintf ( stderr , "Button %d is unsupported.\n" , 
                      mouse_clicks.first_button_press ) ;

       } 
     
   }
 	
}

/*******************************************************************************
* MODULE NUMBER: 8
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
void select_cb ( Widget wid , XtPointer client_data , XEvent * event ) 
{
  Boolean continue_to_dispatch_return = False ;
  extern int draw_poly_flag ;
  extern rubber_band_data rbdata ;
  XtAppContext app ;
  static XtIntervalId id ;

  app = _get_map_context ( ) ;
 
  if ( event->type == ButtonRelease )
  {
     /* Check to see if it was button 1 that was released. */
     if ( event->xbutton.button == 1 )
     {
        button_one_down = False ;

        /* If this was in zoom mode, then end the rubber band. */
        if ( rbdata.rubber_band_zoom_mode == True )
        {
           mpe_end_rubber_band ( wid , ( XtPointer ) & rbdata , event ,
                                 & continue_to_dispatch_return ) ;
        }
     }
    
     return ;
  }

  /* Initialize the elements of the clicks structure. */
  mouse_clicks.x = event->xbutton.x ;
  mouse_clicks.y = event->xbutton.y ;

  /* Set the widget id in the mouse_clicks structure. */
  mouse_clicks.wid = wid ;
  
  /* Test to determine of button 3 has been pressed.  If it has been
     pressed, then process the callback right away. */
  if ( ( event->xbutton.button == 1 ) && ( draw_poly_flag == 1 ) )
  {
     mouse_clicks.first_button_press = event->xbutton.button ;

     start_end_rubber_poly_RFCW ( mouse_clicks.wid , 
                                  ( XtPointer ) & polydata ,   
			          ( XEvent * ) & mouse_clicks , 
                                  & continue_to_dispatch_return ) ;
  }
  else if ( event->xbutton.button == 3 )
  {
     if ( draw_poly_flag == 1 )
     {
        /* Initialize the elements of the clicks structure. */
        mouse_clicks.first_button_press = event->xbutton.button ;
     
        id = XtAppAddTimeOut ( app , time_interval , process_clicks , 
                               False ) ;

        start_end_rubber_poly_RFCW ( mouse_clicks.wid , 
                                     ( XtPointer ) &polydata ,   
    			             (XEvent *) &mouse_clicks , 
                                     & continue_to_dispatch_return ) ;                                                      
     }
     else 
     {
     	input ( wid , client_data , ( XtPointer ) event ) ;
     }
  }
  else
  {
     click_count ++ ;

     if ( event->xbutton.button == 1 )
     {
        button_one_down = True ;
     } 


     if ( click_count == 1 )
     {
        /* Initialize the elements of the clicks structure. */
        mouse_clicks.first_button_press = event->xbutton.button ;
  
        id = XtAppAddTimeOut ( app , time_interval , process_clicks , 
                               False ) ;

     }
     else if ( click_count >= 2 )
     {
        button_one_down = False ;

        /* Initialize the elements of the clicks structure. */
        mouse_clicks.second_button_press = event->xbutton.button ;
   
        XtRemoveTimeOut ( id ) ;
        process_clicks ( ( XtPointer ) True , & id  ) ;
     }
  }
}
     
/*******************************************************************************
* MODULE NUMBER: 9
* MODULE NAME:   exposelegend
* PURPOSE:       Creates the legend which defines the symbols represented on
*                the hydroview display.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                 HEADER FILE       DESCRIPTION
*   mSetLineWidth        map_library.h     Sets the line width used to draw
*                                          the legend symbols. 
*   mSetColor            map_library.h     Sets the color for the text and
*                                          symbols drawn in the legend.
*   mDrawSymbol          map_library.h     This routine draws a user-specified
*                                          symbol on the map.
*   mUpdateMap           map_library.h     This routine redraws the map.  It
*                                          uses a user supplied map-refresh
*                                          routine to do this.
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
/*
void exposelegend()
{
  mSetLineWidth(1);
  mSetColor("Black");
  mDrawText(M_LEGEND,0,50,100,"This is a Legend");
  
  mSetColor("Green");
  mDrawText(M_LEGEND,0,50,30,"Weather");
  mDrawSymbol(M_LEGEND,0,50,40,"Black",M_WEATHER_STATION);
  
  mDrawText(M_LEGEND,0,250,30,"river data");
  mDrawSymbol(M_LEGEND,0,250,40,"Black",M_RIVER_DATA_POINT);
  
  mDrawText(M_LEGEND,0,350,30,"river forecast");
  mDrawSymbol(M_LEGEND,0,350,40,"Black",M_RIVER_FORECAST_POINT);
  
  mDrawText(M_LEGEND,0,450,30,"river reservoir");
  mDrawSymbol(M_LEGEND,0,450,40,"Black",M_RIVER_DATA_POINT_AT_RESERVOIR);
  
  mDrawText(M_LEGEND,0,550,30,"forecast reservoir");
  mDrawSymbol(M_LEGEND,0,550,40,"Black",M_RIVER_FORECAST_POINT_AT_RESERVOIR);
  
  mDrawText(M_LEGEND,0,650,30,"forecast weather");
  mDrawSymbol(M_LEGEND,0,650,40,"Black",M_RIVER_FORECAST_POINT_WITH_WEATHER_STATION);
  
  mDrawText(M_LEGEND,0,750,30,"undefine");
  mDrawSymbol(M_LEGEND,0,750,40,"Black",M_UNDEFINE_STATION);
  mUpdateMap(0);
  
}
*/
/*******************************************************************************
* MODULE NUMBER: 10
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
void toggle_latlon ( )
{
   if ( show_latlon == 0 )
   { 
      show_latlon = 1 ;
   }
   else
   {
      show_latlon = 0 ;

      /* Copy the the lat / lon pixmap onto the screen pixmap. */
      XCopyArea ( _get_map_display ( ) ,
                  last_pixmap ,
                  XtWindow ( _get_map_widget ( 0 ) ) ,
                  _get_map_gc ( ) ,
                  0 ,
                  0 ,
                  LAT_LON_IMAGE_WIDTH ,
                  LAT_LON_IMAGE_HEIGHT ,
                  image_x_pos ,
                  image_y_pos ) ;
      copy = 0 ;
   }

}

/*******************************************************************************
* MODULE NUMBER: 11
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
void toggle_mpemenu ( )
{
   int mpe_data_flag ;
   enum MapState mpe_legend_point_info ;

   /* Test to determine if there is Mpe data displayed. */
   mpe_data_flag = isThereMpeData ( ) ;

   if ( mpe_data_flag != 0 )
   {
      /* There is mpe data. Toggle the "display_data_flag" to "on"
         or "off" depending on its current state. */
      mpe_legend_point_info = getHvDrawLegendStatus ( ) ; 

      if ( mpe_legend_point_info == M_OFF )
      {
         _set_legend_height ( 0 , MPE_LEGEND_HEIGHT + 60 , 0 ) ;
         drawHvLegendPointInfo ( ) ;
      }
      else
      {
         _set_legend_height ( 0 , MPE_LEGEND_HEIGHT , 0 ) ;
         dontDrawHvLegendPointInfo ( ) ;
      }
         
   }
      
}

/*******************************************************************************
* MODULE NUMBER: 12
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
static void popup_cb ( Widget menu_item , XtPointer client_data ,
                       XtPointer call_data )
{


   int item_no = ( int ) client_data ;

   Widget map_widget ; 

   map_widget = _get_map_widget ( 0 ) ;

   switch ( item_no ) 
   {
    
   
      case ZOOM_IN_PUM :
            
         _zoom_map_in ( map_widget , NULL , call_data ) ;
         break ;

      case ZOOM_OUT_PUM :
            
         _zoom_map_out ( map_widget , NULL , call_data ) ;
         break ;

      case RECENTER_PUM :

         _set_recenter_flag ( M_ON ) ;
         mSetCursor ( M_SELECT ) ; 
         break ;

      case PAN_UP_PUM :

         _pan_map_up ( map_widget , NULL , call_data ) ;  
         break ;

      case PAN_DOWN_PUM:

         _pan_map_down ( map_widget , NULL , call_data ) ;
         break ;

      case PAN_LEFT_PUM:

         _pan_map_left ( map_widget , NULL , call_data ) ;
         break ;

      case PAN_RIGHT_PUM:

         _pan_map_right ( map_widget , NULL , call_data ) ;
         break ;


      case TOGGLE_LAT_LON_PUM :

         toggle_latlon ( ) ;
         break ;

      case TOGGLE_MPE_LEGEND_INFO_PUM :
   
         toggle_mpemenu ( ) ;
         break ;
         
      case LAUNCH_TIMESERIES_PUM :

         buttonpress_cb ( & mouse_clicks , 1 ) ;
         break ;
         
      case LAUNCH_TIMERSERIES_LITE_PUM:   
      
         launch_timeserieslite( map_widget, & mouse_clicks );
         break;

      case LAUNCH_DAMCREST_PUM:

         launch_damcrest_cb ( map_widget , & mouse_clicks ) ;
         break ;
         
    

      default :

         fprintf ( stderr , "Unsupported option selected in popup menu.\n"
                            "Button number %d.\n" , item_no ) ;
   }

}

/*******************************************************************************
* MODULE NUMBER: 13
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
void _create_popup_menu ( )
{
   XmString in , out , recenter, up , down , latlon , left , right ,
            mpeinfo , timeseries , timeserieslite, damcrest;

   /* Create the labels for the push buttons on the popup menu. */
   in = XmStringCreateLocalized ( "In" ) ;
   out = XmStringCreateLocalized ( "Out" ) ;
   recenter = XmStringCreateLocalized ( "Recenter" ) ;
   up = XmStringCreateLocalized ( "Up" ) ;
   down = XmStringCreateLocalized ( "Down" ) ;
   left = XmStringCreateLocalized ( "Left" ) ;
   right = XmStringCreateLocalized ( "Right" ) ;
   latlon = XmStringCreateLocalized ( "Lat/Lon" ) ;
   mpeinfo = XmStringCreateLocalized ( "Mpe Info" ) ;
   timeseries = XmStringCreateLocalized ( "Timeseries" ) ;
   timeserieslite = XmStringCreateLocalized ( "TimeSeriesLite" ) ;
   damcrest = XmStringCreateLocalized ( "DamCrest" ) ;
  
   /* Create the popup menu. */
   popup_menu = XmVaCreateSimplePopupMenu 
                         ( _get_map_widget ( 0 ) , "popup" ,
                         popup_cb ,
                         XmVaPUSHBUTTON , in , 'I' , NULL , NULL ,
                         XmVaPUSHBUTTON , out , 'O' , NULL , NULL ,
                         XmVaPUSHBUTTON , recenter , 'R' , NULL , NULL ,
                         XmVaSEPARATOR ,
                         XmVaPUSHBUTTON , up , 'U' , NULL , NULL ,
                         XmVaPUSHBUTTON , down ,'D' , NULL , NULL ,
                         XmVaPUSHBUTTON , left , 'L' , NULL, NULL ,
                         XmVaPUSHBUTTON , right , 'g' , NULL, NULL,
                         XmVaSEPARATOR ,
                         XmVaPUSHBUTTON , latlon , 'T' , NULL , NULL ,
                         XmVaPUSHBUTTON , mpeinfo , 'M' , NULL , NULL ,
                         XmVaSEPARATOR ,
                         XmVaPUSHBUTTON , timeseries , 'e' , NULL , NULL ,
                         XmVaPUSHBUTTON , timeserieslite , 'S' , NULL , NULL ,
                         XmVaPUSHBUTTON , damcrest , 'q' , NULL , NULL ,
                         NULL ) ;

   /* Free the strings used in creating the labels on the pushbuttons
      of the popup menu. */
   XmStringFree ( in ) ;
   XmStringFree ( out ) ;
   XmStringFree ( recenter ) ;
   XmStringFree ( up ) ;
   XmStringFree ( down ) ;
   XmStringFree ( left ) ;
   XmStringFree ( right ) ;
   XmStringFree ( latlon ) ;
   XmStringFree ( mpeinfo ) ;
   XmStringFree ( timeseries ) ;
   XmStringFree ( timeserieslite ) ;
   XmStringFree ( damcrest ) ;

}

/*******************************************************************************
* MODULE NUMBER: 14
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
static void mouse_motion_callback ( Widget wid , XtPointer clientdata ,
                                    XEvent * event )
{
   char str [ SIZE_OF_LAT_LON_STRING ] ;
   char str_value [ SIZE_OF_HRAP_POINT_STRING ] ; 
   float factor ;
   float lat , lon ;
   float value ;
   HRAP pnt ;
   static int first = 0 ;
   int index = (int) clientdata;
   int is_there_mpe_data ;
   int mpe_image_x_pos ;
   int mpe_image_y_pos ;
   int n ;
   enum MapState point_info_legend_state ;
   point hrap ;
   Widget hrap_basin_text = 0 ;
   Widget hrap_county_text = 0 ;
   Widget hrap_precip_text = 0 ;
   Widget hrap_xy_text = 0 ;

   /* Initialize variables to reflect the state of the additional MPE
      HRAP information and the existence of MPE data. */
   is_there_mpe_data = isThereMpeData ( ) ;
   point_info_legend_state = getHvDrawLegendStatus ( ) ;

   /* Check to determine if the user wants the latitude / longitude
      information displayed. */
   if ( show_latlon != 0 )
   {

      /* Determine if this is the very first time in ... */
      if ( first == 0 )
      {
         first = 1 ;
         last_pixmap = XCreatePixmap ( _get_map_display ( ) ,
                                RootWindowOfScreen ( XtScreen ( wid ) ) ,
                                LAT_LON_IMAGE_WIDTH ,
                                LAT_LON_IMAGE_HEIGHT ,
                                DefaultDepthOfScreen ( XtScreen ( wid ) ) ) ;
      }

      /* Check to see if this is the first time that this routine is being
         called since the lat/lon information was turned "on". */
      if ( copy != 0 )
      {
         XCopyArea ( _get_map_display ( ) ,
                     last_pixmap ,
                     XtWindow ( wid ) ,
                     _get_map_gc ( ) ,
                     0 ,
                     0 ,
                     LAT_LON_IMAGE_WIDTH ,
                     LAT_LON_IMAGE_HEIGHT ,
                     image_x_pos ,
                     image_y_pos ) ;
      }

      /* Determine the "x" and "y" coordinates from the position of the
         pointer on the screen. */
      image_x_pos = event->xmotion.x ;
      image_y_pos = event->xmotion.y ;

      /* Compute the latitude and the longitude from this cartesian
         coordinate. */
      mConvertXY2LatLon ( image_x_pos , image_y_pos , & lat , & lon ) ;

      /* Compute the new "x" and "y" positions of the lat/lon information
         pixmap box. */
      image_x_pos += LAT_LON_X_OFFSET ;
      image_y_pos += LAT_LON_Y_OFFSET ;

      /* Copy the portion of the screen pixmap at the cartesian coordinate
         given by image_x_pos and image_y_pos into the last_image
         pixmap. */
      XCopyArea ( _get_map_display ( ) ,
                  XtWindow ( wid ) ,
                  last_pixmap ,
                  _get_map_gc ( ) ,
                  image_x_pos ,
                  image_y_pos ,
                  LAT_LON_IMAGE_WIDTH ,
                  LAT_LON_IMAGE_HEIGHT ,
                  0 ,
                  0 ) ;
      copy = 1 ;

      /* Set the foreground color to white. */
      mSetColor ( "White" ) ;

      /* Write the latitude and the longitude to the lat_lon_image pixmap. */
      memset ( str , '\0' , SIZE_OF_LAT_LON_STRING ) ;
      sprintf(str,"%7.2f",lat);
      _area_draw_text ( XtWindow ( wid ) ,
                        image_x_pos ,
                        image_y_pos + 10 ,
                        str ) ;
      memset ( str , '\0' , SIZE_OF_LAT_LON_STRING ) ;
      sprintf ( str , "%7.2f" , lon );
      _area_draw_text ( XtWindow ( wid ) ,
                        image_x_pos ,
                        image_y_pos + 20 ,
                        str ) ;
   }

   if ( ( is_there_mpe_data == 1 ) && 
        ( point_info_legend_state == M_ON ) )
   {
      /* Determine the "x" and "y" coordinates from the position of the
         pointer on the screen. */
      mpe_image_x_pos = event->xmotion.x ;
      mpe_image_y_pos = event->xmotion.y ;

      /* Compute the latitude and the longitude from this cartesian
         coordinate. */
      mConvertXY2LatLon ( mpe_image_x_pos , mpe_image_y_pos , & lat , & lon ) ;

      /* Convert the latitude/longitude information into an HRAP
         coordinate. */
      pnt = LatLongToHrapMpe ( lat , ( -1 ) * lon ) ;
      hrap.x = ( int ) pnt.x ;
      hrap.y = ( int ) pnt.y ;

      hrap_xy_text = getHvLegendHrapXYText ( ) ;
      hrap_precip_text = getHvLegendHrapValueText ( ) ;
      hrap_county_text = getHvLegendHrapCountyText ( ) ;
      hrap_basin_text = getHvLegendHrapBasinText ( ) ;

      if ( ( hrap.x >= XOR ) && ( hrap.x < ( XOR + MAXX ) ) && 
           ( hrap.y >= YOR ) && ( hrap.y < ( YOR + MAXY ) ) )
      {

         /* Print the values into the text area fields in the Mpe legend. */
         sprintf ( str_value , "%3d, %3d" , hrap.x , hrap.y ) ;
         XmTextSetString ( hrap_xy_text , str_value ) ;

         /* Retrieve the precipitation value underneath the mouse cursor. */
         factor = units_factor * ( float ) scale_factor ;

         if ( strcmp ( cv_use , "LOCSPAN" ) == 0 ||
              strcmp ( cv_use , "PRISM" ) == 0 ||
              strcmp ( cv_use , "LOCBIAS" ) == 0 )
         {
            factor = scale_factor ;
         }

         if ( strcmp ( cv_use , "INDEX" ) == 0 )
         {
            /* Reconfigure the "Value" label to read "Radar". */
            n = rad_data[ index ].data_array [ hrap.x - XOR ] 
                                             [ hrap.y - YOR ] - 1 ;

            if ( n >= 0 )
            {
               sprintf ( str_value , "%s" , nexrad[n].id ) ;
            }
            else
            {
               sprintf ( str_value , "Missing" ) ;
            }

         }
         else
         {
            /* Reconfigure the "Value" label to read "Radar". */
            value = ( float ) 
                    rad_data [ index ].data_array [ hrap.x - XOR ] 
                                                  [ hrap.y - YOR ] / factor ;

            if ( value >= 0 )
            {
              sprintf ( str_value , "%.3f" , value ) ;
            }
            else
            {
              sprintf ( str_value , "Missing" ) ;
            }

         }

         XmTextSetString ( hrap_precip_text , str_value ) ;

         /* Load the county and basin information.  The county is the
            county containing the Hrap Bin the mouse pointer is currently
            on. The basin is the basin containing the Hrap Bin the mouse
            pointer is currently on. */
         if ( overlay_avail.gridtobasin == 0 )
         {
            sprintf ( str_value , "Not Defined" ) ;
         }
         else
         {
            if ( strcmp ( loc_basin [ hrap.y - YOR ] [ hrap.x - XOR ] , "" ) == 0 )
            {
               sprintf ( str_value , "Not Defined" ) ;
            }
            else
            {
               sprintf ( str_value , "%s" , loc_basin [ hrap.y - YOR ] [ hrap.x - XOR ] ) ;
            }
         }

         XmTextSetString ( hrap_basin_text , str_value ) ;

         if ( overlay_avail.gridtocounty == 0 )
         {
            sprintf ( str_value , "Not Defined" ) ;
         }
         else
         {
            if ( strcmp ( loc_cty [ hrap.y - YOR ] [ hrap.x - XOR ] , "" ) == 0 )
            {
               sprintf ( str_value , "Not Defined" ) ;
            }
            else
            {
               sprintf ( str_value , "%s" , loc_cty [ hrap.y - YOR ] [ hrap.x - XOR ] ) ;
            }
         }

         XmTextSetString ( hrap_county_text , str_value ) ;
      }
      else
      {
         /* Out of range.  Cannot process the HRAP point information. */
         sprintf ( str_value , "----" ) ;
         XmTextSetString ( hrap_xy_text , str_value ) ;
         XmTextSetString ( hrap_precip_text , str_value ) ;
         XmTextSetString ( hrap_county_text , str_value ) ;
         XmTextSetString ( hrap_basin_text , str_value ) ;
      }

   }

   return ;
}

/*******************************************************************************
* MODULE NUMBER: 15
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
static void leave_window_callback ( Widget wid , XtPointer clientdata ,
                                    XEvent * event )
{
   int index = ( int ) clientdata;

   if ( show_latlon == 1 )
   {

      /* Copy the the lat / lon pixmap onto the screen pixmap. */
      XCopyArea ( _get_map_display ( ) ,
                  last_pixmap ,
                  XtWindow ( _get_map_widget ( index ) ) ,
                  _get_map_gc ( ) ,
                  0 ,
                  0 ,
                  LAT_LON_IMAGE_WIDTH ,
                  LAT_LON_IMAGE_HEIGHT ,
                  image_x_pos ,
                  image_y_pos ) ;
      copy = 0 ;
   }

}

/*******************************************************************************
* MODULE NUMBER: 16
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
static void about_app_cb ( Widget wid , XtPointer client_data ,
                           XtPointer call_data )
{
   char * hv_name = NULL ;
   char * hv_ver = NULL ;
   char * hv_date = NULL ;

   hv_name = get_hv_name ( ) ;
   hv_ver = get_hv_ver ( ) ;
   hv_date = get_hv_ver_date ( ) ;

   if ( ( hv_name != NULL ) && ( hv_ver != NULL ) && ( hv_date != NULL ) )
   {
      techinfo_show ( wid , hv_name , hv_ver , hv_date ) ;
   }
}


/*******************************************************************************
* MODULE NUMBER: 17
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
void redrawMap ( )
{    
   mUpdateMap ( 0 );
}

/*******************************************************************************
* MODULE NUMBER: 18
* MODULE NAME:  enter_window
* PURPOSE:      The callback when an EnterNotify event is captured.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget
*   Input  XtPointer
*   Input  Event *
*   Input  Boolean *
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                  HEADER FILE       DESCRIPTION
*   get_map_display       map_resource.h    Returns the display of the 
*                                           application.
*   get_map_shell         map.h             Returns the shell of the map
*                                           viewer.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME               DESCRIPTION
*   Display *  hmap_mpe_display   The display containing the hmap_mpe
*                                 application.
*   Widget     shell              The popup shell containing the hmap_mpe map.
*   Window     hmap_mpe_window    The window of the hmap_mpe application.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
void enter_window ( Widget w , XtPointer clientdata , XEvent * event ,
                           Boolean * continue_to_dispatch_return ) 
{
   Display * hmap_mpe_display = NULL ;
   Widget shell ;
   Window hmap_mpe_window ;

   hmap_mpe_display = _get_map_display ( ) ;
   shell = _get_map_shell ( ) ;
   hmap_mpe_window = XtWindow ( shell ) ; 

   XSetInputFocus ( hmap_mpe_display , hmap_mpe_window , RevertToParent ,
                    CurrentTime ) ;
}  

/*******************************************************************************
* MODULE NUMBER: 19
* MODULE NAME:   draw_launch_gui 
* PURPOSE:       This routine creates the hmap_mpe application window
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
*                                        displayed in hmap_mpe. 
*   char *        center_lon_token_name  The name of the token containing
*                                        the center longitude for the map
*                                        displayed in hmap_mpe.
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
void draw_launch_gui ( XtPointer clientdata , XtIntervalId * id )
{
  static char * center_lat_token_name = "CENTER_LAT" ;
  static char * center_lon_token_name = "CENTER_LON" ;
  static char * hv_width_in_pixels_token_name = "hv_width_in_pixels" ;
  static char * hv_height_in_pixels_token_name = "hv_height_in_pixels" ;
  static char res_title [ ] = "hydroview_res" ;
  char reply [ LEN_REPLY ] ;
  static char * width_token_name = "NMI_WIDE" ;
  float latitude ;
  float longitude ;
  float width_in_nmi ;
  HvColorList * hcl = getHvColorList ( ) ;
  static int first = 1 ;
  int hv_height_in_pixels ;
  int hv_width_in_pixels ;
  int reply_len ;
  int request_len ;
  int status ;
  extern rubber_band_data rbdata ;
  Widget shell ;
  Widget map_widget ;
  extern Widget map_menu_items[];
  XtAppContext app ; 

  Widget * working = ( Widget * ) clientdata ;

  if ( ! first ) return;
  first = 0;

  /* Allocate memory for the widget_struct pointer. */
  widget_struct = ( rfcwide_widget_struct * )
                    malloc ( sizeof ( rfcwide_widget_struct ) ) ;

  if ( widget_struct == NULL )
  {
     fprintf ( stderr , "In routine \"button_cb\":\n"
                        "The attempt to allocate memory for the\n"
                        "\"widget_struct\" data member failed.  The\n"
                        "construction of the \"hmap_mpe\" application\n"
                        "is being aborted.\n" ) ;
     return ;
  }

  /* Retrieve the station latitude and longitude from the 
     "apps defaults" files. */
  request_len = strlen ( center_lat_token_name ) ;
  status = get_apps_defaults ( center_lat_token_name , & request_len , reply , 
                               & reply_len ) ;

  if ( status != 0 )
  {
     fprintf ( stderr , "\nIn routine button_cb:\n"
                        "Couldn't retrieve the center latitude of the\n"
                        "Hydroview viewer.  The construction of the viewer\n"
                        "is being stopped.\n" ) ;
     return ;
  }

  latitude = ( float ) atof ( reply ) ;
  request_len = strlen ( center_lon_token_name ) ;
  status = get_apps_defaults ( center_lon_token_name , & request_len , reply , 
                               & reply_len ) ;
  
  if ( status != 0 )
  {
     fprintf ( stderr , "\nIn routine button_cb:\n"
                        "Couldn't retrieve the center longitude of the\n"
                        "Hydroview viewer.  The construction of the viewer\n"
                        "is being stopped.\n" ) ;
     return ;
  }

  longitude = ( float ) atof ( reply ) ;

  /* Retrieve the desired width in nautical miles of the viewer. */
  request_len = strlen ( width_token_name ) ;
  status = get_apps_defaults ( width_token_name , & request_len ,
                               reply , & reply_len ) ; 

  if ( status != 0 )
  {
     fprintf ( stderr , "\nIn routine button_cb:\n"
                        "Couldn't retrieve the width in nautical miles of\n"
                        "the Hydroview viewer.  Token %s is expected to\n"
                        "contain this value.  The construction of the\n"
                        "viewer is being stopped.\n" , width_token_name ) ;
     return ;
  }

  width_in_nmi = ( float ) atof ( reply ) ;


  /* Configure the data viewer. */
  mSetCenterLatLon ( latitude , longitude ) ;

  /* Retrieve the width and height in pixels of the viewing area. */
  request_len = strlen ( hv_width_in_pixels_token_name ) ;
  status = get_apps_defaults ( hv_width_in_pixels_token_name , 
                               & request_len , reply , 
                               & reply_len ) ;

  if ( status != 0 )
  {
     fprintf ( stderr , "\nIn routine button_cb:\n"
                        "Couldn't retrieve the width in pixels of\n"
                        "the viewing area from token %s.  Setting the\n"
                        "width to the default value %d.\n" ,
                         hv_width_in_pixels_token_name ,
                         HV_DEFAULT_WIDTH_IN_PIXELS ) ;
     hv_width_in_pixels = HV_DEFAULT_WIDTH_IN_PIXELS ;
  }
  else
  {
     hv_width_in_pixels = atoi ( reply ) ;

     if ( hv_width_in_pixels <= 0 )
     {
        fprintf ( stderr , "\nIn routine button_cb:\n"
                           "Invalid width in pixels %d retrieved from token\n"
                           "%s.  Setting the width to the default value %d.\n" ,
                           hv_width_in_pixels , hv_width_in_pixels_token_name ,
                           HV_DEFAULT_WIDTH_IN_PIXELS ) ;
        hv_width_in_pixels = HV_DEFAULT_WIDTH_IN_PIXELS ;
     }
  }

  request_len = strlen ( hv_height_in_pixels_token_name ) ;
  status = get_apps_defaults ( hv_height_in_pixels_token_name , 
                               & request_len , reply , 
                               & reply_len ) ;

  if ( status != 0 )
  {
     fprintf ( stderr , "\nIn routine button_cb:\n"
                        "Couldn't retrieve the height in pixels of\n"
                        "the viewing area from token %s.  Setting the\n"
                        "height to the default value %d.\n" ,
                         hv_height_in_pixels_token_name ,
                         HV_DEFAULT_HEIGHT_IN_PIXELS ) ;
     hv_height_in_pixels = HV_DEFAULT_HEIGHT_IN_PIXELS ;
  }
  else
  {
     hv_height_in_pixels = atoi ( reply ) ;

     if ( hv_height_in_pixels <= 0 )
     {
        fprintf ( stderr , "\nIn routine button_cb:\n"
                           "Invalid height in pixels %d retrieved from token\n"
                           "%s.  Setting the height to the default\n"
                           "value %d.\n" ,
                           hv_height_in_pixels , 
                           hv_height_in_pixels_token_name ,
                           HV_DEFAULT_HEIGHT_IN_PIXELS ) ;
        hv_height_in_pixels = HV_DEFAULT_HEIGHT_IN_PIXELS ;
     }
  }

  mInitMapScreen ( hv_height_in_pixels , hv_width_in_pixels , 0 , 0 , 
                   width_in_nmi , M_OFF , M_TOP , "HydroView" ) ;
  mInitLegend ( M_OFF , M_BOTTOM , MPE_LEGEND_WIDTH , MPE_LEGEND_HEIGHT ) ;
  mInitMap ( 1 , M_OFF , hcl->background_color , hcl->state_border_color ) ;
  mCreateMapScreen ( parent , res_title ) ;


  /* Read the overlay information. */
  status = read_overlay_configuration ( ) ;

  if ( status != M_OK )
  {
     fprintf ( stderr , "\nIn routine button_cb:\n"
                        "An error was encountered by routine\n"
                        "\"read_overlay_configuration\".  The construction\n"
                        "of the viewer is being stopped.\n" ) ;
     return ;
  }

  

  /* Set the amount of time that will be used to distinguish between
     a single mouse click and a double mouse click. */
  shell = _get_map_shell ( ) ;
  XtSetMultiClickTime ( XtDisplay ( shell ) , time_interval ) ;
 
  /* Check to see if this application is being used at Guam.
     Because Guam is so far removed from the rest of the U.S.,
     it gets its own special box defined for it. */
  if ( longitude > 0 )
  {
     /* A positive longitude means that we are in the Eastern Hemisphere.
        Guam is the only WFO in this part of the world. */
    mSetRegionArea ( HV_GUAM_REGION_TOP_LAT , HV_GUAM_REGION_LEFT_LON ,
                     HV_GUAM_REGION_BOTTOM_LAT , HV_GUAM_REGION_RIGHT_LON ) ;
  }
  else
  {
     mSetRegionArea ( HV_REGION_TOP_LAT , HV_REGION_LEFT_LON , 
                      HV_REGION_BOTTOM_LAT , HV_REGION_RIGHT_LON ) ;
  }

  /* Add user-defined callback routines to the map library. */
  /* Add the callbacks for map 0. */
  mAddLegendExpose ( 0 , drawHvLegend ) ;
  mAddLegendDrawCb ( 0 , ( MotionCallback ) stationLegendCallback ) ;
  mAddMapExpose ( 0 , redrawBase ) ;
  mAddMapSelectRoutine( 0 , select_cb) ;
  mAddMotionCallback ( 0 , mouse_motion_callback ) ;
  mAddLeaveCallback ( 0 , leave_window_callback ) ;
  mAddAboutAppCallback ( ( MotionCallback ) about_app_cb ) ;
  mAddLegendSelectRoutine ( 0, processHvLegendMouseClick );


  /* Add the callbacks for map 1. */
  /* Hydroview can't have a split screen.  There is no
     need to define callbacks on map 1. */
  /*mAddLegendExpose ( 1 , drawHvLegend ) ;
  mAddLegendDrawCb ( 1 , ( MotionCallback ) stationLegendCallback ) ;
  mAddMapExpose ( 1 , redrawBase ) ;
  mAddMapSelectRoutine( 1 , map1_select_cb) ;
  mAddMotionCallback ( 1 , mouse_motion_callback ) ;
  mAddLeaveCallback ( 1 , leave_window_callback ) ;*/

  /* Add the callback for the case when the user holds mouse button 1
     while and drags the mouse. This is used for drawing a zoom rectangle. */
  map_widget = _get_map_widget ( 0 ) ; 
  XtAddEventHandler ( map_widget , Button1MotionMask , FALSE , 
                      mpe_track_rubber_band , ( XtPointer ) & rbdata ) ;

  /* Define special actions that must be performed when changing the
     orientation of the map viewing area. */
  mAddPanRoutine ( hv_pan_callback ) ;
  mAddZoomRoutine ( hv_zoom_callback ) ;
  mAddRecenterRoutine ( hv_recenter_callback ) ;

  /* Create the Hmap_mpe-specific menus. */ 
  /* Added the creation of the map data menu on 11/22/2005.  Bryon L. */
  _create_menu_mapData ( );
  _create_menu_liveData ( ) ;
  _create_menu_referenceData ( ) ;
  _create_menu_product ( ) ;
  
  update_tools_menu();
  

  /* Removed the mpe control menu on 11/22/2005.  Bryon L. */
  /*_create_menu_mpe_control ( ) ;*/
  /* Removed the MPE fields menu on 11/22/2005.  Bryon L. */
  /*_create_menu_field ( ) ; */

  /* Initialize the menu item widgets that the mpe software
     needs to be able to sensitize and desensitize. This is not
     the best solution, but it will work for now. */

  widget_struct->choose_widget = NULL;
  widget_struct->next_widget = NULL;
  widget_struct->prev_widget = NULL; 
  widget_struct->clear_widget = NULL; 
  widget_struct->radar_site_widget = NULL; 
  timelapse_widget = NULL; 
  widget_struct->gage_widget = NULL;
  /* DeSensitize ( mpeControl [ MpeGageMenu ] ) ;*/
  widget_struct->rerun_widget = NULL; 

  widget_struct->timelapse6_widget = NULL;
  widget_struct->timelapse12_widget = NULL;
  widget_struct->timelapse24_widget = NULL;
  widget_struct->timelapseother_widget = NULL;

  widget_struct->stoptime_widget = NULL; 
  widget_struct->transmit_rfc_qpe = NULL;
  widget_struct->rfc_qpe_mosaic = NULL;
  showids_widget = NULL;
  showval_widget = NULL; 
  
  
  //Added by Ram
  //-----------
  showval_widget = NULL;
  //----------
  
  /* Initialize the "Fields" menu items.  These will also need to be 
     "Sensitized" or "Desensitized" depending on whether there is
     Mpe data being displayed on the screen or not. */
  rmosaic_widget = NULL;
  avgrmosaic_widget = NULL; 
  maxrmosaic_widget = NULL;
  bmosaic_widget = NULL;
  mmosaic_widget = NULL;
  mlmosaic_widget = NULL;
  p3lmosaic_widget = NULL;
  gageonly_widget = NULL;
  xmrg_widget = NULL;
  multihour_widget = NULL;
  height_widget = NULL;
  index_widget = NULL;
  locspan_widget = NULL;
  locbias_widget = NULL;
  prism_widget = NULL;
  satprecip_widget = NULL;
  lsatprecip_widget = NULL;
  bias_widget = NULL;
  widget_struct->display_bias_widget = NULL;
  widget_struct->single_gage_widget = NULL;
  savemaintop_widget = NULL;
  savemainbottom_widget = NULL;
  drawpoly_widget = NULL;
  deletepoly_widget = NULL;
  fullscreen_widget  = NULL; 
  splitscreen_widget = NULL;

  XtVaSetValues ( * working , XmNmessageString ,
                  XmStringCreateLtoR ( "Initializing Point Data ..." ,
                  XmSTRING_DEFAULT_CHARSET ) , NULL ) ;

  app = XtWidgetToApplicationContext ( * working ) ;

  XtAppAddTimeOut ( app , 
                    GUI_BUILDER_INIT_PAUSE_DURATION , 
                    init_point_data , 
                    ( XtPointer ) working ) ;
  
}

/*******************************************************************************
* MODULE NUMBER: 20
* MODULE NAME:   init_point_data
* PURPOSE:       This routine initializes the Hydromap portion of the 
*                hmap_mpe application.  This initialization centers around
*                reading point data from the hydrology database.  This routine
*                has been specifically designed to be in a chain of Motif
*                "time out" callbacks which update the text message in the
*                working dialog box to keep the user informed of the status
*                of the running program.
*
* ARGUMENTS:
*   TYPE   DATA TYPE      NAME         DESCRIPTION/UNITS
*   Input  XtPointer      clientdata   Contains a pointer to the working
*                                      dialog box widget.
*   Input  XtIntervalId * id           Contains a pointer to the identifier
*                                      of the Motif "time out" which is
*                                      causing this program to be run.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                  HEADER FILE             DESCRIPTION
*  getHvDisplayControl    HvDisplayControlProto.h Retrieves the 
*                                                 HvDisplayControl structure.
*   _get_map_widget       map.h                   Retrieves the widget of
*                                                 the drawing area of the
*                                                 map window.
*  initHvDisplayControl   HvDisplayControlProto.h Initializes the point data
*                                                 in HvDisplayControl
*                                                 structure.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE          NAME        DESCRIPTION
*   HvDisplayControl * hdc         A pointer to the structure containing
*                                  the river gage point data.
*   Widget             top_widget  The widget that represents the 
*                                  drawing area of the map display.
*   Widget *           working     A pointer to the working dialog box widget.
*   XtAppContext       app         The application context.
*
* DATA FILES AND/OR DATABASE:
*   Not applicable.
*
* ERROR HANDLING:
*   Not Applicable.
*
********************************************************************************
*/

void init_point_data ( XtPointer clientdata , XtIntervalId * id )
{
  int is_displayed ;
  Widget top_widget ;
  XtAppContext app ;

  Widget * working = ( Widget * ) clientdata ;

  /* Retrieve the Hydromap Display Control structure that will
     be used by this application. */
  HvDisplayControl * hdc = getHvDisplayControl ( ) ;

  top_widget = _get_map_widget ( 0 ) ;

  /* Only initialize the HV river data if this application
     is being run in non-suppressed mode. */
  is_displayed = is_riverstatus_displayed ( ) ;

  if ( is_displayed == 1 )
  {
     initHvDisplayControl ( top_widget , hdc , redrawMap , 1 ) ;
  }
  else
  {
     disableStationDrawing ( ) ;
  }

  XtVaSetValues ( * working , XmNmessageString ,
                  XmStringCreateLtoR ( "Reading Mpe Data ..." ,
                  XmSTRING_DEFAULT_CHARSET ) , NULL ) ;

  app = XtWidgetToApplicationContext ( * working ) ;
  XtAppAddTimeOut ( app , 
                    GUI_BUILDER_INIT_PAUSE_DURATION , 
                    init_mpe_data , 
                    ( XtPointer ) working ) ;
}

/*******************************************************************************
* MODULE NUMBER: 21
* MODULE NAME:   init_mpe_data
* PURPOSE:       This routine initializes the MPE portion of the hmap_mpe
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
*   initMpe                static           The routine which defines all of
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

void init_mpe_data ( XtPointer clientdata , XtIntervalId * id )
{
  static char * hv_map_projection_token_name = "hv_map_projection" ;
  char reply [ LEN_REPLY ] ;
  GC gc ;
  int reply_len ; 
  int request_len ;
  int projection ;
  int status ;
  Widget map_widget ;
  Widget overlay_widget ;
  XtAppContext app ; 

  Widget * working = ( Widget * ) clientdata ;

  /* Initialize and define the Mpe (Multi-sensor Precipitation Estimator)
     control variables. */
  gc = _get_map_gc ( ) ;
  map_widget = _get_map_widget ( 0 ) ;
 
  status = ( int ) initMpeControl ( map_widget , & gc ) ;

  if ( status != ( int ) HydroStatus_OK )
  {

     if ( status != ( int ) HydroStatus_NoCoordFile )
     {
        fprintf ( stderr , "\nIn routine \"gui_builder\":\n"
                           "The call to initMpe failed with a\n"
                           "HydroStatus error code of %d.  The construction\n"
                           "of the viewer is being stopped.\n" , status ) ; 
        exit ( 0 ) ;
     }

     /* The Mpe coordinate data file could not be found.  Desensitize the
        "Choose Hour" option on the MpeControl menu. Also, desensitze the
        HRAP grid overlay option */
     DeSensitize ( mapData [ BestQPE ] ) ;
 
     overlay_widget = _get_menu_item_toggle_widget ( map_hrap_grid ) ;
     DeSensitize ( overlay_widget ) ; 
  
  }

  /* Allocate memory for the display data arrays. */
  initialize_display_memory ( 0 );

  /* Create the popup menus that will be activated by the third mouse
     button. */
  _create_popup_menu ( ) ;

  /* Set the map projection based on the map projection token.  There
     are three supported map projections: Flat, Polar Stereographic, and
     HRAP.   This can only be done at this point because the HRAP 
     projection needs data from the MPE coordinate file which are
     read in this routine. */
  /* Retrieve the station latitude and longitude from the 
     "apps defaults" files. */
  request_len = strlen ( hv_map_projection_token_name ) ;
  status = get_apps_defaults ( hv_map_projection_token_name , & request_len , 
                               reply , & reply_len ) ;

  if ( ( status == 0 ) && ( reply_len > 0 ) )
  {
     switch ( reply [ 0 ] )
     {
        case 'f' :
        case 'F' :
           projection = M_FLAT ;
           break ;
 
        case 'p' :
        case 'P' :
           projection = M_POLAR ;
           break ;

        case 'h' :
        case 'H' :
           projection = M_HRAP ;
           break ;

        default :
           projection = M_FLAT ;
           break ;
     }
  }
  else
  {
     projection = M_FLAT ;
  }

  mSetMapProjection ( projection ) ;  

  /* Open the map screen. */
  mOpenMapScreen ( ) ;

  XtVaSetValues ( * working ,
                  XmNmessageString ,
                  XmStringCreateLtoR ( "GUI Creating Complete ... " ,
                  XmSTRING_DEFAULT_CHARSET ) ,
                  NULL ) ;

  app = XtWidgetToApplicationContext ( * working ) ;
  XtAppAddTimeOut ( app , 
                    GUI_BUILDER_INIT_PAUSE_DURATION , 
                    popdown_dialog_window , 
                    ( XtPointer ) working ) ;

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
void popdown_dialog_window ( XtPointer clientdata , XtIntervalId * id )
{
   Widget * working = ( Widget * ) clientdata ;
   XtUnmanageChild ( * working ) ;
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

void set_parent_widget ( Widget w )
{
   parent = w ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/whfs/src/hydroview/RCS/gui_builder.c,v $";
 static char rcs_id2[] = "$Id: gui_builder.c,v 1.74 2007/08/08 20:38:14 lawrence Exp $";}
/*  ===================================================  */

}
