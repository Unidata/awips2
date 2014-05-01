/*******************************************************************************
* FILENAME:            hv_mainCallbacks.c
* NUMBER OF MODULES:   6
* GENERAL INFORMATION:
*   MODULE 1:          redrawBase
* DESCRIPTION:         This module redraws the various overlays
*                      in the correct "order" to insure that
*                      the images overlay correctly on the screen.
*                      This routine is the "work horse" when it comes to
*                      the screen changing due to an added overlay or
*                      graphic.
*   MODULE 2:          buttonpress_cb
* DESCRIPTION:         This routine is called when the user has clicked
*                      a mouse button in an attempt to highlight a gage
*                      point on the map.  The exact functionality of the
*                      mouse as it relates to selecting a station (single
*                      click, double click, etc.) is determined by the
*                      routine that calls this module.
*   MODULE 3:          arealdisplayCallback
* DESCRIPTION:         This routine is responsible for launching the
*                      "Areal Display" control GUI.
*   MODULE 4:          refreshDataCallback
* DESCRIPTION:         This routine refreshes the station data displayed
*                      on the hydromap application.
*   MODULE 5:          selectStation
* DESCRIPTION:         This routine writes the name of the selected station
*                      across the title bar of the hydromap application.
*   MODULE 6:          stationLegendCallback
* DESCRIPTION:         This module launches the station legend explaining
*                      the point data symbols and colors portrayed on the
*                      hydromap gui.
*
* ORIGINAL AUTHOR:     Unknown
* CREATION DATE:       Unknown
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <unistd.h>
#include <Xm/Xm.h>
#include "clear_data_RFCW.h"
#include "color_threshold.h"
#include "color_threshold_show.h"
#include "hv_color_threshold.h" //for COLOR_USE_NAME_LENGTH only
#include "display_accum_show.h"
#include "display_field_data_RFCW.h"
#include "display_mean_areal_precip.h"
#include "ffgcontrol_show.h"
#include "drawa.h"
#include "drawMpeLegend.h"
#include "drawDams.h"
#include "drawStations.h"
#include "GenericLegendDialog.h"
#include "GeneralUtil.h"
#include "get_mpe_colors.h"
#include "HvCallbacks.h"
#include "hv_mainCallbacks.h"
#include "map.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_menubar_cb.h"
#include "map_resource.h"
#include "plot_routines.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_show.h"
#include "pointcontrol_legend.h"
#include "PointDisplayControl.h"
#include "post_functions.h"
#include "read_netcdf_ffg.h"
#include "rfcwide_callbacks.h"
#include "stage3.h"
#include "stage3_interface.h"
#include "time_lapse_RFCW.h"
#include "tsgen_info.h"

static int highlight_flag = 0;

extern void drawMpeGageTriangles();

#define COLOR_DISPLAY_NAME_LENGTH 40

/*******************************************************************************
* MODULE NUMBER:
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
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
static void drawMpeRadarData ( int map_index )
{
   Display * display = NULL ;
   int data_flag ;
   int height ;
   int width ;
   GC gc       = 0 ;
   NamedColorSetGroup * pColors = NULL ;
   Pixmap destination_pix = 0 ;
   Pixmap source_pix = 0 ;
   Pixmap topo_pixmap = 0;
   int multi_hour_ids_status ;
   int multi_hour_values_status ;

   struct _Overlay * pOverlay = NULL;
   pOverlay = mGetOverlay ( M_TOPOGRAPHY );

   data_flag = isThereMpeData ( ) ;

   printf ( "Refreshing displayed MPE data ... mpe data flag = %d\n", data_flag );

   if ( data_flag != 0 )
   {
       /* Force an update of the color information.  This needs
         to be done in case the user has modified the color
         information through the ColorThresholds window. */
      pColors = get_mpe_default_colors ( );
      MPEGui_set_colorvalues ( & rad_data [ map_index ], pColors );

      /* Set the draw struct corresponding to the map
         being displayed. */
      setDrawStruct ( & rad_data [ map_index ] );

      display  = _get_map_display ( ) ;
      destination_pix = _get_map_pixmap ( ) ;

      /* Create and fill the Mpe data pixmap. */
      /* Check if the topography overlay is displayed.  If it is,
         then pass it into the MPE data drawing routines so that they
         can draw on top of it. */
      if ( pOverlay->status == M_ON )
      {
         topo_pixmap = get_topo_pixmap ( );
         source_pix = getMpeDataPixmap ( topo_pixmap );
      }
      else
      {
         source_pix = getMpeDataPixmap ( 0 );
      }

      if ( source_pix != 0 )
      {
         gc = getMpeDataGC ( ) ;

         if ( gc != 0 )
         {

            /* Retrieve the map width and map height. */
            width = _get_map_width ( map_index ) ;
            height = _get_map_height ( map_index ) ;

            /* Copy the pixmap containing the Mpe multisensor precipitation
               estimates to the map library pixmap that will be drawn on the
               Hmap_mpe viewer. */
            XCopyArea ( display , source_pix , destination_pix , gc , 0 , 0 ,
                        width , height , 0 , 0 ) ;

         }
      }

      if ( rad_data [ map_index ].field_type == display_multiHour )
      {
         if ( ( multi_hour_ids_status = isThereMultiHourIds ( ) ) == 1 )
         {
           /* draw the value of the mean precip and station id */
           drawMultiHourGeoAccumLabels ( ) ;
         }
         if ( ( multi_hour_values_status = isThereMultiHourValues ( ) ) == 1 )
         {
           /* draw the value of the mean precip and station id */
           drawMultiHourGeoAccumLabels ( ) ;
         }
      }
   }
}

/*******************************************************************************
* MODULE NUMBER:
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

/*******************************************************************************
* MODULE NUMBER:
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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/

void redrawBase ( int * map_index )
{
   char header[] = "redrawBase(): ";

   printf("%s inside \n", header);

   HvDisplayControl    *hdc = getHvDisplayControl();
   int state ;

   /* Clear the map. */
   _clear_map ( * map_index );

   /* Draw the topography data.  If MPE data are displayed, the
      topography field should share the same pixmap. */
   _draw_topography_overlay ( * map_index );


   /* draw the Mpe radar/precipitation data (if it exists). */
   drawMpeRadarData ( * map_index ) ;

   /* Draw the Ffg data ( if there is any ) */
   drawFfgHrapGrid ( hdc ) ;

   /* Draw the map overlays. */
   _draw_map_overlays ( * map_index ) ;

   /* draw the Dams overlay (if there is one). */
   drawAllDamDataSets ( ) ;

   /* draw the single point data overlay (if there is one). */

   printf("%s before  drawAllStationDataSets() \n", header);
   drawAllStationDataSets ( ) ;
   printf("%s after  drawAllStationDataSets() \n", header);

   mUpdateLegend( 0 );

   /* draw map with Mpe gage identificators */
   drawMpeGageIds ( * map_index ) ;

   /* draw map with Mpe gage values */
   drawMpeGageValues ( * map_index ) ;

   drawMpeGageTriangles();

   /* Highlight the currently selected gage, if there is one.  Only do this if
      the point data is being drawn on the screen. */
   state = getStationDrawingState ( ) ;

   if ( state == 1 )
   {
      if ( highlight_flag == 1 )
      {
         mapHighlightStation ( ) ;
      }
   }

   return;
}

/*******************************************************************************
* MODULE NUMBER:
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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/

void buttonpress_cb ( clicks  * mouse_clicks ,
                      int launch_timeseries )
{
   char header[] = "buttonpress_cb():  ";

   int          new_station ;
   int          x;
   int          y;
   Station * station;
   Widget w ;

   x = mouse_clicks->x ;
   y = mouse_clicks->y ;

   station = locateStation ( x , y , & new_station ) ;

   if (station == NULL)
   {
       printf("%s station is NULL \n", header);
   }



   if ( station != NULL )
   {

      printf("%s clicked station lid = :%s: paramCode = :%s: \n",
                  header, station->lid, station->paramCode);


      if ( ( new_station == 0 ) &&
           ( highlight_flag == 1 ) &&
           ( launch_timeseries == 0 ) )
      {
          highlight_flag = 0 ;  //deselect the station
          mUpdateMap ( 0 ) ;

          printf("%s deselecting station lid = :%s: paramCode = :%s: \n",
                  header, station->lid, station->paramCode);

      }

      else  /* new station */ // select the station
      {
         /* set hdc->curLid, set the tool form label,
            and set the TB's */

         if ( ( new_station == 1 ) || ( highlight_flag != 1 )  )
         {
            printf("%s selecting station lid = :%s: paramCode = :%s: \n",
                    header, station->lid, station->paramCode);

            selectStation ( station , True ) ;
         }

         mUpdateMap ( 0 );

         /* Launch Timeseries. */
        if ( launch_timeseries == 1 )
        {
           w = _get_map_shell ( ) ;
           tsControlCallback ( w , NULL , NULL ) ;
        }
      }

   }

   return;
}

/*******************************************************************************
* MODULE NUMBER:
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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/

void arealdisplayCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{

   /* call up ffg displayControl Window */
   ffg_display_show ( w ) ;

   return;
}

/*******************************************************************************
* MODULE NUMBER:
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

void precipAccumCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{

   /* call up ffg displayControl Window */
   precip_accum_show ( w ) ;

   return;
}

/****************************************************************************/

void refreshDataCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{

   mSetCursor ( M_WATCH ) ;
   refreshStationData ( w ) ;
   mSetCursor ( M_NORMAL ) ;

   return;
}

/****************************************************************************/

void showStationSelectDSCallback(Widget w, XtPointer ptr, XtPointer cbs)
{

   if ( ( selectDS == NULL ) || ( ! XtIsManaged ( selectDS ) ) )
      ShowSelectDS( _get_top_level ( ) );

   else
   {
      XtUnmapWidget(selectDS);
      XtMapWidget(selectDS);
   }

   return;
}

/****************************************************************************/

void stationLegendCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   XmToggleButtonCallbackStruct * state = ( XmToggleButtonCallbackStruct * )
                                          cbs ;

   static GenericLegendDialog dialog;

   if ( state->set )
   {
      if ( ( dialog.shell ) && XtIsManaged ( dialog.shell ) )
      {
         XtUnmapWidget ( dialog.shell ) ;
         XtMapWidget ( dialog.shell ) ;
      }
      else
      {
         startStationLegend ( w, & dialog ) ;
      }
   }
   else
   {
      if ( ( dialog.shell ) && XtIsManaged ( dialog.shell ) )
      {
         gldCancel ( & dialog ) ;
      }
   }

   return ;
}

/*******************************************************************************
* MODULE NUMBER:
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
void hv_zoom_callback ( Widget w , XtPointer client_data ,
                        XtPointer call_data )
{
   HydroStatus status ;
   int data_flag ;

   /* Add actions here that must be performed when the map is zoomed into
      or out from. */

   /* If MPE data is being displayed, update the pixel grid. */
   data_flag = isThereMpeData ( ) ;

   /* Set the topo change flag. The topography overlay needs to be
      recomputed everytime the map orientation changes. */
   set_topo_change_flag ( );

   if ( data_flag != 0 )
   {
       status = createPixelGrid ( ) ;

       if ( status != HydroStatus_OK )
       {
          fprintf ( stderr , "\nIn routine 'hv_zoom_call_back':\n"
                             "An error was encountered modifying the\n"
                             "pixel grid.  MPE data will not be displayed\n"
                             "properly.\n" ) ;
       }

   }

   /* If the time lapse is looping, then stop it. */
   end_time_lapse_RFCW ( w , client_data , call_data ) ;
}

/*******************************************************************************
* MODULE NUMBER:
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
void hv_pan_callback ( Widget w , XtPointer client_data ,
                       XtPointer call_data )
{

   HydroStatus status ;
   int data_flag ;

   /* Add actions here that must be performed when the map is panned. */

   /* If MPE data is being displayed, update the pixel grid. */
   data_flag = isThereMpeData ( ) ;

   /* Set the topo change flag. The topography overlay needs to be
      recomputed everytime the map orientation changes. */
   set_topo_change_flag ( );

   if ( data_flag != 0 )
   {
       status = createPixelGrid ( ) ;

       if ( status != HydroStatus_OK )
       {
          fprintf ( stderr , "\nIn routine 'hv_zoom_call_back':\n"
                             "An error was encountered modifying the\n"
                             "pixel grid.  MPE data will not be displayed\n"
                             "properly.\n" ) ;
       }

   }

   /* If the time lapse is looping, then stop it. */
   end_time_lapse_RFCW ( w , client_data , call_data ) ;
}
/*******************************************************************************
* MODULE NUMBER:
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
void hv_recenter_callback ( Widget w , XtPointer client_data ,
                            XtPointer call_data )
{
   HydroStatus status ;
   int data_flag ;

   extern rubber_band_data rbdata ;

   /* Set up the CTRL-Z Hot Key so that it will return the user to
      pRbData initial view of the map and any previously drawn rectangles
      are ignored. */
   rbdata.zoom_state = True ;
   rbdata.use_rectangle = False ;

   /* If MPE data is being displayed, update the pixel grid. */
   data_flag = isThereMpeData ( ) ;

   /* Set the topo change flag. The topography overlay needs to be
      recomputed everytime the map orientation changes. */
   set_topo_change_flag ( );

   if ( data_flag != 0 )
   {
       status = createPixelGrid ( ) ;

       if ( status != HydroStatus_OK )
       {
          fprintf ( stderr , "\nIn routine 'hv_zoom_call_back':\n"
                             "An error was encountered modifying the\n"
                             "pixel grid.  MPE data will not be displayed\n"
                             "properly.\n" ) ;
       }

   }

}

/*******************************************************************************
* MODULE NUMBER:
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
void set_highlight_flag ( )
{
   highlight_flag = 1 ;
}

/*******************************************************************************
* MODULE NUMBER:
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
void clear_highlight_flag ( )
{
   highlight_flag = 0 ;
}

/*******************************************************************************
* MODULE NUMBER:
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
int get_highlight_flag ( )
{
   return highlight_flag ;
}

/*******************************************************************************
* MODULE NUMBER:
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
static void clear_color_togglebutton_states ( Boolean color_button_states [ ] )
{
   int i ;

   for ( i = 0 ; i < MPEnumGageColorOptions ; ++ i )
   {
      color_button_states [ i ] = False ;
   }
}
/*******************************************************************************
* MODULE NUMBER:
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
static void clear_missing_togglebutton_states (
		                      Boolean missing_button_states [ ] )
{
   int i ;

   for ( i = 0 ; i < MPEnumGageMissingOptions ; ++ i )
   {
      missing_button_states [ i ] = False ;
   }
}

/*******************************************************************************
* MODULE NUMBER:
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
static void set_color_togglebutton_states ( Widget w )
{
   static Widget previous_selection = 0 ;

   XmToggleButtonSetState ( w , True , False ) ;

   if ( previous_selection != 0 )
   {
      XmToggleButtonSetState ( previous_selection , False , False ) ;
   }

   previous_selection = w ;
}

/*******************************************************************************
* MODULE NUMBER:
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
static void set_missing_togglebutton_states ( Widget w )
{
   static Widget previous_selection = 0 ;

   XmToggleButtonSetState ( w , True , False ) ;

   if ( previous_selection != 0 )
   {
      XmToggleButtonSetState ( previous_selection , False , False ) ;
   }

   previous_selection = w ;
}

/*******************************************************************************
* MODULE NUMBER:
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
void set_mpe_gage_color ( Widget w , XtPointer clientdata ,
                          XtPointer calldata )
{
   static Boolean color_button_states [ MPEnumGageColorOptions ] =
                   { False , False , False , False } ;
   static Boolean first = True ;

   /* The clientdata will indicate which toggle button generated
       this event. */
   enum MPEgageColorOptions gage_color_option =
                      ( enum MPEgageColorOptions ) clientdata ;

   switch ( gage_color_option )
   {
      case MPEgageColorSolid :
      case MPEgageColorContrast :
      case MPEgageColorQC :
      case MPEgageColorByValue :

         if ( color_button_states [ gage_color_option ] == False )
         {
            clear_color_togglebutton_states (  color_button_states ) ;
            color_button_states [ gage_color_option ] = True ;
            set_color_togglebutton_states ( w ) ;
            store_mpe_gage_label_color ( gage_color_option ) ;

            if ( first == False )
            {
               mUpdateMap ( 0 ) ;
            }
            else
            {
               first = False ;
            }
         }
         else
         {
            XmToggleButtonSetState ( w , True , False ) ;
         }

         break ;

      default :

         fprintf ( stderr , "\nIn routine 'set_mpe_gage_color':\n"
                            "Reached the default case in the switch\n"
                            "statement.  The value of gage_color_option\n"
                            "is %d.\n" , ( int ) gage_color_option ) ;
         break ;
   }

}

/*******************************************************************************
* MODULE NUMBER:
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
void set_mpe_gage_missing ( Widget w , XtPointer clientdata ,
                            XtPointer calldata )
{
   static Boolean missing_button_states [ MPEnumGageMissingOptions ] =
                   { False , False , False } ;
   static Boolean first = True ;

   /* The clientdata will indicate which toggle button generated
       this event. */
   enum MPEgageMissingOptions gage_missing_option =
                      ( enum MPEgageMissingOptions ) clientdata ;

   switch ( gage_missing_option )
   {
      case MPEgageMissingAll :
      case MPEgageMissingReported :
      case MPEgageMissingNone :

         if ( missing_button_states [ gage_missing_option ] == False )
         {
            clear_missing_togglebutton_states (missing_button_states ) ;
            missing_button_states [ gage_missing_option ] = True ;
            set_missing_togglebutton_states ( w ) ;
            store_mpe_gage_missing ( gage_missing_option ) ;

            if ( first == False )
            {
               mUpdateMap ( 0 ) ;
            }
            else
            {
               first = False ;
            }
         }
         else
         {
            XmToggleButtonSetState ( w , True , False ) ;
         }

         break ;

      default :

         fprintf ( stderr , "\nIn routine 'set_mpe_gage_missing':\n"
                            "Reached the default case in the switch\n"
                            "statement.  The value of gage_missing_option\n"
                            "is %d.\n" , ( int ) gage_missing_option ) ;
         break ;
   }

}

/*******************************************************************************
* MODULE NUMBER:
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
void clear_data_callback ( Widget w, XtPointer clientdata, XtPointer calldata )
{
   int dataflag;
   int running_flag;
   static Boolean reset_to_initial_date = True;

   /* Check if there is an active time lapse.  If so, then stop the time lapse. */
   running_flag = get_time_lapse_flag ( );

   if ( running_flag == 1)
   {
      end_time_lapse_RFCW ( w, & reset_to_initial_date, NULL );
   }

   /* Check if there are MPE data displayed.  If there are then undisplay
      them. */
   dataflag = isThereMpeData ( );

   if ( dataflag != 0 )
   {
      /* We do not want the Save data dialog to appear. */
      DataSaved = True;
      clear_data_RFCW ( w, clientdata, calldata );
   }

   /* Check if there are PDC data displayed.  If there are then undisplay
      them. */
   dataflag = getStationDrawingState ( );

   if ( dataflag != 0 )
   {
      pc_ClearCB ( w, clientdata, calldata );
   }

   /* Check if there are FFG data displayed.  If there are then undisplay
      them. */
   dataflag = isThereFfgDataToDraw ( );

   if ( dataflag != 0 )
   {
      ffg_clear_display ( w, clientdata, calldata );
   }
}

// -----------------------------------------------------------------------------------

void reread_shift_data_callback ( Widget w, XtPointer clientdata, XtPointer calldata )
{
    // This function reads in the pixel shift values for stations and causes
    // the map to be redraw, using these new shift values.

    Widget map_widget =  _get_map_widget ( 0 );
    loadShiftValues();

    if (pointcontrolDS != NULL) //this check prevents a crash when PDC hasn't been displayed
    {
        pc_drawMap(map_widget, True);
    }

    return;
}

// -----------------------------------------------------------------------------------

void edit_shift_data_callback ( Widget w, XtPointer clientdata, XtPointer calldata )
{

   char header[] = "edit_shift_data_callback(): ";
   static char editor_title[] = "LocationShiftEditor";
   static char file_path[BUFSIZ];
   static char  edit_script_dir[128];
   static char  edit_script[128];
   static char  hv_config_dir[BUFSIZ];
   char  command[LONG_LEN];
   char     gad_value[128];
   int      gad_token_len=0;
   int      gad_value_len=0;
   int      rcfad = 0;
   static int first = 1;

   /* retrieve the directory where the whfs editor script is located
      and the name of the whfs editor script to use. */

   if (first)
   {

      //editor script location
      gad_token_len = strlen("whfs_local_bin_dir" );
      rcfad = get_apps_defaults("whfs_local_bin_dir", &gad_token_len,
                gad_value, &gad_value_len);

      if (rcfad == 0 )
          sprintf(edit_script_dir, "%s", gad_value );
      else
          sprintf(edit_script_dir, "%s", ".");


      //editor script name
      gad_token_len = strlen("whfs_editor");
      rcfad = get_apps_defaults("whfs_editor", &gad_token_len ,
                gad_value, &gad_value_len);
      if (rcfad == 0 )
          sprintf(edit_script, "%s", gad_value);
      else
          sprintf(edit_script, "%s", "whfs_editor");


     // configuration directory
      gad_token_len = strlen("hv_config_dir");
      get_apps_defaults("hv_config_dir", &gad_token_len, hv_config_dir, &gad_value_len);
      if (strlen(hv_config_dir) <= 0)
      {
          strcpy(hv_config_dir, "/awips/hydroapps/whfs/local/data/app/hydroview");
          fprintf(stderr, "hv_config_dir undefined, using %s\n", hv_config_dir);
      }

      // path to configuration file
      sprintf(file_path, "%s/pdc_loc_shift.txt", hv_config_dir);

      first = 0;
   }


   /* editing product files */


   sprintf(command, "%s/%s %s %s &",
                        edit_script_dir,
                        edit_script,
                        editor_title,
                        file_path);

   printf("%s command = :%s:\n", header, command);



   system(command);

   return;

}
//-------------------------------------------------------------------------------------
void show_color_threshold_callback ( Widget w, XtPointer clientdata, XtPointer calldata )
{

 //   char header[] =   "show_color_threshold_callback(): ";

    char * user_id = getlogin();

    NamedColorSetGroup  * default_color_set_group = get_default_hv_colors();


    initialize_color_threshold_window( default_color_set_group,
                                                getApplicationName(),
                                                user_id,
                                                'E' );

    color_threshold_show(w);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
//-------------------------------------------------------------------------------------
