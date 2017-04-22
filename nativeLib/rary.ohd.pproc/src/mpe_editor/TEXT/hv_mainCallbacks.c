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
#include <Xm/Xm.h>

#include "display_accum_show.h"
#include "display_field_data_RFCW.h"
#include "display_mean_areal_precip.h"
#include "drawa.h"
#include "drawMpeLegend.h"
#include "GenericLegendDialog.h"
#include "GeneralUtil.h"
#include "get_colorvalues.h"
#include "get_mpe_colors.h"
#include "HvCallbacks.h"
#include "hv_mainCallbacks.h"
#include "map.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_menubar_cb.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "NamedColorSetGroup.h"
#include "plot_routines.h"
#include "pointcontrol_mgr.h"
#include "PointDisplayControl.h"
#include "post_functions.h"
#include "rfcwide_callbacks.h"
#include "stage3_interface.h"
#include "time_lapse_RFCW.h"
#include "tsgen_info.h"
#include "choose_rfcwide_date.h"
#include "gageqc_gui.h"

static int highlight_flag = 0;
static int choose_hour_popup_check = 0;
extern void drawMpeGageTriangles();
extern void init_dqc_local_date_and_area_values();
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
extern int maxmin_on;
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/
extern int flf_on;
extern int maxmin_on;
extern int qpf_on;

void redrawBase ( int * map_index )
{
   int split_screen;

   /* Clear the map. */
   _clear_map ( * map_index );

   /* Draw the topography data.  If MPE data are displayed, the
      topography field should share the same pixmap. */
   _draw_topography_overlay ( * map_index );

   split_screen = is_screen_split ( );

   /* Draw the Mpe radar/precipitation data (if it exists). */
   if ( * map_index == 1 )
   {

      /* Only draw the MPE data on the bottom window if there are no
         DailyQC data. */
      if ( ( flf_on == -1 ) && ( qpf_on == -1 ) && ( maxmin_on == -1 ) )
      {
         drawMpeRadarData ( * map_index ) ;
      }
   }
   else
   {
      drawMpeRadarData ( * map_index ) ;
   }

  /*Plotting precip, temperature and freezing level QC stations...*/
  /* Do not plot gridded or areal GageQC data on the lower window.
     This window is for point GageQC data and MPE data only! */


   if ( split_screen == 1 )
   {
      /* Only draw the gageqc data on the bottom screen */

      if ( * map_index == 1 )
      {
         _plotGageQCData(* map_index);
      }

   }
   else
   {
      _plotGageQCData(* map_index);
   }

  /* Draw the map overlays. */
   _draw_map_overlays ( * map_index ) ;

   /* draw map with Mpe gage identificators */
   drawMpeGageIds ( * map_index ) ;

   /* draw map with Mpe gage values */
   drawMpeGageValues ( * map_index ) ;

   drawMpeGageTriangles();



   if(choose_hour_popup_check == 0)
   {
	choose_hour_manage(_get_map_shell());
	choose_hour_popup_check = 1;
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
          flogMessage ( stderr , "\nIn routine 'hv_zoom_call_back':\n"
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
          flogMessage ( stderr , "\nIn routine 'hv_zoom_call_back':\n"
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
          flogMessage ( stderr , "\nIn routine 'hv_zoom_call_back':\n"
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

         flogMessage ( stderr , "\nIn routine 'set_mpe_gage_color':\n"
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

         flogMessage ( stderr , "\nIn routine 'set_mpe_gage_missing':\n"
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
void display_image_callback ( Widget w, XtPointer client_data, XtPointer call_data )
{
   Boolean state;
   enum MpeImageDisplay display = ( enum MpeImageDisplay ) client_data;

   /* Check the state of the toggle button. */
   state = XmToggleButtonGetState ( w );

   if ( display == DISPLAY_AS_IMAGE )
   {
      setToPlotImage ( state );
   }
   else if ( display == DISPLAY_AS_CONTOUR )
   {
      setToPlotContour ( state );
   }
   else
   {
     logMessage ( "Unrecognized display option.  Current options are\n"
               "Image or Contour.\n" );
   }

   send_expose ( );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc/src/mpe_editor/RCS/hv_mainCallbacks.c,v $";
 static char rcs_id2[] = "$Id: hv_mainCallbacks.c,v 1.31 2007/10/18 17:41:57 lawrence Exp $";}
/*  ===================================================  */

}
