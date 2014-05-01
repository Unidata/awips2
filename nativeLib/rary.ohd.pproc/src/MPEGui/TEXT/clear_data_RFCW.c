/*******************************************************************************
* FILENAME:            clear_data_RFCW.c
* NUMBER OF MODULES:   2
* GENERAL INFORMATION:
*   MODULE 1:          clear_data_RFCW
* DESCRIPTION:         This routine turns off the display of the MPE data on
*                      the Hmap_mpe display.
*
*   MODULE 2:          clear_data_ok_RFCW
* DESCRIPTION:         Desensitizes Mpe menu options that should not be
*                      accessible when there is no Mpe data visible on
*                      the screen. 
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       February 26, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP Unix / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          2        2/26/2002    Bryon Lawrence    Original Coding
********************************************************************************
*/

#include <stdlib.h>
#include <Xm/Xm.h>

#include "clear_data_RFCW.h"
#include "create_ss_interface_rfcwide.h"
#include "display7x7_show.h"
#include "display_field_data_RFCW.h"
#include "display_mean_areal_precip.h"
#include "draw_precip_poly_RFCW.h"
#include "time_lapse_RFCW.h"
#include "map.h"
#include "map_library.h"
#include "newhour_RFCW.h"
#include "post_functions.h"
#include "read_precip_data.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "Xtools.h"

extern int single_radar_popup_flag ; /* Flag indicating if there is already
                                        a single radar popup pane being
                                        displayed.  This is to prevent more
                                        than one single radar from being 
                                        displayed at a time. */
extern int gage_ids_status ;  /* Flag indicating the status (on/off) of the 
                                 gage identifiers. */
extern int gage_values_status ; /* Flag indicating the status (on/off) of the
                                   gage values. */

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NA
* PURPOSE:       This routine allows the user of the new hmap_mpe application
*               to toggle "off" the display of the MPE data when he wishes
*                to work solely with Hydromap data.
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                         HEADER FILE         DESCRIPTION
*   clear_data_ok_RFCW           clear_data_RFCW.h
*   create_save_data_dialog_RFCW rfcwide_callbacks.h
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

void clear_data_RFCW ( Widget w , XtPointer clientdata , XtPointer calldata )
{
   Widget message ;
   enum DisplayFieldData display_field_type ;

   display_field_type = rad_data [ 0 ].field_type;

   /* Check to see if the user has saved the data about to be cleared.
      If the user has not yet saved it, then launch a dialog box
      prompting the user to save. */
      
   if ( DataSaved == FALSE )
   {
      switch ( display_field_type )
      {
         case display_rMosaic :
         case display_avgrMosaic :
         case display_maxrMosaic :
         case display_bMosaic :
         case display_lMosaic :
         case display_mMosaic :
         case display_mlMosaic :
         case display_gageOnly :
         case display_satPrecip :
	 case display_lsatPrecip :
         case display_sgMosaic :
         case display_srMosaic :
         case display_srgMosaic :
         case display_p3Mosaic :
         case display_rfcMosaic :
         case display_Xmrg :
	 
	 /* Create the "create save data dialog" box informing it that
            is should perform the functions necessary for clearing the
            MPE data from the hmap_mpe screen. */
	    
            message = create_save_data_dialog_RFCW(ClearData);
            XtManageChild(message);
            XtVaSetValues(message, XmNx, 40, XmNy, 40, NULL);
            return;

         default :

            break ;
       }      
   }

   clear_data_ok_RFCW ( w , clientdata , calldata ) ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   clear_data_ok_RFCW
* PURPOSE:       This routine desensitizes the "Next Hour",
*                "Previous Hour", "Save Hour", "Clear Data",
*                and "Draw Polygons" menu items on the Mpe Control
*                menu.  These options should not be accessible when
*                the Mpe data has been cleared off of the screen.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME         DESCRIPTION/UNITS
*   I      Widget      w            The widget the event was generated in.
*   I      XtPointer   clientdata   The data supplied by the caller of this
*                                   routine. 
*   I      XtPointer   calldata     The callback structure specific to this
*                                   type of event.
*
* RETURNS:
*   None 
*
* APIs UTILIZED:
*   NAME                          HEADER FILE               DESCRIPTION
*   DeSensitize                   Xtools.h                  Desensitizes the
*                                                           widget sent to it.
*   mUpdateMap                    map_library.h             Tells the map 
*                                                           library to update
*                                                           the data on the
*                                                           geographic viewer.
*   popdown_draw_precip_value_gui draw_precip_ploy_RFCW.h   "Pops Down" the
*                                                           Precip Value GUI
*                                                           if it exists.
*   turnOffMpeData                post_functions.h          Sets the Mpe Data
*                                                           flag to indicate
*                                                           to the Map Library
*                                                           that there is no
*                                                           Mpe Data to draw.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None.
*
* DATA FILES AND/OR DATABASE:
*   Not Applicable.
*
* ERROR HANDLING:
*   Not Applicable.
*
********************************************************************************
*/
void clear_data_ok_RFCW ( Widget w , XtPointer clientdata ,
                          XtPointer calldata )
{
   Boolean reset_to_initial_date = True ;
   int status;

   /* The draw polygon flag ... This needs to be turned "off"
      when the data field is cleared. */
   extern int display_7x7_flag ; 
   extern int display_gage_table ;   
   extern int draw_poly_flag ;

   /* Set the flag indicating to the "map library" exposure and drawing
      routines that MPE data is not to be drawn. */
   turnOffMpeData ( ) ;

   /* Check to make sure that the time lapse is "off".  If it is not "off",
      then turn it off before clearing the map data. */
   end_time_lapse_RFCW ( _get_map_widget ( 0 ) , 
                         ( XtPointer * ) & reset_to_initial_date ,
                         calldata ) ;

   /* Indicate that the data has been saved.  This will prevent the
      "Do you want to save the data" dialog box from being launched
      when you try to choose an hour of Mpe data. */
   DataSaved = TRUE ;

   /* Desensitize the "Save Data", "Clear Data", "Draw Polygons" and 
      "Single Radar Site" items on the "MpeControl" menu.  Also, 
      desensitize all of the menu items on the "Fields" menu. */
      
   if ( widget_struct->next_widget != NULL )
   {
      DeSensitize ( widget_struct->next_widget ) ;
      DeSensitize ( widget_struct->prev_widget ) ;
      DeSensitize ( savemaintop_widget ) ;
      DeSensitize ( savemainbottom_widget ) ;
      DeSensitize ( drawpoly_widget ) ;
      DeSensitize ( deletepoly_widget );
      DeSensitize ( widget_struct->radar_site_widget ) ;
      DeSensitize ( satprecip_widget ) ;
      DeSensitize ( lsatprecip_widget ) ;
      DeSensitize ( srmosaic_widget ) ;
      DeSensitize ( sgmosaic_widget ) ;
      DeSensitize ( srgmosaic_widget ) ;
   }

   if ( gage_ids_status == M_ON )
   {
      gage_ids_status = M_OFF;
	  XtVaSetValues ( showids_widget , XmNset , False , NULL ) ;
      turnOffMpeGageIds ( ) ;
   }

   if ( gage_values_status == M_ON )
   {
       gage_values_status = M_OFF ;
       XtVaSetValues ( showval_widget , XmNset , False , NULL ) ;
       turnOffMpeGageValues ( ) ;
   }

   if ( widget_struct->display_bias_widget != NULL )
   {
      /* Desensitize items on the MPEcontrol menu. */
      DeSensitize ( widget_struct->next_widget ) ;
      DeSensitize ( widget_struct->prev_widget ) ;
      DeSensitize ( savemaintop_widget ) ;
      DeSensitize ( savemainbottom_widget ) ;
      DeSensitize ( widget_struct->rerun_widget ) ;
      DeSensitize ( widget_struct->transmit_rfc_qpe );
      DeSensitize ( widget_struct->transmit_rfc_bias );

      /* Desensitize items on the Tools menu. */
      DeSensitize ( fullscreen_widget );
      DeSensitize ( splitscreen_widget );

      /* DeSensitize items on the Polygons menu. */
      DeSensitize ( drawpoly_widget ) ;
      DeSensitize ( deletepoly_widget ) ;

      /* Desensitize the "Precip Fields" menu items.  These are only applicable
       *          after the user has chosen a hour to view MPE data for. */
      DeSensitize ( rmosaic_widget ) ;
      DeSensitize ( avgrmosaic_widget );
      DeSensitize ( maxrmosaic_widget );
      DeSensitize ( bmosaic_widget ) ;
      DeSensitize ( bias_widget ) ;
      DeSensitize ( mmosaic_widget ) ;
      DeSensitize ( mlmosaic_widget ) ;
      DeSensitize ( p3lmosaic_widget ) ;
      DeSensitize ( gageonly_widget ) ;
      DeSensitize ( xmrg_widget ) ;
      DeSensitize ( multihour_widget ) ;
      DeSensitize ( satprecip_widget ) ;
      DeSensitize ( lsatprecip_widget ) ;
      DeSensitize ( srmosaic_widget ) ;
      DeSensitize ( sgmosaic_widget ) ;
      DeSensitize ( srgmosaic_widget ) ;
      DeSensitize ( rfcbmosaic_widget );
      DeSensitize ( rfcmmosaic_widget );
      DeSensitize ( widget_struct->rfc_qpe_mosaic );

      /* Desensitize items under the Base Fields menu. */
       
      DeSensitize ( locspan_widget ) ;
      DeSensitize ( locbias_widget );
      DeSensitize ( height_widget ) ;
      DeSensitize ( index_widget ) ;
      DeSensitize ( widget_struct->gage_triangles );

      /* Desensitize the items on the Gage menu. */
      DeSensitize ( widget_struct->save_level2_data );
      DeSensitize ( widget_struct->pseudo_widget );
      DeSensitize ( widget_struct->gage_table_widget );
      DeSensitize ( widget_struct->single_gage_widget ) ;
      DeSensitize ( showids_widget );
      DeSensitize ( showval_widget );
      DeSensitize ( widget_struct->gage_missing_menu );
      DeSensitize ( widget_struct->gage_color_menu );

      /* Desensitize the Gages menu items. */
      DeSensitize ( widget_struct->pseudo_widget );
      DeSensitize ( widget_struct->gage_color_menu );
      DeSensitize ( widget_struct->gage_missing_menu );
      DeSensitize ( widget_struct->gage_table_widget );

      /* Desensitize the items on the Misc menu. */
      DeSensitize ( widget_struct->display_bias_widget ) ;
      DeSensitize ( widget_struct->radar_site_widget ) ;
      DeSensitize ( timelapse_widget ) ;
      DeSensitize ( widget_struct->timelapse6_widget ) ;
      DeSensitize ( widget_struct->timelapse12_widget ) ;
      DeSensitize ( widget_struct->timelapse24_widget ) ;
      DeSensitize ( widget_struct->timelapseother_widget ) ;
      DeSensitize ( widget_struct->stoptime_widget ) ;
      DeSensitize ( multihour_widget );
   }
   

   /* Clear any SingleRadarSite popup windows that are currently being
      displayed. Also, free any system resources that they are using. */
   popdown_all_windows_single_site ( NULL , NULL , NULL ) ; 
    
   /* Make certain that the polygon data substitution gui is 
      "off". */
   popdown_draw_precip_value_gui ( ) ;

   /* Turn off the ability to draw a "polygon". */
   draw_poly_flag = 0 ;
  
   /* Turn off the Mpe data flag.  This will tell the map library
      exposure routines not to draw Mpe data. */
   turnOffMpeData ( ) ;

   /* Turn the legend off.  Note that this must be done before the call
      to the "mUpdateMap" routine to ensure that the map is redrawn 
      properly. */
   _turn_legend_off ( ) ;
   
   /* Turn off the Mpe Gage Ids flag.  This will tell the map library
      exposure routines not to draw Mpe Gage Ids data. */
   turnOffMpeGageIds ( ) ;
   
   /* Turn off the Mpe Gage Values flag.  This will tell the map library
      exposure routines not to draw Mpe Gage Values data. */
      
   turnOffMpeGageValues ( ) ;

   /* Turn off the gage triangles overlay.  This does not need
      to be shown when MPE data are not being displayed. */
   status = isThereMpeGageTriangles ( );

   if ( status == 1 )
   {
      turnOffMpeGageTriangles ( );
   }
   
   /* Free the memory used by the pixel grid. */
   freePixelGrid ( ) ;

   free_mean_areal_precip ( ) ; 
   free_poly_temp ( );

   /* Free the memory used by the gage table. */
   if ( gage != NULL )
   {
      free ( gage ) ;
      gage = NULL ;
      ngages = 0 ;
   }

   if ( display_7x7_flag == 1 )
   {
      /* End the display 7x7 feature when switching hours. */
      close7x7Callback ( NULL , NULL , NULL ) ;
   }

   if ( display_gage_table == 1 )
   {
      /* Popdown the Gage Table when switching hours. */
      popdown_gagetable ( NULL , NULL , NULL ) ;
   }

   /* Check to see if in split screen mode.  If so, then switch back
      to full screen mode. */
   status = is_screen_split ( );

   if ( status == 1 )
   {
      full_screen_callback ( NULL, NULL, NULL );
   }
   else
   {
      /* Inform the "map library" that the map viewing area needs to be 
         redrawn. */
      mUpdateMap ( 0 ) ;
   }

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc_lib/src/MPEGui/RCS/clear_data_RFCW.c,v $";
 static char rcs_id2[] = "$Id: clear_data_RFCW.c,v 1.15 2007/05/24 12:59:37 whfs Exp $";}
/*  ===================================================  */

}
