
/*******************************************************************************
* FILENAME:            display_best_qpe_show.c
* GENERAL INFORMATION: Contains the routines for creating the Display Best
*                      Estimate QPE GUI.  Also contains the callback routines.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 23, 2005
* ORGANIZATION:        OHD, HSEB, WHFS
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <string.h>
#include <Xm/AtomMgr.h>
#include <Xm/Scale.h>
#include <Xm/Xm.h>
#include <Xm/ComboBox.h>
#include <Xm/Protocols.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include <Xm/ToggleB.h>

#include "clear_data_RFCW.h"
#include "display_accum_show.h"
#include "display_best_qpe.h"
#include "display_best_qpe_show.h"
#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_mean_areal_precip.h"
#include "drawa.h"
#include "find_dates.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_field_names.h"
#include "rfcwide_callbacks.h"
#include "stage3.h"
#include "time_lapse_RFCW.h"
#include "Xtools.h"

static Boolean accumulate_flag = True;
static Boolean time_lapse_flag = False;
static date_struct base_time;
static enum PrecipAccumArea selected_area = PrecipGrid;
static enum BestQPESource qpe_source = QPELocalSource;
static int duration_value = 1;
static time_t end_time_value = 0;

const static int num_seconds_in_hour = 60 * 60;
const static int num_seconds_in_day = 24 * 60 * 60;

extern time_lapse_struct tldata;

/*******************************************************************************
* MODULE NUMBER: 5
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

static void accumulate_data ( )
                             
{
   enum DisplayFieldData field_to_display;
   int i ;
   int index = 0 ;
   int j ;

   if ( qpe_source == QPELocalSource )
   {
      field_to_display = display_Xmrg; 
   }
   else
   {
      field_to_display = display_rfcMosaic;
   }

   /*--------------------------------------------------------------*/
   /*   Display watch cursor and flush the display buffer.         */
   /*--------------------------------------------------------------*/
   mSetCursor ( M_WATCH ) ;
   XFlush(XtDisplay(rad_data[0].w));
 
   for ( i = 0 ; i < NUMHRS ; ++i )
   {
      if ( strcmp ( base_time.cdate , dates [ i ].cdate ) == 0 ) break ;
      index ++ ;
   }

   if ( index == NUMHRS )
   {
      fprintf ( stderr , "\nIn routine \"accum_show_data\":\n"
                         "Could not find date %s in the array of valid\n"
                         "datetimes.  Aborting the attempt to display\n"
                         "the precipitation accumulations.\n" , 
                         base_time.cdate ) ;
      return ;
   }

   /* Zero out the grid. */
   for ( i = 0 ; i < rad_data[0].maximum_columns ; ++ i )
   {
      for ( j = 0 ; j < rad_data[0].maximum_rows ; ++ j )
      {
         rad_data[0].data_array [ i ] [ j ] = 0 ; 
      }
   }

   /* Check to see if any of the hours being used for the multi-hour QPE
      are outside of the bounds of the dates array. */
   if ( ( index + duration_value ) > NUMHRS )
   {
      fprintf ( stderr , "\nIn routine \"accum_show_data\":\n"
                         "Unable to completely process an interval of %d\n"
                         "hours because part of it goes back more than\n"
                         "%d hours from the current date and time, the\n"
                         "amount of MPE data which is saved.  There will only\n"
                         "be %d %s of data in the multi-hour precipitation\n"
                         "estimate.\n" , duration_value , NUMHRS ,
                         NUMHRS - index ,
                         ( NUMHRS - index > 1 ) ? "hours" : "hour" ) ;
   }

   set_multi_hour_interval_val ( duration_value );

   if ( duration_value == 1 )
   {
      display_field_data_RFCW ( field_to_display , rad_data[0].data_array,
                                dates [ index ] , 0 ) ;
   }
   else
   {
      /* Retrieve each of the Best QPE HRAP grids.  Accumulate the rainfall.
         Make certain that only times within the "dates" array are used. */
      for ( i = index ; ( i < ( index + duration_value ) ) && 
                        ( i < NUMHRS ) ; ++ i )
      {
         display_field_data_RFCW ( field_to_display , rad_data[0].data_array ,
                                   dates [ i ] , 1 ) ;
      }
   }

   /* Set the color use name based on the product being displayed. */
   if ( field_to_display == display_Xmrg )
   {
      strcpy ( cv_use, "MULTIHOUR" );
      strcpy (rad_data[0].cv_use, "MULTIHOUR" );
   }
   else
   {
      strcpy ( cv_use, "RFCQPE" );
      strcpy ( rad_data[0].cv_use, "RFCMOSAIC" );
   }

   /* If the type of this precipitation accumulation is anything other
      than "GRID", then areal averaging must be performed on the
      precipitation values. */
   if ( selected_area != PrecipGrid )
   {

      display_mean_areal_precip ( rad_data[0].data_array , XOR , YOR ,
                                  rad_data[0].maximum_columns , 
                                  rad_data[0].maximum_rows ,
                                  AccumAreaTypes [ selected_area ] ) ;                                   
   }

   display_field ( "" , 0, 0 ) ;

   /* Force the legend to be updated. */
   mUpdateLegend ( 0 ) ;
}


static void best_qpe_closeCB ( Widget w, XtPointer clientdata,
                               XtPointer calldata )
{
   XtUnmanageChild ( bestqpeDS );
   return;
}


static void best_qpe_showCB ( Widget w, XtPointer clientdata, 
                              XtPointer calldata )
{
   extern int cv_duration ;
   int running_flag;

   /* Retrieve the value of the duration. */
   XmScaleGetValue ( durationSCL, &duration_value );
   
   running_flag = get_time_lapse_flag ( );
   
   if ( running_flag == 1 ) return;
   
   /* Retrieve the time. This is in the base_time structure. */
   /* Basic Algorithm Documentation. */
   /* Prep the data fields. */
   /* If anything other than GRID is being displayed, than 
      load information from the GeoData table and the LineSegs table. */
   if ( time_lapse_flag == True )
   {
      /* Check the QPE source ... is it the local QPE or the RFC
         QPE mosaic? */
      if ( qpe_source == QPELocalSource )
      {

         /* Set the coloruse scheme. */
         strcpy ( cv_use, "MULTIHOUR" );

         /* Set the radar data array so that it contains only the 
            Best Estimate QPE. */
         rad_data [ 0 ].field_type = display_Xmrg;
      }
      else
      {
         /* Set the coloruse scheme. */
         strcpy ( cv_use, "RFCMOSAIC" );

         /* Set the radar array so that it contains only the
            RFC QPE Mosaic. */
         rad_data [ 0 ].field_type = display_rfcMosaic;
      }

      /* Set the duration. */
      cv_duration = 3600;
      rad_data[0].cv_duration = cv_duration;
     
      /* Set the time lapse duration. */
      tldata.nhrs = duration_value;
      

      /* Time lapse the data. */
      time_lapse_RFCW (w, &tldata, NULL );
            
   }
   else
   {
      /* New on November 9, 2004 ... Added the ability to load different
         color schemes for the multihour precipitation product based on
         the accumulation interval. */
      cv_duration = duration_value * SECONDS_PER_HOUR ;
      rad_data[0].cv_duration = cv_duration;

      /* Retrieve the colors associated with the precipitation levels. */
      strcpy ( cv_use , "MULTIHOUR" ) ;
      rad_data [ 0 ].field_type = display_multiHour;

      /* Accumulate the hourly QPE. */
      accumulate_data ( );
   }

}

static void restart_timelapseCB ( Widget w, 
                                  XtPointer clientdata,
                                  XtPointer calldata )
{
   int running_flag;

   running_flag = get_time_lapse_flag ( );

   if ( running_flag == 0 )
   {
      /* Check to see if the timelapse toggle button is "On". */
      if ( time_lapse_flag == True )
      {
         /* Start the time lapse for the first time. */
         best_qpe_showCB ( w, clientdata, calldata ); 
      } 
   }
   else
   {
      end_manual_loop_callback ( w, clientdata, calldata );
   }
}
                                  
static void change_qpe_endtimeCB ( Widget w, 
                                   XtPointer clientdata,
                                   XtPointer calldata )
{
   extern int dates_struct_count;
   char base_time_string [ 14 ] ;
   date_struct temp_base_time ;
   Display * display = NULL ;
   int i ;
   int save_date_index;
   struct tm end_tm_struct;
   struct tm * pStructTm = NULL ;
   enum QPEEndTimeOper interval_end_time_oper ;
   time_t save_time;

   interval_end_time_oper =  ( enum QPEEndTimeOper ) clientdata ;

   save_time = end_time_value ;
   save_date_index = get_date_array_index ( );

   /* Test for the "other" case. */
   switch ( interval_end_time_oper )
   {
      case QPEDayIncrement :

         end_time_value += num_seconds_in_day ;
         break ;

      case QPEDayDecrement :

         end_time_value -= num_seconds_in_day ;
         break ;

      case QPEHourIncrement :

         end_time_value += num_seconds_in_hour ;
         break ;

      case QPEHourDecrement :

         end_time_value -= num_seconds_in_hour ;
         break ;

      case QPENoOperation :

         break ;

      default :

         fprintf ( stderr , "\nIn routine \"accum_change_endtime\":\n"
                            "Unrecognized enum PrecipEndTimeOper value.\n"
                            "The end time value of the accumulation interval\n"
                            "is not being modified.\n" ) ;
   }

   if ( end_time_value == 0 )
   {
      /* Initialize the the date_st3 structure with the most current 
         time in the dates array. */
      date_st3 = dates [ 0 ];
      end_tm_struct.tm_year = date_st3.year - 1900 ;
      end_tm_struct.tm_mon = date_st3.month - 1 ;
      end_tm_struct.tm_mday = date_st3.day ;
      end_tm_struct.tm_hour = date_st3.hour ;
      end_tm_struct.tm_min = 0 ;
      end_tm_struct.tm_sec = 0 ;
      end_time_value = mktime ( & end_tm_struct ) ;
   }

   pStructTm = localtime ( & end_time_value ) ;

   temp_base_time.year = pStructTm->tm_year + 1900 ;
   temp_base_time.month = pStructTm->tm_mon + 1 ;
   temp_base_time.day = pStructTm->tm_mday ;
   temp_base_time.hour = pStructTm->tm_hour ;

   /* Create the base date time string. */
   memset ( temp_base_time.cdate , '\0' , 11 ) ;
   strftime ( temp_base_time.cdate , 11 , "%Y%m%d%Hz" , pStructTm ) ;

   /* Test the base time to make sure it is in the "dates" array. */
   for ( i = 0 ; i < NUMHRS ; ++ i )
   {
      if ( strcmp ( temp_base_time.cdate , dates [ i ].cdate ) == 0 )
      {
         date_st3 = dates [ i ];
         break;
      }
   }

   if ( i == NUMHRS )
   {
      end_time_value = save_time ;
      set_date_array_index ( save_date_index );
      dates_struct_count = save_date_index;

      /* Ring the bell. */
     display = XtDisplay ( w ); 
     if ( display != NULL )
     {
        XBell ( display , 30 ) ;
     }
   }
   else
   {
      set_date_array_index ( i );
      dates_struct_count = i;
      base_time = temp_base_time ;

      /* Create the base date time string formatted for the legend. */
      strftime ( base_time.lldate , 20 , "%b %d %Y %Hz" , pStructTm ) ;
      set_base_time ( & base_time );

      /* Display the base time value in the text field on GUI. */
      memset ( base_time_string , '\0' , 14 ) ;
      sprintf ( base_time_string , "%04d-%02d-%02d %02d" ,
                base_time.year , base_time.month , base_time.day ,
                base_time.hour ) ;

      XtVaSetValues ( qpetimeTX , XmNvalue , base_time_string , NULL ) ;
  }
   
}

static void best_qpe_clearCB ( Widget w, XtPointer clientdata,
                               XtPointer calldata )
{
	static Boolean reset_to_initial_date = True;
	int running_flag;
	
   /* Test if there is a time lapse running.  If there is then stop it
      before clearing the data. */
   running_flag = get_time_lapse_flag ( );
   
   if ( running_flag == 1 )
   {
   	  end_time_lapse_RFCW ( w, & reset_to_initial_date, NULL );
   }
   
   /* Call the old "clear data" callback for mpe. */
   /* Do not show the Saved Data dialog window. */
   DataSaved = True;
   clear_data_RFCW ( w, clientdata, calldata ); 
   
   
   
}

static void set_accumulateCB ( Widget w, XtPointer clientdata,
                               XtPointer calldata )
{
   /* Get the state of the toggle button. */
   XmToggleButtonCallbackStruct * pToggleStruct = 
              ( XmToggleButtonCallbackStruct * ) calldata; 

   accumulate_flag = pToggleStruct->set;

   /* Reset the scale to have the maximum value specified
      for the accumulation mode. */ 
   XtVaSetValues ( durationSCL, XmNmaximum, MAX_ACCUMULATION_INTERVAL, NULL );

   /* Make sure that the ids and labels toggle buttons are sensitive
      as well as the area selection combo box.  These features are
      used in accumulation mode. */
   XtSetSensitive ( idsTB, True );
   XtSetSensitive ( labelsTB, True );
   XtSetSensitive ( displayCBX, True );
}

static void set_time_lapseCB ( Widget w, XtPointer clientdata,
                               XtPointer calldata )
{
   /* Get the state of the toggle button. */
   XmToggleButtonCallbackStruct * pToggleStruct = 
              ( XmToggleButtonCallbackStruct * ) calldata; 
 
   time_lapse_flag = pToggleStruct->set;

   /* Reset the Scale Widget to have the maximum value specified
      for the time lapse mode. */
   XtVaSetValues ( durationSCL, XmNmaximum, MAX_TIMELAPSE_DURATION, NULL );

   /* Desensitize the ids and labels toggle buttons as well as the
      the area type combo box. These features aren't available in 
      time lapse mode. */
   XtSetSensitive ( idsTB, False );
   XtSetSensitive ( labelsTB, False );
   XtSetSensitive ( displayCBX, False );
    
}

static void set_idsCB ( Widget w, XtPointer clientdata, XtPointer calldata )
{
   /* Get the state of the ids toggle button. */
   XmToggleButtonCallbackStruct * pToggleStruct =
              ( XmToggleButtonCallbackStruct * ) calldata;
   
   if ( pToggleStruct->set == True )
   {
      turnOnMultiHourIds ( );
   }
   else
   {
      turnOffMultiHourIds ( );
   }
}

static void set_labelsCB ( Widget w, XtPointer clientdata, XtPointer calldata )
{
   /* Get the state of the labels toggle button. */
   XmToggleButtonCallbackStruct * pToggleStruct =
              ( XmToggleButtonCallbackStruct * ) calldata;

   if ( pToggleStruct->set == True )
   {
      turnOnMultiHourValues ( );
   }
   else
   {
      turnOffMultiHourValues ( );
   }
}

static void display_cbxCB ( Widget w, XtPointer clientdata, 
                            XtPointer calldata )
{
   XmComboBoxCallbackStruct * cbs_struct = ( XmComboBoxCallbackStruct * )
	                                   calldata; 

   /* Determine the currently selected item in the scrolled combo box. */
   int cbsPosition = cbs_struct->item_position;

   selected_area = ( enum PrecipAccumArea ) cbsPosition;
   
   if ( selected_area == QPEGrid )
   {   	
   	   free_mean_areal_precip ( );
   }
}

static void best_qpe_source_callback ( Widget w, 
                                       XtPointer call_data,
                                       XtPointer client_data )
{
   Boolean state;

   state = XmToggleButtonGetState ( localTB );

   if ( state == True )
   {
      qpe_source = QPELocalSource; 
   }
   else
   {
      qpe_source = QPERFCSource;
   }
}

static void add_best_qpe_callbacks ( )
{

   Atom wmAtom;

   /* Indicates to the routine which ends the time lapse
      to reset the data display to hour before the time lapse was
      started. */
   static Boolean reset_to_initial_date = True;

   wmAtom = XmInternAtom(XtDisplay(bestqpeDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(bestqpeDS, wmAtom, best_qpe_closeCB, NULL);

   XtAddCallback ( closeqpePB, XmNactivateCallback, best_qpe_closeCB, NULL );
   XtAddCallback ( showdataPB, XmNactivateCallback, best_qpe_showCB, NULL );
   XtAddCallback ( cleardataPB, XmNactivateCallback, best_qpe_clearCB, NULL );


   XtAddCallback ( dayupAB, XmNactivateCallback, change_qpe_endtimeCB,
                   (XtPointer) QPEDayIncrement );
   XtAddCallback ( daydownAB, XmNactivateCallback, change_qpe_endtimeCB,
                   (XtPointer) QPEDayDecrement );
   XtAddCallback ( hourupAB, XmNactivateCallback, change_qpe_endtimeCB,
                   (XtPointer) QPEHourIncrement );
   XtAddCallback ( hourdownAB, XmNactivateCallback, change_qpe_endtimeCB,
                   (XtPointer) QPEHourDecrement );
   XtAddCallback ( accumTB, XmNvalueChangedCallback, set_accumulateCB, NULL );

   XtAddCallback ( lapseTB, XmNvalueChangedCallback, set_time_lapseCB, NULL );
   XtAddCallback ( idsTB, XmNvalueChangedCallback, set_idsCB, NULL ); 
   XtAddCallback ( labelsTB, XmNvalueChangedCallback, set_labelsCB, NULL );
   XtAddCallback ( displayCBX, XmNselectionCallback, display_cbxCB, NULL ); 
   XtAddCallback ( endlapsePB, XmNactivateCallback, end_time_lapse_RFCW,
                   (XtPointer) & reset_to_initial_date );
   XtAddCallback ( lastframePB, XmNactivateCallback, hydroview_manual_loopCB, 
                   (XtPointer) LOOP_BACKWARD );
   XtAddCallback ( firstframePB, XmNactivateCallback, hydroview_manual_loopCB, 
                   (XtPointer) LOOP_FORWARD );
   XtAddCallback ( stepbackPB, XmNactivateCallback, loop_step_callback,
		   (XtPointer) LOOP_BACKWARD ); 
   XtAddCallback ( stepforwardPB, XmNactivateCallback, loop_step_callback,
		   (XtPointer) LOOP_FORWARD );
   XtAddCallback ( restartPB, XmNactivateCallback, restart_timelapseCB, 
                   NULL );
   XtAddCallback ( localTB, XmNvalueChangedCallback, best_qpe_source_callback,
                   NULL );
   XtAddCallback ( rfcTB, XmNvalueChangedCallback, best_qpe_source_callback,
                   NULL );
                   
}

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   show_bestQpeDS
* PURPOSE:       Displays the Display Best Estimate QPE window.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget      w                    The parent widget.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME              HEADER FILE          DESCRIPTION
*   create_bestqpeDS  display_best_qpe.h
*   GetTopShell
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   Widget     top_shell                    The widget of the poopup shell.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   No error handling.
*
********************************************************************************
*/
void show_bestQpeDS ( Widget w )
{
   static char * hv_config_token = "hv_config_dir";
   static char config_directory [ 128 ];
   static char pixmap_file_path [ 128 ];
   Boolean toggle_status;
   int len_reply;
   int len;
   Pixmap pix;
   Pixel  pixel;

   Widget top_shell = NULL;
   
   /* Retrieve the lastest array of valid dates for MPE to use. */
   get_dates_RFCW ( );

   /* Check if the Display QPE gui has been created and is managed. */
   if ( bestqpeDS == NULL )
   {
      /* Create the popup shell. */
      top_shell = GetTopShell ( w );
      create_bestqpeDS ( top_shell );

      /* Create the callbacks. */
      add_best_qpe_callbacks ( );

      /* Get the location of the bitmap file for the restart
         timelapse button. */
      len = strlen ( hv_config_token );
      get_apps_defaults ( hv_config_token, &len, config_directory,
                          & len_reply );

      if ( len_reply > 0 )
      {

        XtVaGetValues ( restartPB,
                        XmNbackground,
                        & pixel,
                        NULL );

         sprintf ( pixmap_file_path, "%s/restart_timelapse.xbm", 
                   config_directory );
         pix = XmGetPixmap ( XtScreen ( w ),
                             pixmap_file_path,
                             _get_color("Black"),
                             pixel );

         XtVaSetValues ( restartPB,
                         XmNlabelPixmap,
                         pix,
                         NULL );
      }
   }

   /* Initialize the time field. */
   change_qpe_endtimeCB ( w, ( XtPointer ) QPENoOperation, NULL );

   XtManageChild ( bestqpeDS );
   XtManageChild ( bestqpeFO );
   
   /* Check the states of the toggle buttons. */
   time_lapse_flag = XmToggleButtonGetState ( lapseTB );

   if ( time_lapse_flag )
   {
      XtVaSetValues ( durationSCL, XmNmaximum, MAX_TIMELAPSE_DURATION, NULL );
   }

   accumulate_flag = XmToggleButtonGetState ( accumTB );

   if ( accumulate_flag )
   {
      XtVaSetValues ( durationSCL, XmNmaximum, MAX_ACCUMULATION_INTERVAL, 
                      NULL );
   }

   toggle_status = XmToggleButtonGetState ( idsTB );

   if ( toggle_status )
   {
      turnOnMultiHourIds ( );
   }
   else
   {
      turnOffMultiHourIds ( );
   }
   
   toggle_status = XmToggleButtonGetState ( labelsTB );

   if ( toggle_status )
   {
      turnOnMultiHourValues ( );
   }
   else
   {
      turnOffMultiHourValues ( );
   }

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
