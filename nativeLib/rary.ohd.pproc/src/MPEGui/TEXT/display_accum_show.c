/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:   accum_close_display
* DESCRIPTION:
*   MODULE 2:   accum_select_interval
* DESCRIPTION:
*   MODULE 3:   accum_change_endtime
* DESCRIPTION:
*   MODULE 4:   accum_select_area
* DESCRIPTION:
*   MODULE 5:   accum_show_data
* DESCRIPTION:
*
* ORIGINAL AUTHOR:   Bryon Lawrence
* CREATION DATE:     November 2004
* ORGANIZATION:      HSEB/OHD
* OPERATING SYSTEM:  Redhat Linux           
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          5        11/9/2004    Bryon Lawrence    Modified to allow 
*                                                  color schemes to be
*                                                  specified for
*                                                  multiple durations. 
********************************************************************************
*/

#include <time.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleB.h>
#include <Xm/Xm.h>

#include "DbmsDefs.h"
#include "display_accum_show.h"
#include "display_control_endtime.h"
#include "display_control_interval.h"
#include "display_control_precip.h"
#include "display_endtime_show.h"
#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_interval_show.h"
#include "display_mean_areal_precip.h"
#include "drawa.h"
#include "GeneralUtil.h"
#include "GeoArea.h"
#include "help.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "post_functions.h"
#include "rfcwide_interface.h"
#include "stage3.h" /* For "date_struct" definition. */
#include "time_convert.h" /* For gm_mktime prototype. */
#include "time_defs.h" /* For the MINUTES_PER_HOUR constant. */
#include "Xtools.h"

char * AccumAreaTypes [ NumPrecipAreaItems ] = { "GRID" , "BASIN" , "COUNTY" , "ZONE" } ;

static date_struct base_time ;

static enum PrecipAccumInterval accum_interval = Accum24 ;
static enum PrecipAccumArea area = PrecipGrid ;

static time_t end_time_value = 0 ;
static int interval_value = ( int ) Accum6 ;
static int area_value = ( int ) PrecipGrid ;

const int num_seconds_in_day = 24 * 60 * 60 ;
const int num_seconds_in_hour = 60 * 60 ;

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
static void accum_close_display ( Widget w , XtPointer clientdata ,
                                  XtPointer calldata )
{
   if ( accum_displayControlDS != NULL ) 
   {
      XtPopdown ( accum_displayControlDS ) ;
      XtUnmanageChild ( accum_displayControlDS ) ;
   }

   if ( accum_endtimeControlDS != NULL )
   {
      XtDestroyWidget ( accum_endtimeControlDS ) ;
      accum_endtimeControlDS = NULL ;
   }

   if ( accum_intervalControlDS != NULL )
   {
      XtDestroyWidget ( accum_intervalControlDS ) ;
      accum_intervalControlDS = NULL ;
   }

   return ;
}

/*******************************************************************************
* MODULE NUMBER: 2
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
static void accum_select_interval ( Widget w , XtPointer clientdata ,
                                    XtPointer calldata )
{
   accum_interval = ( enum PrecipAccumInterval ) clientdata ;

   /* Test for the "other" case. */
   if ( accum_interval == AccumOther )
   {
      accum_interval_show ( w ) ;   
   }
   else
   {
      interval_value = ( int ) accum_interval ;
   }
}

/*******************************************************************************
* MODULE NUMBER: 3
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

static void accum_change_endtime ( Widget w , XtPointer clientdata ,
                                   XtPointer calldata )
{
   char base_time_string [ 14 ] ;
   date_struct temp_base_time ;
   Display * display = NULL ;
   int i ;
   struct tm end_tm_struct ;
   struct tm * pStructTm = NULL ;
   enum PrecipEndTimeOper interval_end_time_oper ;
   time_t save_time ;

   interval_end_time_oper =  ( enum PrecipEndTimeOper ) clientdata ;

   save_time = end_time_value ;

   /* Test for the "other" case. */
   switch ( interval_end_time_oper )
   {
      case EndTimeDayIncrement :
         
         end_time_value += num_seconds_in_day ;
         break ;

      case EndTimeDayDecrement :

         end_time_value -= num_seconds_in_day ;
         break ;

      case EndTimeHourIncrement : 

         end_time_value += num_seconds_in_hour ;
         break ;

      case EndTimeHourDecrement :

         end_time_value -= num_seconds_in_hour ;
         break ;

      case EndTimeNoOperation :

         break ;

      default :

         flogMessage ( stderr , "\nIn routine \"accum_change_endtime\":\n"
                            "Unrecognized enum PrecipEndTimeOper value.\n"
                            "The end time value of the accumulation interval\n"
                            "is not being modified.\n" ) ;
   }

   if ( end_time_value == 0 )
   {
      end_tm_struct.tm_year = date_st3.year - 1900 ;
      end_tm_struct.tm_mon = date_st3.month - 1 ;
      end_tm_struct.tm_mday = date_st3.day ;
      end_tm_struct.tm_hour = date_st3.hour ;
      end_tm_struct.tm_min = 0 ;
      end_tm_struct.tm_sec = 0 ;

      /* Let the system handle daylight savings time. */
      end_tm_struct.tm_isdst = -999;
      end_time_value = gm_mktime ( & end_tm_struct ) ;
   }

   pStructTm = gmtime ( & end_time_value ) ;
   temp_base_time.year = pStructTm->tm_year + 1900 ;
   temp_base_time.month = pStructTm->tm_mon + 1 ;
   temp_base_time.day = pStructTm->tm_mday ;
   temp_base_time.hour = pStructTm->tm_hour ;

   /* Create the base date time string. */
   memset ( temp_base_time.cdate , '\0' , 11 ) ;
   sprintf ( temp_base_time.cdate , "%4d%02d%02d%02d" , temp_base_time.year ,
             temp_base_time.month , temp_base_time.day, temp_base_time.hour ) ;

   /* Test the base time to make sure it is in the "dates" array. */
   for ( i = 0 ; i < NUMHRS ; ++ i )
   {
      if ( strcmp ( temp_base_time.cdate , dates [ i ].cdate ) == 0 ) break ;
   }

   if ( i == NUMHRS )
   {
      end_time_value = save_time ;
      
      /* Ring the bell. */
     display = XtDisplay ( w ) ;

     if ( display != NULL )
     {
        XBell ( display , 30 ) ; 
     }
   }
   else
   {
      base_time = temp_base_time ;

      /* Create the base date time string formatted for the legend. */
      strftime ( base_time.lldate , 20 , "%b %d %Y %Hz" , pStructTm ) ;

      /* Display the base time value in the text field on GUI. */
      memset ( base_time_string , '\0' , 14 ) ;
      sprintf ( base_time_string , "%04d-%02d-%02d %02d" ,
                base_time.year , base_time.month , base_time.day , 
                base_time.hour ) ;

      XtVaSetValues ( accum_endTimeTF , XmNvalue , base_time_string , NULL ) ;  
   }
}

/*******************************************************************************
* MODULE NUMBER: 4
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
static void accum_select_area ( Widget w , XtPointer clientdata ,
                                XtPointer calldata )
{
   Boolean mprecip_idTB_status , mprecip_valTB_status ; 
   mprecip_idTB_status = XmToggleButtonGetState ( mprecip_idTB ) ;
   mprecip_valTB_status = XmToggleButtonGetState ( mprecip_valTB ) ;
   area = ( enum PrecipAccumArea ) clientdata ;
   area_value = ( int ) area ;
   
   if ( area_value == PrecipGrid )
   {
      DeSensitize ( mprecip_valTB ) ;
      DeSensitize ( mprecip_idTB ) ;
      
      turnOffMultiHourIds ( ) ;
      turnOffMultiHourValues ( ) ;
   }
   else 
   {
      if ( mprecip_idTB_status == True )
         turnOnMultiHourIds ( ) ;
      if ( mprecip_valTB_status == True )
         turnOnMultiHourValues ( ) ;
      Sensitize ( mprecip_valTB ) ;
      Sensitize ( mprecip_idTB ) ;
   }
}

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
static void accum_show_data ( Widget w , XtPointer clientdata ,
                              XtPointer calldata )
{
   enum DisplayFieldData display_field_type ;
   extern int cv_duration ;
   int i ;
   int index = 0 ;
   int j ;

   /* Set the field type to represent that this is a multi hour precipitation
      estimate.  This is for the purpose of drawing the legend. */
   display_field_type = display_multiHour ;
   rad_data [ 0 ].field_type = display_field_type;
   
   /*--------------------------------------------------------------*/
   /*   Display watch cursor and flush the display buffer.         */
   /*--------------------------------------------------------------*/
   mSetCursor ( M_WATCH ) ;
   XFlush(XtDisplay(rad_data[0].w));
 
   for ( i = 0 ; i < NUMHRS ; ++ i )
   {
      if ( strcmp ( base_time.cdate , dates [ i ].cdate ) == 0 ) break ;
      index ++ ;
   }

   if ( index == NUMHRS )
   {
      flogMessage ( stderr , "\nIn routine \"accum_show_data\":\n"
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
   if ( ( index + interval_value ) > NUMHRS )
   {
      flogMessage ( stderr , "\nIn routine \"accum_show_data\":\n"
                         "Unable to completely process an interval of %d\n"
                         "hours because part of it goes back more than\n"
                         "%d hours from the current date and time, the\n"
                         "amount of MPE data which is saved.  There will only\n"
                         "be %d %s of data in the multi-hour precipitation\n"
                         "estimate.\n" , interval_value , NUMHRS ,
                         NUMHRS - index ,
                         ( NUMHRS - index > 1 ) ? "hours" : "hour" ) ;
   }
   
   /* Retrieve each of the Best QPE HRAP grids.  Accumulate the rainfall.
      Make certain that only times within the "dates" array are used. */
   for ( i = index ; ( i < ( index + interval_value ) ) && 
                     ( i < NUMHRS ) ; ++ i )
   {
      display_field_data_RFCW ( display_Xmrg , rad_data[0].data_array ,
                                dates [ i ] , 1 ) ;
   }

   /* If the type of this precipitation accumulation is anything other
      than "GRID", then areal averaging must be performed on the
      precipitation values. */
   if ( area_value != PrecipGrid )
   {

      display_mean_areal_precip ( rad_data[0].data_array , XOR , YOR ,
                                  rad_data[0].maximum_columns , 
                                  rad_data[0].maximum_rows ,
                                  AccumAreaTypes [ area_value ] ) ;                                   
   }
   
   /* Retrieve the colors associated with the precipitation levels. */
   strcpy ( cv_use , "MULTIHOUR" ) ;
   strcpy ( rad_data[0].cv_use , "MULTIHOUR" ) ;

   /* New on November 9, 2004 ... Added the ability to load different
      color schemes for the multihour precipitation product based on
      the accumulation interval. */
   cv_duration = interval_value * SECONDS_PER_HOUR ;
   rad_data[0].cv_duration = cv_duration;
   

   /* Desensitize the menu items the user should not have access to
      while looking at the multi hour precipitation accumulations. */

   /* Desensitize items on the MPEcontrol menu. */
   DeSensitize ( widget_struct->next_widget ) ;
   DeSensitize ( widget_struct->prev_widget ) ;
   DeSensitize ( savemaintop_widget ) ;
   DeSensitize ( savemainbottom_widget ) ;
   DeSensitize ( widget_struct->rerun_widget ) ;

   /* Items on the Tools menu. */
   DeSensitize ( fullscreen_widget );
   DeSensitize ( splitscreen_widget );

   /* DeSensitize items on the Polygons menu. */
   DeSensitize ( drawpoly_widget ) ;
   DeSensitize ( deletepoly_widget ) ;

   /* Desensitize items under the Base Fields menu. */
   DeSensitize ( widget_struct->gage_triangles );

   /* Desensitize the items on the Gage menu. */
   DeSensitize ( widget_struct->qc_precipitation );
   DeSensitize ( widget_struct->qc_temperature );
   DeSensitize ( widget_struct->qc_freezing );
   DeSensitize ( widget_struct->save_level2_data );
   DeSensitize ( widget_struct->pseudo_widget );
   DeSensitize ( widget_struct->gage_table_widget );
   DeSensitize ( widget_struct->single_gage_widget ) ;
   DeSensitize ( showids_widget );
   DeSensitize ( showval_widget );
   DeSensitize ( widget_struct->gage_missing_menu );
   DeSensitize ( widget_struct->gage_color_menu );

   /* Desensitize the items on the Misc menu. */
   DeSensitize ( widget_struct->display_bias_widget ) ;
   DeSensitize ( widget_struct->radar_site_widget ) ;
   DeSensitize ( timelapse_widget ) ;
   DeSensitize ( widget_struct->timelapse6_widget ) ;
   DeSensitize ( widget_struct->timelapse12_widget ) ;
   DeSensitize ( widget_struct->timelapse24_widget ) ;
   DeSensitize ( widget_struct->timelapseother_widget ) ;
   DeSensitize ( widget_struct->stoptime_widget ) ;

   display_field ( "" , 0, 0 ) ;

   /* Force the legend to be updated. */
   mUpdateLegend ( 0 ) ;
}

static void precip_accum_AddCallbacks ( )
{
   Atom atom ;

   atom = XmInternAtom(XtDisplay(accum_displayControlDS),
                       "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(accum_displayControlDS, atom,
                           accum_close_display, NULL);

   XtAddCallback ( accum_closeDisplayPB , XmNactivateCallback , 
                   accum_close_display , NULL ) ;

//   XtAddCallback ( accum_helpDisplayPB , XmNactivateCallback ,
//                   popup_help_window , "MAIN" ) ;

   /* Add callbacks to each of the buttons on the accumulation interval
      pull down option menu. */
   XtAddCallback ( accum_intervalPB1 , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) Accum1 ) ;
   XtAddCallback ( accum_intervalPB3 , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) Accum3 ) ;
   XtAddCallback ( accum_intervalPB6 , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) Accum6 ) ;
   XtAddCallback ( accum_intervalPB12 , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) Accum12 ) ;
   XtAddCallback ( accum_intervalPB24 , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) Accum24 ) ;
   XtAddCallback ( accum_intervalPB36 , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) Accum36 ) ;
   XtAddCallback ( accum_intervalPB48 , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) Accum48 ) ;
   XtAddCallback ( accum_intervalPB72 , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) Accum72 ) ;
   XtAddCallback ( accum_intervalPB_other , XmNactivateCallback ,
                   accum_select_interval , ( XtPointer ) AccumOther ) ;


   /* Add callbacks for the buttons that control the increment and decrement
      of the day and hour values. */
   XtAddCallback ( accum_dayUpAB , XmNactivateCallback , 
                   accum_change_endtime , ( XtPointer ) EndTimeDayIncrement ) ;
   XtAddCallback ( accum_dayDownAB , XmNactivateCallback , 
                   accum_change_endtime , ( XtPointer ) EndTimeDayDecrement ) ;
   XtAddCallback ( accum_hourUpAB , XmNactivateCallback , 
                   accum_change_endtime , ( XtPointer ) EndTimeHourIncrement ) ;
   XtAddCallback ( accum_hourDownAB , XmNactivateCallback , 
                   accum_change_endtime , ( XtPointer ) EndTimeHourDecrement ) ;

   /* Add callbacks to each of the buttons on the accumulation area pull down
      option menu. */
   XtAddCallback ( accum_areaPB1 , XmNactivateCallback ,
                   accum_select_area , ( XtPointer ) PrecipGrid ) ;
   XtAddCallback ( accum_areaPB2 , XmNactivateCallback ,
                   accum_select_area , ( XtPointer ) PrecipBasin ) ;
   XtAddCallback ( accum_areaPB3 , XmNactivateCallback ,
                   accum_select_area , ( XtPointer ) PrecipCounty ) ;
   XtAddCallback ( accum_areaPB4 , XmNactivateCallback ,
                   accum_select_area , ( XtPointer ) PrecipZone ) ;

   /* Add callbacks for the "Show Data" and the "Clear Data" buttons. */
   XtAddCallback ( accum_displayPrecipPB , XmNactivateCallback ,
                   accum_show_data , NULL ) ;
                      
   /* Add callbacks for the "Mean Precip" to each of togglebuttons 
      to display Values and/or station Ids */
   XtAddCallback ( mprecip_valTB , XmNvalueChangedCallback , mean_precip_showCB , ( XtPointer ) 0 ) ;
   XtAddCallback ( mprecip_idTB , XmNvalueChangedCallback , mean_precip_showCB , ( XtPointer )  1 ) ; 
} 

void precip_accum_show ( Widget w )
{

   if ( ( accum_displayControlDS == NULL ) || ( ! accum_displayControlDS ) )
   {
      

      /* Create all of the widgets. */
      create_accum_displayControlDS ( GetTopShell ( w ) ) ;

      /* Set up the callbacks. */
      precip_accum_AddCallbacks ( ) ;
      DeSensitize ( mprecip_valTB ) ;
      DeSensitize ( mprecip_idTB ) ; 
   }
   
   if ( XtIsManaged (  accum_displayControlDS ) == False )
   {
      end_time_value = ( time_t ) 0 ; 
      XtManageChild ( accum_displayControlDS ) ;
      XtManageChild ( accum_displayControlFO ) ;
   }

   XtPopup ( accum_displayControlDS , XtGrabNone ) ;
   accum_change_endtime ( w , ( XtPointer ) EndTimeNoOperation , NULL ) ;

}

void set_base_time ( const date_struct * pDate )
{
   if ( pDate != NULL )
   {
     base_time = *pDate;
   }
}

int get_multi_hour_interval_val ( ) 
{
   return interval_value ;
} 

date_struct get_multi_hour_date_info ( )
{
   return base_time ;
}

void set_multi_hour_interval_val ( int value )
{
   if ( value >= 0 && value <= Accum72 )
   {
      interval_value = value ;
   } 
}

void set_multi_hour_endtime_val ( int value )
{
   if ( value >= 0 && value <= 23 )
   {
      end_time_value = value ;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
