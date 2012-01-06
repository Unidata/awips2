/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:          drawMpeLegend
* DESCRIPTION:
*   MODULE 2:          getMpeDrawLegendStatus
* DESCRIPTION:
*   MODULE 3:          drawMpeLegendPointInfo
* DESCRIPTION:
*   MODULE 4:          dontDrawMpeLegendPointInfo
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <Xm/Label.h> 
#include <Xm/Text.h> 
#include <Xm/Xm.h>

#include "mpe_field_names.h"
#include "display_accum_show.h"
#include "display_field_data_RFCW.h"
#include "drawa.h"
#include "drawMpeLegend.h"
#include "gageqc_gui.h"
#include "map.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "post_functions.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"

static enum MapState legend_state = M_OFF ;
static Widget hrap_xy_label = 0 ;
static Widget hrap_precip_label = 0 ;
static Widget hrap_county_label = 0 ;
static Widget hrap_basin_label = 0 ;

static Widget hrap_xy_text = 0 ;
static Widget hrap_precip_text = 0 ;
static Widget hrap_county_text = 0 ;
static Widget hrap_basin_text = 0 ;

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
*
********************************************************************************
*/

void drawMpeLegend ( int * map_index )
{

 Boolean display_dqc_data;
 Boolean display_mpe_menu = True;
 Boolean is_managed ;
 extern char RFC [ ] ;
 date_struct multi_hour_date_info ;
 enum DisplayFieldData display_field_type ;
 extern struct _Map map [ ] ;
 
 int                          accum_interval;
 int                          actual_height;
 enum MapState                dqc_freezing_flag;
 enum MapState                dqc_precip_flag;
 enum MapState                dqc_temp_flag;
 enum MapState                mpe_data_flag ;
 int                          i , x ;
 int                          offset;
 int                          required_legend_height = 0;
 int                          split_screen;
 int                          y ;
 unsigned int                 width , height ;
 char                         str [ 10 ] , strleg [ 100 ] , str_cv_use [ 50 ] ;

 width = 40 ;
 height = 15 ;
 y = 20 ;

 split_screen = is_screen_split ( );

 if ( ( * map_index == 1 ) && ( split_screen == 0 ) )
 {
    /* This combination does not make sense.  In full screen mode,
       there should not be a bottom legend. */
    return ;
 }

 display_field_type = rad_data [ * map_index ].field_type;

 /*-------------------------------------------------------------------------*/
 /* If no MPE or DailyQC data are being displayed, don't bother doing this. */
 /*-------------------------------------------------------------------------*/
 mpe_data_flag = isThereMpeData ( );
 dqc_temp_flag = isThereDQCtempData ( );
 dqc_precip_flag = isThereDQCprecipData ( );
 dqc_freezing_flag = isThereDQCfreezingData ( );

 
 if ( ( * map_index == 0 ) && ( split_screen == 1 ) )
 {
    /* DailyQC data are not displayed in the top window. */
    display_dqc_data = False; 
 }
 else
 {
    display_dqc_data = True;
 }

 if ( ( * map_index == 1 ) && ( split_screen == 1 ) )
 {
    if ( ( dqc_temp_flag == M_ON ) ||
         ( dqc_precip_flag == M_ON ) ||
         ( dqc_freezing_flag == M_ON ) )
    {
       display_mpe_menu = False;
    }
 }

 if ( ( mpe_data_flag == M_OFF ) && 
      ( dqc_temp_flag == M_OFF ) &&
      ( dqc_precip_flag == M_OFF ) &&
      ( dqc_freezing_flag == M_OFF ) )
 {
    /* There are no MPE or DailyQC data.  Do not display the MPE legend. 
       The legend drawing area on the MPE Editor viewer must be
       disabled as well. This drawing area is owned by the map library.
       In order to keep the map library "pure" and without association
       to any application, the pixmap must be manipulated from here. */

    /* Make sure the legend is not displayed. */
    XtUnmanageChild ( map [ * map_index ].legend ) ;
    return ;
 }

 /* Make sure the legend is large enough to contain the data to be
    displayed. */
 /* Are MPE data displayed? */
 if ( mpe_data_flag == M_ON && display_mpe_menu == True )
 {
    required_legend_height += MPE_LEGEND_HEIGHT;
 }

 /* Are DQC data displayed? */
 if ( ( ( dqc_temp_flag == M_ON ) ||
        ( dqc_precip_flag == M_ON ) ||
        ( dqc_freezing_flag == M_ON ) ) &&
        display_dqc_data == True ) 
 {
    required_legend_height += MPE_LEGEND_HEIGHT;
 }

 /* Is the MPE point info being displayed?  Only display the MPE Point 
    information if the MPE data legend is active. */
    
 if ( ( legend_state == M_ON ) && ( mpe_data_flag == M_ON ) && 
      ( display_mpe_menu == True ) )
 {
    required_legend_height += MPE_LEGEND_HEIGHT;
 }

 /* Retrieve the height of the legend in pixels. */
 actual_height = _get_legend_height_index ( * map_index );

 if ( actual_height != required_legend_height )
 {
    /* Set the legend height.  This call will also fill the legend
       so that it is properly initialized for drawing. */
    _set_legend_height ( * map_index, required_legend_height, 1 );

 }

 /* Test to determine if the legend drawing area is managed. If it isn't,
    then manage it. */
 is_managed = XtIsManaged ( map [ * map_index ].legend ) ;

 if ( is_managed == False )
 {
    XtManageChild ( map [ * map_index ].legend ) ;
 }

 /* Set the base offset. */
 offset = 0;

 /* Based on the type of data being displayed, set the MPE legend to the
    appropriate height and draw the legend data. */ 
 if ( ( mpe_data_flag == 1 ) && ( display_mpe_menu == True ) )
 {
    /*-----------------------------------------------------------*/
    /*   create and display rectangles containing legend colors  */
    /*   for the MPE data.                                       */
    /*-----------------------------------------------------------*/

    for ( i = 0 ; i < rad_data[ * map_index ].num_levels ; i++ )
    {
       x = ( int ) width * i ;
       mSetColor ( color_list_levels [ i ] ) ;
       mDrawFillBox ( M_LEGEND , * map_index , x , y + offset , width , height ) ;
    }

    /*-------------------------------------------------------*/
    /*   create and display legend values                    */
    /*   display varies by type and units of data displayed  */
    /*-------------------------------------------------------*/

    mSetColor ( "White" ) ;
 
    switch ( display_field_type )
    {
     /*----------------------------------*/
    /*   INDEX legend                   */
    /*----------------------------------*/

       case display_Index :
    
          sprintf ( str , "mis" ) ;
          mDrawText ( M_LEGEND , * map_index , width+11 , y-5 + offset , 
                      str ) ;

          for ( i = 0 ; i < NRADARS ; i++ )
          {
             x = ( int ) width * ( i + 2 ) ;
             sprintf ( str , "%s" , nexrad[i].id ) ;
             mDrawText ( M_LEGEND , * map_index , x + width - 26 , 
                         y - 5 + offset , str ) ;
          }

          break;
       
         /*----------------------------------*/
         /*   LOCSPAN legend                 */
         /*----------------------------------*/

       case display_Locspan : 
    
          sprintf ( str , "mis" ) ;
          mDrawText ( M_LEGEND , * map_index , 11 , y - 5 + offset , str ) ;
          memset ( str , '\0' , 10 ) ;
          sprintf ( str , "0" ) ;
          mDrawText ( M_LEGEND , * map_index , 3 * width - 26 , 
                      y - 5 + offset , str ) ;

          for ( i = 1 ; i < 11 ; ++i )
          {
             x = ( int ) width * ( i + 2 ) ;
             sprintf ( str , "%d" , i ) ;
             mDrawText ( M_LEGEND , * map_index , x + width - 26 , 
                         y - 5 + offset , str ) ;

          }
    
          break ;
       
       default :
    
          sprintf ( str , "0.00" ) ;
          mDrawText ( M_LEGEND , * map_index , width + 5 , y - 5 + offset , 
                      str ) ;

          if ( level_value [ 1 ] < 1.0 )
          {
              sprintf ( str , "%.2f" , level_value [ 1 ] ) ;
          }
          else
          { 
              sprintf ( str , "%.0f" , level_value [ 1 ] ) ;
          }

          mDrawText ( M_LEGEND , * map_index , 3 * width - 11 ,
                      y - 5 + offset , str ) ;

         for ( i = 2 ; i < rad_data[ * map_index ].num_levels - 2 ; ++i )
         {
             x = ( int ) width * ( i + 1 ) ;
             sprintf ( str , "%.2f" , level_value [ i ] ) ;
             if ( level_value [ i ] > 6 )
             {
                sprintf ( str , "%.0f" , level_value [ i ] ) ;
                mDrawText ( M_LEGEND , * map_index , x + width - 5 , 
                            y - 5 + offset , str ) ;
             }
             else
             {
                mDrawText ( M_LEGEND , * map_index , x + width - 11 , 
                            y - 5 + offset , str ) ;
             }
         }
       
    }

    switch ( display_field_type )
    {
       case display_rMosaic :

          strcpy(str_cv_use,"Radar-Derived Precip");
          break ;

       case display_avgrMosaic :

          strcpy(str_cv_use,"Average Radar-Derived Precip");
          break ;

       case display_maxrMosaic :

          strcpy(str_cv_use,"Max Radar-Derived Precip");
          break ;

       case display_mMosaic :

          strcpy(str_cv_use,"Multisensor Precip");
          break ;

       case display_rfcMosaic :

          strcpy (str_cv_use, "RFC Best Estimate Mosaic" );
          break;

       case display_mlMosaic :

          strcpy(str_cv_use,"Local Bias Multisensor Precip") ; 
          break ;

       case display_bMosaic :

          strcpy(str_cv_use,"Mean Field Bias Corrected Radar-Derived Precip");
          break ;
      
       case display_Height :

          strcpy(str_cv_use,"Height of Radar Coverage (m) ");
          break ;

       case display_Index :

          strcpy(str_cv_use,"Radar Coverage Map (in)");
          break ;

       case display_gageOnly :

          strcpy(str_cv_use,"Gage Only Analysis (in)");
          break ;

       case display_lMosaic :

          strcpy(str_cv_use,"Local Bias Corrected Radar-Derived Precip (in)");
          break ;

       case display_p3Mosaic :

          strcpy(str_cv_use,"P3 Local Bias Corrected Radar-Derived Precip (in)");
          break ;

       case display_Locspan :

          strcpy(str_cv_use,"memory span index (local bias)");
          break ;

       case display_Locbias :

          strcpy(str_cv_use,"Local Bias Values");
          break ;

       case display_satPrecip :

          strcpy(str_cv_use,"Satellite-Derived Precip (in)");
          break ;

       case display_lsatPrecip :

          strcpy(str_cv_use,"Local Bias Corrected Satellite-Derived Precip");
          break ;

       case display_Xmrg :

          strcpy(str_cv_use,"Saved Precip Estimate (in)");
          break ;

       case display_multiHour :

          accum_interval = get_multi_hour_interval_val ( ) ;
          multi_hour_date_info = get_multi_hour_date_info ( ) ;
          sprintf ( strleg , "%d hr Saved Precip Estimate For %s Ending %s (in)" , 
                 accum_interval , RFC , multi_hour_date_info.lldate ) ;
          mDrawText ( M_LEGEND , * map_index , 15 , y + 35 + offset , 
                      strleg ) ;
          return ;

       case display_Prism :

          strcpy(str_cv_use,"Monthly Normal Precipitation (in)");
          break ;

       case display_maxtempPrism :
   
          strcpy ( str_cv_use, "Monthly Normal Max Temperature (F)");
          break;
   
       case display_mintempPrism :

          strcpy ( str_cv_use, "Monthly Normal Min Temperature (F)");
          break;

       case display_sgMosaic :

          strcpy ( str_cv_use, "Satellite Gage Mosaic (in)");
          break;

       case display_srMosaic :

          strcpy ( str_cv_use, "Satellite Radar Mosaic (in)");
          break;

       case display_srgMosaic :

          strcpy ( str_cv_use, "Satellite Gage Radar Mosaic (in)");
          break;

       case display_rfcbMosaic :

          strcpy ( str_cv_use, "RFC Bias Corrected Radar-Derived Precip (in)");
          break;

       case display_rfcmMosaic :
     
          strcpy ( str_cv_use, "RFC Multisensor Precip (in)");
          break;

       default :

       flogMessage ( stderr , "\nIn routine \"drawMpeLegend.c\":\n"
                          "Unrecognized MPE display type.\n" ) ;
    }

    sprintf ( strleg , "%s   site=%s   %s" , date_st3.lldate , RFC ,
              str_cv_use );
    mDrawText ( M_LEGEND , * map_index , 15 , y + 35 + offset , strleg ) ;

    offset += MPE_LEGEND_HEIGHT;
 }

 /* Is MPE point information displayed? */
 if ( ( legend_state == M_ON ) && ( mpe_data_flag == M_ON ) &&
      ( display_mpe_menu == True ) )
 {
    offset += MPE_LEGEND_HEIGHT;
 }

 /* Are DailyQC data displayed? */
 if ( ( ( dqc_temp_flag == M_ON ) ||
        ( dqc_precip_flag == M_ON ) ||
        ( dqc_freezing_flag == M_ON ) ) &&
      display_dqc_data == True )
 {

    if ( dqc_precip_flag == M_ON )
    {
        draw_precip_legend ( *map_index, offset);
    }
    else if ( dqc_temp_flag == M_ON )
    {
        draw_temperature_legend ( *map_index, offset);
    }
    else if ( dqc_freezing_flag == M_ON )
    {
       draw_freezing_legend ( *map_index, offset );
    } 
    
 }

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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/
enum MapState getMpeDrawLegendStatus ( )
{
   return legend_state ;
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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/

void drawMpeLegendPointInfo ( )
{
   static int first_time = 1 ;
   int mpe_data_flag ;
   Widget parent = 0 ;
   XmString str ;

   legend_state = M_ON ;

   mpe_data_flag = isThereMpeData ( ) ;
  
   if ( mpe_data_flag != 0 )
   {

      /* Create the widgets that will be used to display the HRAP bin
         information. */
      if ( first_time == 1 )
      {
         parent = _get_legend_widget ( 0 ) ;

         /* Create the HRAP X,Y coordinate label. */
         str = XmStringCreateLocalized ( "Hrap x, y:" ) ;
         hrap_xy_label = XtVaCreateManagedWidget ( "hrap_xy_label" ,
                                                  xmLabelWidgetClass ,
                                                  parent ,
                                                  XmNwidth , 80 ,
                                                  XmNheight , 30 ,
                                                  XmNy , 80 ,
                                                  XmNx , 5 ,
                                                  XmNforeground ,
                                                  _get_color ( "White") ,
                                                  XmNbackground ,
                                                  _get_color ( "Black" ) ,
                                                  XmNlabelString , str ,
                                                  NULL ) ;
         XmStringFree ( str ) ;

         /* Create the HRAP X,Y coordinate text field. */
         hrap_xy_text = XtVaCreateManagedWidget ( "hrap_xy_text" ,
                                                 xmTextWidgetClass ,
                                                 parent ,
                                                 XmNwidth , 105 ,
                                                 XmNheight , 30 ,
                                                 XmNy , 80 ,
                                                 XmNx , 90 ,
                                                 XmNeditable , False ,
                                                 XmNforeground ,
                                                 _get_color ( "White") ,
                                                 XmNbackground ,
                                                 _get_color ( "Black" ) ,
                                                 NULL ) ; 

         /* Create the HRAP precipitation value label. */
         str = XmStringCreateLocalized ( "Value:" ) ;
         hrap_precip_label = XtVaCreateManagedWidget ( "hrap_precip_label" , 
                                                       xmLabelWidgetClass ,
                                                       parent ,
                                                       XmNwidth , 60 ,
                                                       XmNheight , 30 ,
                                                       XmNy , 80 , 
                                                       XmNx , 200 ,
                                                       XmNforeground ,
                                                       _get_color ( "White") ,
                                                       XmNbackground ,
                                                       _get_color ( "Black" ) ,
                                                       XmNlabelString , str ,
                                                       NULL ) ;
         XmStringFree ( str ) ;

         /* Create the HRAP X coordinate text field. */
         hrap_precip_text = XtVaCreateManagedWidget ( "hrap_precip_text" ,
                                                 xmTextWidgetClass ,
                                                 parent ,
                                                 XmNwidth , 80 ,
                                                 XmNheight , 30 ,
                                                 XmNy , 80 ,
                                                 XmNx , 265 ,
                                                 XmNeditable , False ,
                                                 XmNforeground ,
                                                 _get_color ( "White") ,
                                                 XmNbackground ,
                                                 _get_color ( "Black" ) ,
                                                 NULL ) ; 

         /* Create the county label. */
         str = XmStringCreateLocalized ( "County:" ) ;

         hrap_county_label = XtVaCreateManagedWidget ( "hrap_county_label" , 
                                                       xmLabelWidgetClass ,
                                                       parent ,
                                                       XmNwidth , 90 ,
                                                       XmNheight , 30 ,
                                                       XmNy , 80 , 
                                                       XmNx , 350 ,
                                                       XmNforeground ,
                                                       _get_color ( "White") ,
                                                       XmNbackground ,
                                                       _get_color ( "Black" ) ,
                                                       XmNlabelString , str ,
                                                       NULL ) ;
         XmStringFree ( str ) ;

         /* Create the county text field. */
         hrap_county_text = XtVaCreateManagedWidget ( "hrap_county_text" ,
                                                      xmTextWidgetClass ,
                                                      parent ,
                                                      XmNwidth , 210 ,
                                                      XmNheight , 30 ,
                                                      XmNy , 80 ,
                                                      XmNx , 445 ,
                                                      XmNeditable , False ,
                                                      XmNforeground ,
                                                      _get_color ( "White") ,
                                                      XmNbackground ,
                                                      _get_color ( "Black" ) ,
                                                      NULL ) ; 

         /* Create the basin label. */
         str = XmStringCreateLocalized ( "Basin:" ) ;
         hrap_basin_label = XtVaCreateManagedWidget ( "hrap_basin_label" , 
                                                      xmLabelWidgetClass ,
                                                      parent ,
                                                      XmNwidth , 80 ,
                                                      XmNheight , 30 ,
                                                      XmNy , 80 , 
                                                      XmNx , 660 ,
                                                      XmNforeground ,
                                                      _get_color ( "White") ,
                                                      XmNbackground ,
                                                      _get_color ( "Black" ) ,
                                                      XmNlabelString , str ,
                                                      NULL ) ;
         XmStringFree ( str ) ;

         /* Create the basin text field. */
         hrap_basin_text = XtVaCreateManagedWidget ( "hrap_basin_text" ,
                                                     xmTextWidgetClass ,
                                                     parent ,
                                                     XmNwidth , 130 ,
                                                     XmNheight , 30 ,
                                                     XmNy , 80 ,
                                                     XmNx , 745 ,
                                                     XmNeditable , False ,
                                                     XmNforeground ,
                                                     _get_color ( "White") ,
                                                     XmNbackground ,
                                                     _get_color ( "Black" ) ,
                                                     NULL ) ; 
      }
      else
      {
         /* The MPE point info legend widgets have already been created.  
            Manage these widgets to make them visible. */ 
         XtManageChild ( hrap_xy_label );
         XtManageChild ( hrap_xy_text );
         XtManageChild ( hrap_precip_label );
         XtManageChild ( hrap_precip_text );
         XtManageChild ( hrap_county_label );
         XtManageChild ( hrap_county_text );
         XtManageChild ( hrap_basin_label );
         XtManageChild ( hrap_basin_text );
      }

      mUpdateLegend ( 0 ) ;
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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/
void dontDrawMpeLegendPointInfo ( )
{
   legend_state = M_OFF ;

   /* Unmanage the MPE point info legend widgets to make them invisible. */
   XtUnmanageChild ( hrap_xy_label );
   XtUnmanageChild ( hrap_xy_text );
   XtUnmanageChild ( hrap_precip_label );
   XtUnmanageChild ( hrap_precip_text );
   XtUnmanageChild ( hrap_county_label );
   XtUnmanageChild ( hrap_county_text );
   XtUnmanageChild ( hrap_basin_label );
   XtUnmanageChild ( hrap_basin_text );

   mUpdateLegend ( 0 ) ;
}

/*******************************************************************************
* MODULE NUMBER:   5
* MODULE NAME:     getMpeLegendHrapXYText 
* PURPOSE:         Returns the text widget representing the Hrap X coordinate
*                  value. 
*
* ARGUMENTS:
*   None.
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   Widget      hrap_xy_text                The text widget to which the
*                                           x,y coordinates under the HRAP bin
*                                           are written.
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   Not Applicable
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
Widget getMpeLegendHrapXYText ( )
{
   return hrap_xy_text ;
}

/*******************************************************************************
* MODULE NUMBER:   7
* MODULE NAME:     getMpeLegendHrapValueText 
* PURPOSE:         Returns the text widget to which the precipitation value
*                  in the HRAP bin under the mouse cursor is written. 
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE   NAME             DESCRIPTION
*   Widget      hrap_value_text  Contains the widget to which the current HRAP
*                                bin precipitation value can be written.
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
Widget getMpeLegendHrapValueText ( )
{
   return hrap_precip_text ;
}

/*******************************************************************************
* MODULE NUMBER:   8
* MODULE NAME:     getMpeLegendHrapCountyText 
* PURPOSE:         Returns the text area widget into which the label of the
*                  county containing the HRAP bin under the mouse pointer
*                  is written.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE   NAME                DESCRIPTION
*   Widget      hrap_county_text    The text area widget to which the
*                                   county label information is written.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
Widget getMpeLegendHrapCountyText ( )
{
   return hrap_county_text ;
}

/*******************************************************************************
* MODULE NUMBER:   9 
* MODULE NAME:     getMpeLegendHrapBasinText 
* PURPOSE:         Returns the text area widget into which the label of the
*                  basin containing the HRAP bin under the mouse cursor
*                  is written.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE   NAME              DESCRIPTION
*   Widget      hrap_basin_text   The text area widget into which
*                                 the label of the basin containing the
*                                 HRAP bin under the mouse pointer is
*                                 stored. 
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   Not needed. 
********************************************************************************
*/
Widget getMpeLegendHrapBasinText ( )
{
   return hrap_basin_text ; 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/drawMpeLegend.c,v $";
 static char rcs_id2[] = "$Id: drawMpeLegend.c,v 1.18 2007/05/24 13:05:30 whfs Exp $";}
/*  ===================================================  */

}
