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

#include "display_accum_show.h"
#include "display_field_data_RFCW.h"
#include "drawa.h"
#include "gageqc_gui.h"
#include "map.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_resource.h"
#include "post_functions.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "pointcontrol_legend.h"
#include "drawHvLegend.h"
#include "HvColorList.h"
#include "pointcontrol_mgr.h"

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

void drawHvLegend ( int * map_index )
{
    
 //    char header[] = "drawHvLegend(): ";
     Boolean display_mpe_menu = True;
     Boolean is_managed ;
     enum DisplayFieldData display_field_type ;
     extern struct _Map map [ ] ;
     
     
     int                          actual_height;
     enum MapState                mpe_data_flag ;
     int                          offset;
     int                          required_legend_height = 0;
     int                          y ;
     unsigned int                 width , height ;
     int drawingPdcData = 0;
       
     width = 40 ;
     height = 15 ;
     y = 20 ;
    
     display_field_type = rad_data [ * map_index ].field_type;
    
     /*-------------------------------------------------------------------------*/
     /* If no MPE or PDC data are being displayed, don't bother doing this. */
     /*-------------------------------------------------------------------------*/
     mpe_data_flag = isThereMpeData ( );
     
     
     // determine if the PDC data is being displayed
     drawingPdcData = getStationDrawingState ( ); 
    

     if ((mpe_data_flag == M_OFF ) && (! drawingPdcData) )
     {
        /* There are no MPE or PDC data.  Do not display the MPE legend. 
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
    
    
     /* Is the MPE point info being displayed?  Only display the MPE Point 
        information if the MPE data legend is active. */
        
    if ( ( legend_state == M_ON ) && ( mpe_data_flag == M_ON ) && 
          ( display_mpe_menu == True ) )
    {
        required_legend_height += MPE_LEGEND_HEIGHT;
    }
 
 
 
     // adjust the legend size of the pdc data, if needed
     if ( drawingPdcData )
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


     draw_mpe_legend(map_index, offset, 
                     mpe_data_flag, display_field_type,
                     width, height, y);

     /* Is MPE point information displayed? */
     if ( ( mpe_data_flag == M_ON ) &&
              ( display_mpe_menu == True )  )
     {
        offset += MPE_LEGEND_HEIGHT;
        
        if (legend_state == M_ON)
        {
            offset += MPE_LEGEND_HEIGHT;
        }
     
     }
        

     //draw the PDC's portion of the legend
     if (drawingPdcData)
     {   
         draw_pdc_legend(map_index, offset, width, height); 
         offset += MPE_LEGEND_HEIGHT;      
     } 
    
    return;

}

// --------------------------------------------------------------------------------------------
void draw_mpe_legend(int * map_index, int offset,
                     enum MapState mpe_data_flag,
                     enum DisplayFieldData display_field_type,
                     unsigned int width,
                     unsigned int height,
                     int y)
{
    
     Boolean display_mpe_menu = True;
     extern char RFC [ ] ;
     date_struct multi_hour_date_info ;
     extern struct _Map map [ ] ;
     int                          accum_interval;
     int                          i;
     int                          x;

     char                         str [ 10 ] , 
                                  strleg [ 100 ],
                                  str_cv_use [ 50 ] ;
                                    
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

       default :

       fprintf ( stderr , "\nIn routine \"drawMpeLegend.c\":\n"
                          "Unrecognized MPE display type.\n" ) ;
    }

    sprintf ( strleg , "%s   site=%s   %s" , date_st3.lldate , RFC ,
              str_cv_use );
    mDrawText ( M_LEGEND , * map_index , 15 , y + 35 + offset , strleg ) ;

    return;
 }
    
    
    
}

// --------------------------------------------------------------------------------------------

void draw_pdc_legend(int * map_index, int y_starting,
                     unsigned int width,
                     unsigned int height)
{
   //draw the pdc portion of the legend 
    
  // char header[] = "draw_pdc_legend(): ";
   int isDrawingTheStation = getStationDrawingState ( );
   int bar_width = 40;
   int bar_height = 15;
   int text_spacing = 10;
   int text_height = 10;
   int x = 0;
   int value_text_y = y_starting + text_height;
   int bar_y = value_text_y;
   int value_text_left_shift = 15;
   int legend_y = 0;
   int is_time_step_mode = 0;
   
   
   pc_options_struct * pc_options = get_pc_options();
        
   if (pc_options->query_mode == TIME_STEP_MODE)
   {
       is_time_step_mode = 1;
   }
     
   if ( isDrawingTheStation)
   {
        
       // we use gray, dark green, green, yellow, and red for this when the
       // TIME STEP MODE element type is RIVER
       if ( (is_time_step_mode) && pc_options->element_type != RIVER_TIME_STEP_TYPE)
       {
           draw_pdc_legend_color_bar(map_index, bar_width, bar_height, 
                                    value_text_y,
                                    bar_y,
                                    value_text_left_shift);
   
           legend_y = bar_y + bar_height + text_spacing;
       }
       else if  ( (is_time_step_mode) && 
                 ( (pc_options->selectedTimeStepElement == DEPTH_ABOVE_FS_TSDE)  ||
                   (pc_options->selectedTimeStepElement == PERCENT_FLOOD_FLOW_TSDE)
                 )
                )
       {
           draw_pdc_legend_color_bar(map_index, bar_width, bar_height, 
                                    value_text_y,
                                    bar_y,
                                    value_text_left_shift);
   
           legend_y = bar_y + bar_height + text_spacing;
       }
       
       else //there is no color bar
       {
           legend_y = y_starting + text_height;
       }

       char * legendString = getPDCLegendString();
       
       
 //      printf("%s value_text_y = :%d: \n", header, value_text_y);  
 //      printf("%s legendString = :%s: \n", header, legendString);  
 //      printf("%s bar_y = :%d: \n", header, bar_y);  
     
 //      printf("%s legend_y = :%d: \n", header, legend_y);  
       
       mSetColor("yellow");
      
       mDrawText ( M_LEGEND , * map_index ,
                    x,
                    legend_y,
                    legendString ) ;

       mSetColor("white");

       if (legendString != NULL)
       {
                free(legendString);
                legendString = NULL;
       }

   }
   
   return; 
}

// --------------------------------------------------------------------------------------------
void draw_pdc_legend_color_bar(int * map_index,
                     unsigned int bar_width,
                     unsigned int bar_height,
                     int value_text_y,
                     int bar_y,
                     int value_text_left_shift)
{
    
     char header[] = "draw_pdc_legend_color_bar(): ";
     HvColorList * hcl = getHvColorList();
     int i = 0;
     char text[50];
  //   int text_height = 30;
   //  int bar_y = y + text_height;
  //   int value_text_y = y + text_height;
 //    int half_value_text_length = 15;
     

    printf("%s bar_y = %d value_text_y = %d \n", header, bar_y, value_text_y);

    int x = 0;
    int text_x = 0;
    
    mSetColor ( hcl->color_threshold_array.missingColorName ) ;
    mDrawFillBox ( M_LEGEND , * map_index , x , bar_y, bar_width , bar_height ) ;
    x += bar_width;

    mSetColor ( hcl->color_threshold_array.defaultColorName ) ;
    mDrawFillBox ( M_LEGEND , * map_index , x , bar_y, bar_width , bar_height ) ;
    x += bar_width;


    for ( i = 0 ; i < hcl->color_threshold_array.length ; i++ )
    {
       x = ( int ) bar_width  * (i + 2) ;
       text_x = x - value_text_left_shift ;
       
       mSetColor ( "White" ) ;
       sprintf(text, "%6.2lf", hcl->color_threshold_array.thresholds[i].value); 
       
   //    printf("%s  text = :%s: y = %d  text_x = %d \n", header, text, value_text_y, text_x);
 
       mDrawText(M_LEGEND , 
                 * map_index ,
                   text_x , 
                   value_text_y,
                   text );
                   
       mSetColor ( hcl->color_threshold_array.thresholds[i].colorName ) ;
       mDrawFillBox ( M_LEGEND , * map_index , 
                     x , bar_y,
                     bar_width , bar_height ) ;
    }

    /*-------------------------------------------------------*/
    /*   create and display legend values                    */
    /*   display varies by type and units of data displayed  */
    /*-------------------------------------------------------*/

    mSetColor ( "White" ) ;
 
    return;    
    
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
enum MapState getHvDrawLegendStatus ( )
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

void drawHvLegendPointInfo ( )
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
void dontDrawHvLegendPointInfo ( )
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
Widget getHvLegendHrapXYText ( )
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
Widget getHvLegendHrapValueText ( )
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
Widget getHvLegendHrapCountyText ( )
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
Widget getHvLegendHrapBasinText ( )
{
   return hrap_basin_text ; 
}
