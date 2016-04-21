#include "drawStations.h"
#include "HvDisplayControlProto.h"
#include "HvColorList.h"
#include "ColorThreshold.h"
#include "hv_color_threshold.h"
#include "map.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_resource.h"
#include "pointcontrol_mgr.h"
#include "PointDisplayControl.h"
#include "pointcontrol_options.h"
#include "pointcontrol_legend.h"
#include "pointcontrol_loc_shift.h"
#include "pointcontrol_derive.h"

/***********************************************************************/

void drawAllStationDataSets ( )
{
	
 //   char header[] = "drawAllStationDataSets(): ";
	pc_options_struct * pc_options = get_pc_options();
   int	is_river_request ;
   int  point_data_update_state ;
   int state ;
   int x ;
   int y ;
 
   ReportList * reportlistHead  = getReportListHead();
   ReportList * pReportlistNode = NULL ;

   state = getStationDrawingState ( ) ;

   if ( state != 0 )
   {
      /* Only load and transfer the point data and pc options
         if the data has been modified via the point control gui
         or the timed point control refresh. */
      point_data_update_state = get_pointdata_update_flag ( ) ;

      if ( point_data_update_state == 1 )
      {
         clear_pointdata_update_flag ( ) ;
      }

      /* Is there any river data to plot? */
      if ( reportlistHead == NULL )
      {
         return ;
      }
   
      /* Check if this is a request to plot river data, which
         involves unique display features */
      is_river_request = check_if_rivertype ( ) ;

      // load color threshold array into the HvColorList structure
      loadColorThresholdArrayByPcOptions(pc_options);

      /* Draw the data for each station */
      pReportlistNode = ( ReportList  * ) ListFirst 
                                          ( & reportlistHead->list ) ;

      while ( pReportlistNode != NULL )
      {
         if ( pReportlistNode->use == 1 )
         {
            /* This station must be drawn. Convert its latitude and longitude
               into pixel coordinates. */
            mConvertLatLon2XY ( pReportlistNode->latitude ,
                                pReportlistNode->longitude ,
                                & x , & y ) ; 
                 
            //shift the x and y coords by the custom display shift amount (if any)                               
            pReportlistNode->pos.x = x + pReportlistNode->x_shift;                    
            pReportlistNode->pos.y = y + pReportlistNode->y_shift; 
                       
   	        drawStationDataSet ( pReportlistNode , pc_options ,
                                 is_river_request ,
                                 pReportlistNode->pos.x, pReportlistNode->pos.y) ;
                                                                 
         } 

         pReportlistNode = ( ReportList * ) ListNext 
                                           ( & pReportlistNode->node ) ;
      }
   }

   return ;
}


// --------------------------------------------------------------------------

void drawStationDataSet( ReportList * pReportList , 
                         pc_options_struct * pPcOptions ,
                         int is_river_request , int x , int y  )
{
   
   /* show the dot */
   if (pPcOptions->query_mode == TIME_STEP_MODE)
   {           
       if (! pPcOptions->icon) // draw dot only when the icon is not being displayed
       {
           drawDot(pReportList, pPcOptions, x, y); 
       } 
   }
    
   /* show the icon */
   drawIcon( pReportList , pPcOptions , x , y) ;
   
   /* draw labels such as id and name */
   drawIconLabels( pReportList , pPcOptions , x , y) ;
   
   /* lastly draw the data based on the type of request */
   if ( is_river_request )
      drawRiverStationData( pReportList , pPcOptions , x , y) ;
   else   
      drawGeneralStationData( pReportList , pPcOptions , x , y) ;  
   
   return;
}



/***********************************************************************/
void drawDot ( ReportList * pReportList ,
               pc_options_struct * pc_options ,
               int x ,
               int y )
{
   char * valueColor = NULL ;
   
   
   //only draw a dot when in TIME_STEP_MODE
   if  ( (pc_options->selectedTimeStepElement == STAGE_POOL_TSDE) || 
              (pc_options->selectedTimeStepElement == FLOW_STORAGE_TSDE)
            )
   {
       valueColor = getRiverValueColorForTimeStepMode(pReportList, pc_options);
   }
       
   else
   {
       valueColor = determineValueColor(pReportList->value, pc_options);
   }
        
    mSetColor ( valueColor ) ;
    
    char * dotString = ".";
    
    //int text_width = mGetStringWidth(dotString);
   
    mDrawText ( M_EXPOSE , 0 ,
                x,
                y,
                dotString ) ;
}
  

/***********************************************************************/

void drawIcon ( ReportList * pReportList ,
                pc_options_struct * pPcOptions ,
                int x ,
                int y )
{
   char * color = NULL ;

   char isFcstPt ;
   char isRiver ;
   char isReservoir ;
   char isNonRiver ;
      
   HvColorList * hcl = getHvColorList ( ) ;
   
   /*draw the station icon size based on set_pdsize_flag*/
   
   define_pdsize(set_pdsize_flag);
   
   /* display icon only if requested */
   if ( pPcOptions->icon == 1 )
   {
      /* determine color of triangle if a station
	 has stage or discharge data */
      if ( strstr ( pReportList->disp_class , DISP_CLASS_FCSTPT ) != NULL )
      {
         isFcstPt = 1 ;
      }
      else
      {
         isFcstPt = 0 ;
      }

      if ( strstr ( pReportList->disp_class , DISP_CLASS_RESERVOIR ) != NULL)
      {
         isReservoir = 1 ;
      }
      else
      {
         isReservoir = 0 ;
      }
  
      if ( strstr ( pReportList->disp_class , DISP_CLASS_RIVER ) != NULL ||
          isFcstPt || isReservoir )
      {
         isRiver = 1;
      }
      else
      {
         isRiver = 0;
      }

   if ( ( strstr ( pReportList->disp_class , DISP_CLASS_PRECIP ) != NULL ) ||
        ( strstr ( pReportList->disp_class , DISP_CLASS_SNOW )  != NULL ) ||
        ( strstr ( pReportList->disp_class , DISP_CLASS_TEMP )  != NULL ) ||
        ( strstr ( pReportList->disp_class , DISP_CLASS_OTHER )  != NULL ) )
   {
      isNonRiver = 1;
   }
   else
   {
      isNonRiver = 0;
   }
 
      if ( isFcstPt || isRiver || isReservoir )
      {
          color = getRiverStationColor ( pReportList ) ;
      }
      
      /* display icon  */
      if ( isFcstPt )
      {
         drawFcstPointStationIcon ( x , 
                                    y , 
                                    color ) ;
      }

      if ( isRiver )
      {
         drawRiverStationIcon ( x , 
                                y , 
                                color ) ;
      }

      if ( isReservoir )
      {
         drawReservoirStationIcon ( x , 
                                    y , 
                                    color ) ;
      }

      if ( isNonRiver )
      {
         drawOtherStationIcon ( x , 
                                y , 
                                hcl->otherStationColor ) ;
      }
   }
   
   return;   
}

/***********************************************************************/
char * getRiverValueColorForTimeStepMode( ReportList * pReportList,
                                          pc_options_struct *pc_options )
{
   HvColorList      *hcl = getHvColorList ( ) ;
   static char color [ MAX_COLOR_NAME_LENGTH ] ;
  
   char threat_index = THREAT_MISSING_DATA;
   
   double actionLevel = MISSING_VAL;
   double floodLevel = MISSING_VAL;
   
   char header[] = "getRiverValueColorForTimeStepMode(): ";
   
   rs_info_struct * rsInfoPtr = get_rs_info(pReportList->lid);
   
   TimeStepDataElement selectedTimeStepElement = 
        pc_options->selectedTimeStepElement;
      
   char  * block_text = NULL; 
      
   if (rsInfoPtr != NULL)
   {       
    
       //set actionLevel and floodLevel
       if (selectedTimeStepElement == STAGE_POOL_TSDE)
       {
            actionLevel = rsInfoPtr->as;
            floodLevel = rsInfoPtr->fs;   
            
            block_text = "STAGE"; 
            
       }
       else if (selectedTimeStepElement == FLOW_STORAGE_TSDE)
       {
            actionLevel = rsInfoPtr->aq;
            floodLevel = rsInfoPtr->fq;   
            
            block_text = "FLOW";   
       }
       else
       {
            actionLevel = rsInfoPtr->as;
            floodLevel = rsInfoPtr->fs; 
            
            block_text = "OTHER";   
       }
       
       //determine the threat level
       if (pReportList->value != MISSING_VAL)
       {
           threat_index = THREAT_MISSING_STAGE; //next level up 
            
            
           if ( actionLevel != MISSING_VAL)
           {
               if (pReportList->value >= actionLevel)
               {
                   threat_index = THREAT_ACTION;
               }  
               else
               {
                   threat_index = THREAT_NONE;
               }  
           } 
            
           if ( floodLevel != MISSING_VAL)
           {
                if (pReportList->value >= floodLevel)
                {
                    threat_index = THREAT_FLOOD;
                }
                else if (actionLevel == MISSING_VAL)
                {
                    threat_index = THREAT_NONE;
                }
           }                    
       } //end if (pReportList->value != MISSING_VAL)
       
       else // current data was missing
       {
           threat_index = THREAT_MISSING_DATA;      
       }  
   }
   else //station is null
   {
        printf("%s  station %s IS NULL \n", header, pReportList->lid); 
   }
   
   /* Fill the color array with '\0' characters. */
   memset ( color , '\0' , MAX_COLOR_NAME_LENGTH ) ;

   switch (threat_index )
   {
      case THREAT_MISSING_DATA :

         strcpy ( color , hcl->noDataColor ) ;
         break ;

      case THREAT_MISSING_STAGE :

         strcpy ( color , hcl->noStageColor ) ;
         break ;

      case THREAT_NONE :

         strcpy ( color , hcl->nonFloodColor ) ;
         break ;

      case THREAT_ACTION :

         strcpy ( color , hcl->nearFloodColor ) ;
         break ;

      case THREAT_FLOOD :

         strcpy ( color , hcl->floodColor ) ;
         break ;

      default :

         fprintf ( stderr , "\nIn routine \"getRiverValueColorForTimeStepMode\":\n"
                            "Reached default case in switch statement.\n"
                            "Unrecognized switch value '%c'.\n" , 
                            threat_index ) ; 
         strcpy ( color , hcl->noDataColor ) ;
         break ;
   } 

   return color;   
}


/***********************************************************************/



char * getRiverStationColor( ReportList * pReportList )
{
   HvColorList		*hcl = getHvColorList ( ) ;
   static char color [ MAX_COLOR_NAME_LENGTH ] ;
   
   /* Fill the color array with '\0' characters. */
   memset ( color , '\0' , MAX_COLOR_NAME_LENGTH ) ;

   switch ( pReportList->threat_index )
   {
      case THREAT_MISSING_DATA :

         strcpy ( color , hcl->noDataColor ) ;
         break ;

      case THREAT_MISSING_STAGE :

         strcpy ( color , hcl->noStageColor ) ;
         break ;

      case THREAT_NONE :

         strcpy ( color , hcl->nonFloodColor ) ;
         break ;

      case THREAT_ACTION :

         strcpy ( color , hcl->nearFloodColor ) ;
         break ;

      case THREAT_FLOOD :

         strcpy ( color , hcl->floodColor ) ;
         break ;

      default :

         fprintf ( stderr , "\nIn routine \"getRiverStationColor\":\n"
                            "Reached default case in select statement.\n"
                            "Unrecognized switch value '%c'.\n" , 
                            pReportList->threat_index ) ; 
         strcpy ( color , hcl->noDataColor ) ;
         break ;
   } 

   return color;   
}


/***********************************************************************/

void drawFcstPointStationIcon( int		x ,
			       int 	        y ,
			       char *  	        color )
{
	
   draw_river_forecast_point(M_EXPOSE, 0, x, y, color);	
  // mDrawSymbol ( M_EXPOSE , 0 , x , y , color , 
  //               M_RIVER_FORECAST_POINT ) ;
}


/***********************************************************************/

void drawRiverStationIcon ( int 	x ,
			    int 	y ,
			    char *      color )
{
	
   draw_river_data_point(M_EXPOSE, 0, x, y, color);	
   //mDrawSymbol ( M_EXPOSE , 0 , x , y , color , 
   //              M_RIVER_DATA_POINT ) ;
}


/***********************************************************************/

void drawReservoirStationIcon ( int x , 
			        int y , 
			        char * color )
{
	
   draw_river_reservoir(M_EXPOSE, 0, x, y, color);	
   
  // mDrawSymbol ( M_EXPOSE , 0 , x , y , color ,
 //                M_RESERVOIR_POINT ) ;
}


/***********************************************************************/

void drawOtherStationIcon( int 	x , 
			   int 	y ,
               char * color )
{  
	
   draw_other_station(M_EXPOSE, 0, x, y, color);		
 //  mDrawSymbol ( M_EXPOSE , 0 ,  x , y , color , 
  //               M_OTHER_STATION ) ;
}


/***********************************************************************/ 

void drawIconLabels( ReportList * pReportList ,
                     pc_options_struct * pPcOptions ,
                     int x , int y )
{   
   HvColorList *hcl = getHvColorList ( ) ;
   
   char idLabel[BUFSIZ];
   char nameLabel[BUFSIZ];
   
   mSetColor ( hcl->labelColor ) ;

   if ( pPcOptions->id == 1 )
   {      
       sprintf ( idLabel , "%s" , pReportList->lid );  
       int text_width = mGetStringWidth(idLabel);
       int shift = text_width + X_SMALL_SPACING;
      
       mDrawText ( M_EXPOSE , 0 ,
                  x - shift,
		          y + Y_ID_OFFSET,
		          idLabel ) ;
   }
   
   if ( pPcOptions->name == 1 )
   {
      sprintf(nameLabel, "%s", pReportList->name);
      mDrawText ( M_EXPOSE , 0 ,
                  x + X_NAME_OFFSET ,
		          y + Y_NAME_OFFSET ,
		          nameLabel ) ;
   }
  
   return;
}

// -----------------------------------------------------------------------

void drawRiverStationData ( ReportList * pReportList ,
                            pc_options_struct * pc_options ,
                            int x , int y )
{   
    
   //char header[] = "drawRiverStationData(): ";
   time_t	actualTime;
   double	displayedValue1 = MISSING;
   double	displayedValue2 = MISSING;
   char * valueColor = NULL;  
     
     
     
   /* get the value for the report */
   actualTime  = pReportList->validtime;
   
   /* set the displayedValue to be either the actual value
      or the flood level departure value */
   displayedValue1 = pReportList->value ;
   
   
   
   
   if (pc_options->query_mode == TIME_STEP_MODE)
   {           
        if  ( (pc_options->selectedTimeStepElement == STAGE_POOL_TSDE) || 
              (pc_options->selectedTimeStepElement == FLOW_STORAGE_TSDE)
            )
        {
            valueColor = getRiverValueColorForTimeStepMode(pReportList, pc_options);
        }
        else
        {
            valueColor = determineValueColor(displayedValue1, pc_options);
        }
   }
   else // in AD_HOC_MODE, don't color the text anything other than labelColor
   {
        HvColorList *hcl = getHvColorList ( ) ;
        valueColor = hcl->labelColor;
   }
 
   /* the second value is set to the flood level or 
      derived stage / flow. A switch defined in the ReportList 
      structure is used later to determine whether to plot 
      the data*/
   displayedValue2 = pReportList->value2 ;
   
   /* draw name and id labels, if requested */
   drawPointLabels(displayedValue1, 
                   actualTime, 
		           valueColor,
                   x,
                   y,
		           displayedValue2,
                   True, 
                   pReportList ,
                   pc_options ) ;
   return;
}


/***********************************************************************/

void drawGeneralStationData( ReportList * pReportList ,
                             pc_options_struct * pc_options ,
                             int x , int y )
{
   
   char * valueColor = NULL;
   double value = pReportList->value;
   
   if (pc_options->query_mode == TIME_STEP_MODE)
   {
        valueColor = determineValueColor(value, pc_options);
   }
   else // in AD_HOC_MODE
   {
        HvColorList *hcl = getHvColorList ( ) ;
        valueColor = hcl->labelColor;
   }
   
   
   
   drawPointLabels( value ,
		           pReportList->validtime ,
		          // hcl->labelColor ,
                  valueColor,
                  x,
                  y,
		          MISSING ,
                  False ,
		          pReportList ,
                  pc_options ) ;

   return;
}



/***********************************************************************/   

void drawPointLabels(double 		displayedValue1,
		     		time_t 		    value1Time,
		    		 char *	        value1Color,
                     int            x ,
                     int            y ,
		    		 double 		displayedValue2,
                     int            display_value2 ,
		     		 ReportList *   pReportList ,
                     pc_options_struct * pPcOptions ) 
{
   HvColorList *hcl = getHvColorList();
 //  char header[] = "drawPointLabels(): ";
   char 	valueLabel[BUFSIZ];
   char 	dateLabel[BUFSIZ];
   char 	hourLabel[BUFSIZ];
   char     elevationLabel[20];
   char     paramCodeLabel[BUFSIZ];
   Boolean 	valueMissing = True;
   Boolean      showValue1 ;
   Boolean      showValue2 ;
   Boolean  isPcData = False;
   char		formatstr[12];
   
   
   /* determine if the value1 is missing.
      this is needed for displaying value1 and for the time. */   
   
   if ((displayedValue1 != MISSING) &&
       (IsNull(FLOAT, &displayedValue1) != ISNULL))
      valueMissing = False;	 
   
   else
      valueMissing = True;
   
   /* determine the format based on the PE being processed */
   set_PE_formatstr ( pReportList->pe , formatstr ) ;
   
   if (strcmp(pReportList->pe, "PC") == 0)
   {
       isPcData = True;
   }
   
   /* Logic for determining how the data values are displayed. */
   showValue1 = pPcOptions->value;
  
   if ( showValue1 == False )
   {
      showValue2 = False ;
   }
   else
   {
      showValue2 = pPcOptions->fldlevel | pPcOptions->derive_stage_flow ;
      if (pPcOptions->query_mode == TIME_STEP_MODE) //never show value2 in TimeStep Mode
      {
          showValue2 = False;  
      }
   }

   /* create and draw the value label(s) using the determined format */
   if ( showValue1 == True )
   {
      if (!valueMissing)
	      sprintf(valueLabel, formatstr, displayedValue1);      
      else
	      strcpy(valueLabel, "M");
          
      //shift the drawing of PC data up higher than PP data, so can show both
      //when they are different virtual stations (but same actual station)    


      mSetColor ( value1Color ) ;


      int text_width = mGetStringWidth(valueLabel);
      int xTextShift = text_width + X_SMALL_SPACING;
    

      int yTextShiftAmount = determineYDataShift(pReportList);
          mDrawText ( M_EXPOSE , 0 ,
                      x - xTextShift ,
                      y + yTextShiftAmount,
                      valueLabel ) ;
     
   }
      
   if ( ( display_value2 == True ) && 
        ( pPcOptions->time_mode != VALUE_CHANGE ) )
   { 
      if ( showValue2 )
      {
         if ( displayedValue2 != MISSING )
         {
   
            /* Determine the format that value2 should be displayed as.
               Use the format string for value1 except in the case where
               value2 represents a derived flow. */
            if ( pReportList->pe[0] == 'H' &&
                 pReportList->pe[1] == 'G' &&
                 pPcOptions->derive_stage_flow == 1 )
            {
                sprintf ( valueLabel , "%6.0f" , displayedValue2 ) ;
            }
            else if ( pReportList->pe[0] == 'Q' &&
                      pReportList->pe[1] == 'R' &&
                      pPcOptions->derive_stage_flow == 1 )
            {
                sprintf ( valueLabel , "%6.2f" , displayedValue2 ) ;
            }
            else
            {   
	            sprintf ( valueLabel , formatstr , displayedValue2 ) ;      
            }
         }
         else
		    strcpy(valueLabel, "     M");	


         
         int text_width = mGetStringWidth(valueLabel);
         int x_text_shift = text_width + X_SMALL_SPACING;

         mSetColor ( hcl->labelColor ) ; 
         mDrawText ( M_EXPOSE , 0 , 
                     x - x_text_shift,
		             y + Y_DATA2_OFFSET,
		             valueLabel ) ;
      }
   }
   
   /* create and draw the date, hour labels XOR the elevation XOR the paramCode*/    
   if  ( pPcOptions->time )
   {
      /* if value is not missing */
      
      if (! valueMissing)
      {
		 formatTimeLabel(dateLabel, hourLabel, value1Time);
		 mSetColor ( hcl->labelColor ) ;
		 mDrawText ( M_EXPOSE , 0 , x + X_DATE_OFFSET ,
			     y + Y_DATE_OFFSET ,
			     dateLabel ) ;
		 mSetColor ( hcl->labelColor ) ;
		 mDrawText ( M_EXPOSE , 0 , x + X_HOUR_OFFSET ,
			     y + Y_HOUR_OFFSET ,
			     hourLabel ) ;
      }      
   }
   else if (pPcOptions->elevation)
   {
        sprintf(elevationLabel, "%.0lf", pReportList->elevation); 
        
        mSetColor ( hcl->labelColor ) ;
	 	mDrawText ( M_EXPOSE , 0 , x + X_HOUR_OFFSET ,
		     		y + Y_HOUR_OFFSET ,
		     		elevationLabel ) ;
        	
   }
   
   else if (pPcOptions->paramCode)
   {  
        getParamCodeFromReport(pReportList, paramCodeLabel);
         
        mSetColor ( hcl->labelColor ) ;
   
        int yTextShiftAmount = determineYDataShift(pReportList);
        mDrawText ( M_EXPOSE , 0 ,
                       x + X_DATE_OFFSET ,
                       y + yTextShiftAmount ,
                       paramCodeLabel ) ;
   }
   
   return;     
}
/***********************************************************************/
 int  determineYDataShift(ReportList *currentReport)
 {
      //this routine determines if a PC PRECIP report should be 
      //shifted a little higher, so that a PP PRECIP report can be
      // drawn just under the PC precip.  If there is no PP report for this station,
      // then just draw the PC report value (and paramcode) in the normal spot
      // for values and paramcodes
      
      //int yShiftValue = Y_DATA1_OFFSET; //this is the standard Y shift amount for values and paramcodes
      
      int yShiftValue = 0; //this is the standard Y shift amount for values and paramcodes
      
      
      ReportList *nextReport = NULL;
      nextReport = (ReportList*) ListNext( & currentReport->node);
      while (nextReport != NULL)
      {    
          if (strcmp(currentReport->lid, nextReport->lid) == 0) //same lid
          {
              //only consider another report if it is used
              if (nextReport->use) 
              {
                  //if the currentReport and nextReport have the same y_shift value
                  //from the location shifting feature,
                  //we still want to shift due to PE or TS values.
                  //if they have different values, we don't want to shift due to
                  // PE or TS values
                  if (currentReport->y_shift == nextReport->y_shift)
                  {
                      yShiftValue -= Y_LETTER_OFFSET;
                  }
              }

              nextReport = (ReportList*) ListNext( & nextReport->node);
          }
          else //next report is for a different station
          {
              break;  //no need to look for more matching reports for the same station
          }
         
      }
      
      return yShiftValue;
 }
/***********************************************************************/

void formatTimeLabel(char 	*dateString,
		     char 	*hourString,
		     time_t 	timet)
{   
   struct tm 	*tPtr;
   int 		stringlen = 6;
   
   
   /* initialize */   
   
   memset(dateString, '\0', stringlen + 1);
   memset(hourString, '\0', stringlen + 1);
   
      
   tPtr = gmtime(&timet);
   strftime(dateString, stringlen, "%m/%d", tPtr);
   strftime(hourString, stringlen, "%H:%M", tPtr);
   
   
   return;	
}


/***********************************************************************/
void set_PE_formatstr(const char * pe ,
		      char * formatstr )
                      
{
   static char prev_pe[SHEF_PE_LEN + 1] = "\0";
   static char prev_formatstr[12] = "\0";
   
   /* if the same pe is being processed as the last time in this
      function, then use the already determined format */
   if ( strcmp ( pe , prev_pe ) == 0 )
   {
      strcpy(formatstr, prev_formatstr);
   } 
   else
   {
      switch ( pe[0] )
      {
         case 'H' :   /* height data */
      
	    strcpy(formatstr, "%6.2f");
            break ;

         case 'P' :   /* precip, pressure data */

	    strcpy(formatstr, "%6.2f");
            break ;
      
         case 'T' :   /* temperature data */

	    strcpy(formatstr, "%6.0f");
            break ; 

         case 'S' :   /* snow data */

	    if (pe[1] == 'L')
   	       strcpy(formatstr, "%6.2f"); 
	    else
	       strcpy(formatstr, "%6.1f");
      
            break ;

         case 'U' :   /* wind data */

	    if (pe[1] == 'Q')
	       strcpy(formatstr, "%8.4f"); 
	    else
	       strcpy(formatstr, "%6.0f");
      
            break ;

         case 'X' :   /* weather data */

	    strcpy(formatstr, "%5.0f"); 
            break ;

         case 'Q' :   /* Flow / Runoff Data */

            if ( pe [ 1 ] != 'B' )
            {
               strcpy ( formatstr, "%6.0f" ) ;
            }
            else
            {
               strcpy ( formatstr, "%6.2f" ) ;
            }

            break ;

         default:

	    strcpy(formatstr, "%6.2f"); 
            break ;
      }

      /* save for the next time thru for slight performance improvement */
      strcpy(prev_formatstr, formatstr);
      strcpy(prev_pe, pe);
   }
   
   
   return;
}
/*****************************************************************************
 *
 * Routine: draw_weather_station
 *
 * Description: this routine draws weather station
 *
 ****************************************************************************/

void draw_weather_station(int area,int number,int x,int y,char *color)
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( ) ;
  else
    pix = _get_map_pixmap ( ) ;

  _set_foreground ( color ) ;
  _area_draw_fill_circle(pix,x,y,5,5);
}

/*****************************************************************************
 *
 * Routine: draw_river_data_point
 *
 * Description: this routine draws river data point
 *
 ****************************************************************************/
void draw_river_data_point(int area,int number,int x,int y,char *color)
{
  int pos_x ;
  int pos_y ;
  XPoint triangle [ 3 ] ;
         
  _set_foreground ( color ) ;
  pos_x = x + RIVER_GAGE_X ;
  pos_y = y + RIVER_GAGE_Y ;

  triangle[0].x = pos_x ;
  triangle[0].y = pos_y - RIVER_GAGE_SIZE ;
  triangle[1].x = pos_x + RIVER_GAGE_SIZE ;
  triangle[1].y = pos_y + RIVER_GAGE_SIZE ;
  triangle[2].x = pos_x - RIVER_GAGE_SIZE ;
  triangle[2].y = pos_y + RIVER_GAGE_SIZE ;

  mDrawFillPolygon ( area , 0 , triangle , 3 , Convex , CoordModeOrigin ) ;
}

/*****************************************************************************
 *
 * Routine: draw_river_forecast_point
 *
 * Description: this routine draws river forecast point
 *
 ****************************************************************************/

void draw_river_forecast_point(int area,int number,int x,int y,char *color)
{
  int pos_x = 0 ;
  int pos_y = 0;
  unsigned int size = FCST_POINT_SIZE;   
 
  _set_foreground ( color ) ;

  pos_x = x + FCST_POINT_OFFSET_X ;
  pos_y = y + FCST_POINT_OFFSET_Y ;

  mDrawCircle ( area , number , pos_x , pos_y , ( int ) size , ( int ) size ) ;

}

/*****************************************************************************
 *
 * Routine: draw_river_reservoir
 *
 * Description: this routine draws a reservoir symbol
 *
 ****************************************************************************/
void draw_river_reservoir ( int area , int number , int x , int y , 
                             char * color )
{
  int pos_x ;
  int pos_y ;
     
  pos_x = x + RESERVOIR_OFFSET_X ;
  pos_y = y + RESERVOIR_OFFSET_Y ;

  _set_foreground ( color ) ;
  mDrawFillBox ( area , number , pos_x , pos_y , RESERVOIR_WIDTH ,
                 RESERVOIR_HEIGHT ) ;
}

/*****************************************************************************
 *
 * Routine: draw_river_data_point_at_reservoir
 *
 * Description: this routine draws river data point at a reservoir
 *
 ****************************************************************************/

void draw_river_data_point_at_reservoir(int area,int number,int x,int y,
					 char *color)
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( ) ;
  else
    pix = _get_map_pixmap ( ) ;

  draw_river_data_point(area,number,x,y,color);
  _area_draw_box(pix,x-7,y+7,14,5);
}

/*****************************************************************************
 *
 * Routine: draw_river_forecast_point_at_reservoir
 *
 * Description: this routine draws river forecast point at a reservoir
 *
 ****************************************************************************/

void draw_river_forecast_point_at_reservoir(int area,int number,int x,int y,
					     char *color)
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( ) ;
  else
    pix = _get_map_pixmap ( ) ;

  draw_river_forecast_point(area,number,x,y,color);
  _area_draw_box(pix,x-7,y+7,14,5);
}
  
/*****************************************************************************
 *
 * Routine: draw_undefine_station
 *
 * Description: this routine draws undefine station
 *
 ****************************************************************************/

void draw_undefine_station(int area,int number,int x,int y,char *color)
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( )  ;
  else
    pix = _get_map_pixmap ( ) ;

  _set_foreground ( color ) ;
  mSetFont(M_MEDIUM);
  _area_draw_text(pix,x,y,"?");
}

/*****************************************************************************
 *
 * Routine: draw_other_station
 *
 * Description: this routine draws undefine station
 *
 ****************************************************************************/
void draw_other_station ( int area , int number , int x , int y ,
                           char * color )
{
  int pos_x = 0;
  int pos_y = 0 ;
  unsigned int size = GENERAL_STATION_SIZE;
         
  _set_foreground ( color ) ;

  pos_x = x + X_OTHER_OFFSET ;
  pos_y = y + Y_OTHER_OFFSET ;

  mDrawFillCircle ( area , number , pos_x , pos_y , ( int ) size , 
                    ( int ) size ) ;
}



/*****************************************************************************
 *
 * Routine: draw_river_forecast_point_with_weather_station
 *
 * Description: this routine draws river forecast point
 *
 ****************************************************************************/

void draw_river_forecast_point_with_weather_station(int area,int number,
						     int x,int y,
						     char *color)
{
  draw_river_forecast_point(area,number,x,y,color);
  draw_weather_station(area,number,x-2,y-1,"Orange");
  _set_foreground ( color );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

