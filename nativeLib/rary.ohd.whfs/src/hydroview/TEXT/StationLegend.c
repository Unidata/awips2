#include "gui_builder.h"
#include "map.h"
#include "map_library.h"
#include "StationLegend.h"
#include "drawStations.h" //for the icon drawing

/**********************************************************************/

void startStationLegend ( Widget parent , GenericLegendDialog *dialog )
{
   
   Dimension height = ( Dimension ) HV_LEGEND_HEIGHT ;
   Dimension width  = ( Dimension ) HV_LEGEND_WIDTH ;
   
   initGenericLegendDialog ( dialog,
			     GetTopShell ( parent ) ,
			     "Station Legend",
			     height, width,
			     drawStationLegend,
			     (void *) NULL ) ;
   
   return;	
}


/*************************************************************************/

void drawStationLegend(struct _GenericLegendDialog *dialog)
{
   HvColorList *hcl = getHvColorList();
   char * color;
   
   Position center_x;
   Position left_x;
   Position right_x;
   Position x;
   Position y;
   Position top_y_offset = 30;
   Position secondHalfBeginY = 330;
   Position label_y_offset = 5;
   
   Position labelSeparation = 20;
   Position exampleBeginY = top_y_offset + 50;
   Position exampleSeparation = 30;
   
   char label[BUFSIZ];

   mSetUserPixmap ( dialog->pixmap ) ;
   
   /* Clear the drawing area with black */
   
   mSetColor ( "Black" ) ; 
   mDrawFillBox ( M_USER , 0 , 0 , 0 , dialog->daWidth , dialog->daHeight ) ;
   
   /* set the x related variables */
   
   center_x = dialog->daWidth / 2;
   left_x = 20;
   right_x = 50;
   
   
   /*************************  top half left      ********************/
   
   
   
   /* draw section label */
   
   y = top_y_offset;
   x = left_x;
   sprintf(label, "Station Icons");
   mSetColor ( "white" ) ;

   mDrawText ( M_USER , 0 , x , y , label ) ;
   
   /* draw River Data Point icon and label */
   
   y = exampleBeginY;
   x = left_x;
   color = hcl->nonFloodColor;
   
   mSetColor ( color ) ;

   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   sprintf(label, "River Data Point");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;

   /* draw River Forecast Point icon and label */
   
   y += exampleSeparation;
   
   draw_river_forecast_point(M_USER , 0 , x , y , color);
 // mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;
 
   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   sprintf(label, "River Forecast Point");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;
   
   /* draw Reservoir Data Point icon and label */
   
   y += exampleSeparation;
   
   draw_river_reservoir(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RESERVOIR_POINT ) ;
   
   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   sprintf(label, "Reservoir Data Point");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;

   /* draw Reservoir Forecast Point icon and label */
   y += exampleSeparation;
   
   draw_river_reservoir(M_USER , 0 , x , y , color);
  // mDrawSymbol ( M_USER , 0 , x , y , color , M_RESERVOIR_POINT ) ;
  
   draw_river_forecast_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;
   
   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   sprintf(label, "Reservoir Forecast Point");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;
   
   
   /* draw Meteorological Station icon and label */
   /* August 11, 2004 - Added setting for red color. */
   color = hcl->otherStationColor;
   y += exampleSeparation;
   
   draw_other_station(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_OTHER_STATION ) ;
   
   sprintf(label, "Meteorological Station");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;
   
   /* draw River Forecast Point with met station icon and label */
   /* August 11, 2004 - Added setting of color to green here and also
                        the setting of the color for the met symbol to red. */
   color = hcl->nonFloodColor;
   y += exampleSeparation;
   
   
   
   
   draw_river_forecast_point(M_USER , 0 , x , y , color);
  // mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;
   
   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   color = hcl->otherStationColor;
   
   draw_other_station(M_USER , 0 , x , y , color);  
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_OTHER_STATION ) ;
   
   
   sprintf(label, "River Forecast Point with Meteorological Station");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;
   
   /*************************  top half right       ********************/
   
   
   /* draw section label */
   
   y = top_y_offset;
   x = center_x;
   sprintf(label, "River/Reservoir Station Colors");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x , y , label ) ;
   
   /* draw a River Forecast Point with missing data */
   y = exampleBeginY;
   x = center_x;
   
   color = hcl->noDataColor;
   
   draw_river_data_point(M_USER , 0 , x , y , color);
  // mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
  
   draw_river_forecast_point(M_USER , 0 , x , y , color); 
  // mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;
  
   sprintf(label, "River Forecast Point with missing data");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;
   
   
   /* draw a River Forecast Point above flood stage */
   
   y += exampleSeparation;
   color = hcl->floodColor;
   
   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   draw_river_forecast_point(M_USER , 0 , x , y , color); 
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;
   
   sprintf(label, "River Forecast Point above flood stage");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;
   
   /* draw a River Forecast Point above action stage */
   
   y += exampleSeparation;
   
   color = hcl->nearFloodColor;
   
   draw_river_data_point(M_USER , 0 , x , y , color);
  // mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   draw_river_forecast_point(M_USER , 0 , x , y , color); 
  // mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;
   
   sprintf(label, "River Forecast Point above action stage");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;
   
   /* draw a River Forecast Point below action stage */
   y += exampleSeparation;
   color = hcl->nonFloodColor;
   
   
   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   draw_river_forecast_point(M_USER , 0 , x , y , color);  
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;
   
   sprintf(label, "River Forecast Point below action stage");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;

   /* draw a River Forecast Point which is missing the flood and action
      stage information but has an observed of forecast stage (or flow). */
   y += exampleSeparation ;
   color = hcl->noStageColor ;


   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   draw_river_forecast_point(M_USER , 0 , x , y , color); 
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;

   sprintf(label, "River Forecast Point missing stage data");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , x + labelSeparation , y + label_y_offset , 
               label ) ;

   /*************************  bottom  half       ********************/
   
   
   /* draw section label */
   
   y = secondHalfBeginY;
   
   sprintf(label, "Station Data");
   mSetColor ( "white" ) ;
   mDrawText ( M_USER , 0 , center_x - 30 , y , 
               label ) ;
   
   /* draw data example */
   
   x = center_x;
   
   y += exampleSeparation * 2;
   
   color = hcl->nonFloodColor;
  /*
    * 
    *   draw_river_reservoir(M_USER , 0 , x , y , color);
   
   draw_river_forecast_point(M_USER , 0 , x , y , color);
  
   draw_river_data_point(M_USER , 0 , x , y , color);
   draw_other_station(M_USER , 0 , x , y , color);
    * 
    * */
   
   draw_river_forecast_point(M_USER , 0 , x , y , color);    
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_FORECAST_POINT ) ;
   
   draw_river_data_point(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_RIVER_DATA_POINT ) ;
   
   draw_other_station(M_USER , 0 , x , y , color);
   //mDrawSymbol ( M_USER , 0 , x , y , color , M_OTHER_STATION ) ;
   
   mSetColor ( "white" ) ;

   /* draw value label */
   sprintf(label, "Value");
   mDrawText ( M_USER , 0 , x + X_DATA1_OFFSET , y + Y_DATA1_OFFSET , 
               label ) ;
   
   
   /* draw value2 label */
   
   sprintf(label, "[Fld Stg/Flow]");
   mDrawText ( M_USER , 0 , x + X_DATA2_OFFSET - 50 , y + Y_DATA2_OFFSET , 
               label ) ;
   
   /* draw date label */
   sprintf(label, "MM/DD");
   mDrawText ( M_USER , 0 , x + X_DATE_OFFSET , y + Y_DATE_OFFSET , 
               label ) ;
   
   
   /* draw hour label */
   
   sprintf(label, "hh:mm");
   mDrawText ( M_USER , 0 , x + X_HOUR_OFFSET , y + Y_HOUR_OFFSET , 
               label ) ;
   
   
   /* draw id label */
   
   sprintf(label, "Id");
   mDrawText ( M_USER , 0 , x + X_ID_OFFSET , y + Y_ID_OFFSET , 
               label ) ;
   
   
   /* draw name label */
   
   sprintf(label, "Name");
   mDrawText ( M_USER , 0 , x + X_NAME_OFFSET , y + Y_NAME_OFFSET , 
               label ) ;
   
   return;     
}

/*************************************************************************/

