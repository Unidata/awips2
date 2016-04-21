#ifndef DRAW_STATIONS_H
#define DRAW_STATIONS_H

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "time_defs.h"
#include "HvDisplayControl.h"

#include "HvTime.h"


#define MISSING -9999

#define HIGHLIGHT_GAGE_SIZE 40

#define CLOSE_ENOUGH_IN_PIXELS 50

#define X_LETTER_OFFSET 10
#define Y_LETTER_OFFSET 10

#define X_LEFT_LABEL
#define X_RIGHT_LABEL

#define X_ID_OFFSET 	-5*X_LETTER_OFFSET
#define Y_ID_OFFSET 	1*Y_LETTER_OFFSET

#define X_NAME_OFFSET 	1*X_LETTER_OFFSET
#define Y_NAME_OFFSET 	1*Y_LETTER_OFFSET

#define X_DATA2_OFFSET 	-4*X_LETTER_OFFSET
#define Y_DATA2_OFFSET 	-1*Y_LETTER_OFFSET

#define X_DATA1_OFFSET 	-4*X_LETTER_OFFSET
#define Y_DATA1_OFFSET 	 0*Y_LETTER_OFFSET


#define X_DATE_OFFSET 	 1*X_LETTER_OFFSET
#define Y_DATE_OFFSET 	-1*Y_LETTER_OFFSET

#define X_HOUR_OFFSET 	 1*X_LETTER_OFFSET
#define Y_HOUR_OFFSET 	-0*Y_LETTER_OFFSET

#define X_SMALL_SPACING 5
// constants removed from map_draw.h

static const int RIVER_GAGE_X = 0 ;
static const int RIVER_GAGE_Y = 0 ;

static const unsigned int RESERVOIR_HEIGHT = 2 ;

// --------------------------------------------------------------------------

void drawAllStationDataSets ( ) ;

void drawStationDataSet( ReportList * pReportList , 
                         pc_options_struct * pc_options ,  
			 int is_river_request ,
                         int x , int y ) ;

/* draw actual icons; used by the map display and the legend */

void drawDot ( ReportList * pReportList ,
               pc_options_struct * pPcOptions ,
               int x ,
               int y );
               
void drawIcon ( ReportList * pReportList ,
                pc_options_struct * pPcOptions , 
                int x , int y ) ;

void drawFcstPointStationIcon ( int x , 
			        int y , 
			        char * color ) ;

void drawRiverStationIcon ( int x, 
			    int y, 
			    char * color ) ;

void drawReservoirStationIcon ( int x ,
			        int y , 
			        char * color ) ;

void drawOtherStationIcon ( int x ,
			    int y ,
                            char * color ) ;

/* draws the id and name around the icon */

void drawIconLabels ( ReportList * pReportList ,
                      pc_options_struct * pPcOptions ,
                      int x , int y ) ;
/* draw the data */
void drawRiverStationData ( ReportList *  pReportList ,
                            pc_options_struct * pPcOptions ,
                            int x , int y ) ;

void drawGeneralStationData ( ReportList *  pReportList ,
                              pc_options_struct * pPcOptions ,
                              int x , int y ) ;

char * getRiverStationColor ( ReportList * pReportList ) ;
char * getRiverValueColorForTimeStepMode( ReportList * pReportList,
                                          pc_options_struct *pc_options );

int  determineYDataShift(ReportList *currentReport);

void drawPointLabels(double 		displayedValue1 ,
		     time_t 		value1Time ,
		     char *             value1Color ,
                     int                x ,
                     int                y ,
		     double 		displayedValue2 ,
                     int                display_value2 ,
		     ReportList 	* pReportList ,
                     pc_options_struct * pPcOptions ) ;


void formatTimeLabel(char *dateString, 
		     char *hourString, 
		     time_t timet);

void set_PE_formatstr ( const char * pe , 
		        char * formatstr ) ;
		     
void draw_weather_station(int area,int number,int x,int y,char *color);
void draw_river_data_point(int area,int number,int x,int y,char *color);
void draw_river_forecast_point(int area,int number,int x,int y,char *color);
void draw_river_reservoir ( int area , int number , int x , int y , 
                             char * color );
void draw_river_data_point_at_reservoir(int area,int number,int x,int y,
					 char *color);
					 
void draw_river_forecast_point_at_reservoir(int area,int number,int x,int y,
					     char *color);                             
                             
void draw_undefine_station(int area,int number,int x,int y,char *color);

void draw_other_station ( int area , int number , int x , int y ,
                           char * color );
                           
void draw_river_forecast_point_with_weather_station(int area,int number,
						     int x,int y,
						     char *color);
#endif
