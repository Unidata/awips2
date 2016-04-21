#include <unistd.h> 
#include "HvDisplayControlProto.h"
#include "HvColorList.h"

#include "ColorThreshold.h"
#include "NamedColorSetGroup.h"
#include "NamedColorUseSet.h"
#include "color_threshold_show.h" //for getApplicationName();

#include "map.h"
#include "pointcontrol_options.h"

#include "get_colorvalues.h"
#include "hv_color_threshold.h"

#include "write_colorsets.h"


#define num_height_colors 5
#define num_precip_colors  17
#define num_fs_departure_colors  12
#define num_percent_flood_flow_colors  11

#define num_swe_colors  13
#define num_swe_change_colors  15

#define num_temperature_colors  13
#define num_temperature_change_colors  13

#define num_dewpoint_colors  13
#define num_dewpoint_change_colors  13
#define num_relative_humidity_colors 13

#define num_wind_speed_colors 13
#define num_wind_direction_colors 10



//height
// this one is not actually used, but is used for code symmetry (and not breaking things)
static const char * height_colors [ num_height_colors ] =
              { "GRAY30" , "DARKGREEN" ,
                 "GREEN" , "YELLOW" , "RED" 
              };

static const double height_levels [ num_height_colors ] =
              { -9999.0 , -8888.0 , 
                0.00 , 10.0 , 20.0 
               } ;


//precip totals
static const char * precip_colors [ num_precip_colors ] =
              { "GRAY30" , "GRAY40" , "GRAY40" , "BLUE" , "CYAN" ,
                "DARKGREEN" , "GREEN" , "GREENYELLOW" , "YELLOW" ,
                "GOLD2" , "DARKORANGE1" , "RED" , "RED3" , "RED4" ,
                "MAGENTA1" , "DARKORCHID" , "WHITE" } ;

static const double precip_levels [ num_precip_colors ] =
              { -9999.0 , -8888.0 , 0.00 , 0.01 , 0.10 , 0.20 , 0.30 ,
                0.40 , 0.50 , 0.75 , 1.00 , 1.25 , 1.50 , 1.75 , 2.00 ,
                2.50 , 3.00 } ;




//flood stage departure
static const char * fs_departure_colors [ num_fs_departure_colors ] =
              { "GRAY30" , "GREEN" , 
                "GREEN" , "GREENYELLOW" , "YELLOW",
                "ORANGE" ,
                "DARKORANGE1" , "RED" , "RED3" , "RED4" ,
                "MAGENTA1" ,  "WHITE" } ;

static const double fs_departure_levels [ num_fs_departure_colors ] =
              { -9999.0 , -8888.0 ,
                -5.0, -2.5,  -1.00 , 
                0.0, 
                 1.0 , 2.0 , 3.0 , 4.0 ,
                 5.0 , 10.0 } ;



// percent flood flow
static const char * percent_flood_flow_colors [ num_percent_flood_flow_colors ] =
              { "GRAY30" , "GREEN" , 
                "GREEN" , "GREENYELLOW" , "YELLOW",
                "ORANGE" , "DARKORANGE1" ,
                "RED" , "RED3" , "RED4" ,"MAGENTA1" } ;

static const double percent_flood_flow_levels [ num_percent_flood_flow_colors ] =
              { -9999.0 , -8888.0 ,
                10.0, 50.0 , 75.0, 
                90.0 , 95.0, 
                100.0 , 105.0, 110.0, 120.0 } ;


// swe
static const char * swe_colors [ num_swe_colors ] =
              {
                "GRAY30" , "GRAY40" ,
                "GRAY40" , "BLUE" , "CYAN" ,
                "DARKGREEN" , "GREEN" , "GREENYELLOW" , 
                "YELLOW" ,  "GOLD2" , "DARKORANGE1" , 
                "RED" , "WHITE" } ;

static const double swe_levels [ num_swe_colors ] =
              { -9999.0 , -8888.0 ,
                0.0, 1.0 , 2.0, 
                3.0 , 4.0, 5.0,
                6.0, 7.0, 8.0 , 
                9.0, 10.}; 
       

// swe_change
static const char * swe_change_colors [ num_swe_change_colors ] =
             { "GRAY30" , "GRAY40" ,
               "GRAY40" , "BLUE" , "CYAN" , "DARKGREEN" , "GREEN" ,
               "GREENYELLOW" , "YELLOW" ,  "GOLD2" , "DARKORANGE1" ,
               "RED" , "MAGENTA1" , "DARKORCHID" , "WHITE" } ;


static const double swe_change_levels [ num_swe_change_colors ] =
              { 
                 -9999.0 , -8888.0 ,
                0.0, 0.25, 0.5, .75,  1.0 ,
                1.5, 2.0 , 2.5, 3.0,
                3.5, 4.0, 4.5 , 5.0 
              }; 
 
// temperature
static const char * temperature_colors [ num_temperature_colors ] =
             { 
               "GRAY30" , "GRAY40" ,
               "GRAY40" , "BLUE" , "CYAN" , "DARKGREEN" , "GREEN" ,
               "GREENYELLOW" , "YELLOW" ,  "GOLD2" , "DARKORANGE1" ,  "RED" , 
               "MAGENTA1"
             } ;


static const double temperature_levels [ num_temperature_colors ] =
             { 
                -9999.0 , -8888.0 ,
                0.0, 10.0, 20.0, 30.0,  40.0 ,
                50.0, 60.0, 70.0 , 80.0, 90.0,
                100.0
             };       


// temperature change
static const char * temperature_change_colors [ num_temperature_change_colors ] =
             { 
               "GRAY30" , "GRAY40" ,
               "GRAY40" , "BLUE" , "CYAN" , "DARKGREEN" , "GREEN" ,
               "GREENYELLOW" , "YELLOW" ,  "GOLD2" , "DARKORANGE1" ,  "RED" , 
               "MAGENTA1"
             } ;


static const double temperature_change_levels [ num_temperature_change_colors ] =
             { 
                -9999.0 , -8888.0 ,
                -20, -15, -10, -7.5, -5, 
                0.0,
                5, 7.5, 10.0 , 15, 20
             };   


// dewpoint
static const char * dewpoint_colors [ num_dewpoint_colors ] =
             { 
               "GRAY30" , "GRAY40" ,
               "GRAY40" , "BLUE" , "CYAN" , "DARKGREEN" , "GREEN" ,
               "GREENYELLOW" , "YELLOW" ,  "GOLD2" , "DARKORANGE1" ,  "RED" , 
               "MAGENTA1"
             } ;


static const double dewpoint_levels [ num_dewpoint_colors ] =
             { 
                -9999.0 , -8888.0 ,
                0.0, 10.0, 20.0, 30.0,  40.0 ,
                50.0, 60.0, 70.0 , 80.0, 90.0,
                100.0
             };         


// dewpoint change
static const char * dewpoint_change_colors [ num_dewpoint_change_colors ] =
             { 
               "GRAY30" , "GRAY40" ,
               "GRAY40" , "BLUE" , "CYAN" , "DARKGREEN" , "GREEN" ,
               "GREENYELLOW" , "YELLOW" ,  "GOLD2" , "DARKORANGE1" ,  "RED" , 
               "MAGENTA1"
             } ;


static const double dewpoint_change_levels [ num_dewpoint_change_colors ] =
             { 
                -9999.0 , -8888.0 ,
                -20, -15, -10, -7.5, -5, 
                0.0,
                5, 7.5, 10.0 , 15, 20
             };   



// relative humidity
static const char * relative_humidity_colors [ num_relative_humidity_colors ] =
             { 
               "GRAY30" , "GRAY40" ,
               "GRAY40" , "BLUE" , "CYAN" , "DARKGREEN" , "GREEN" ,
               "GREENYELLOW" , "YELLOW" ,  "GOLD2" , "DARKORANGE1" ,  "RED" , 
               "MAGENTA1"
             } ;

static const double relative_humidity_levels [ num_relative_humidity_colors ] =
             { 
                -9999.0 , -8888.0 ,
                0.0, 10.0, 20.0, 30.0,  40.0 ,
                50.0, 60.0, 70.0 , 80.0, 90.0,
                100.0
             };       


//wind speed
static const char * wind_speed_colors [ num_wind_speed_colors ] =
             { 
               "GRAY30" , "GRAY40" ,
               "GRAY40" , "BLUE" , "CYAN" , "DARKGREEN" , "GREEN" ,
               "GREENYELLOW" , "YELLOW" ,  "GOLD2" , "DARKORANGE1" ,  "RED" , 
               "MAGENTA1"
             } ;


static const double wind_speed_levels [ num_wind_speed_colors ] =
             { 
                -9999.0 , -8888.0 ,
                0.0, 10.0, 20.0, 30.0,  40.0 ,
                50.0, 60.0, 70.0 , 80.0, 90.0,
                100.0
             };       
             
//wind direction
static const char * wind_direction_colors [ num_wind_direction_colors ] =
             { 
               "GRAY30" , "YELLOW" ,
               "YELLOW" ,
               "ORANGE" ,
               "RED" ,
               "PURPLE" ,
               "BLUE" ,
               "SEAGREEN" ,
               "GREEN" ,
               "YELLOWGREEN" 
             } ;


static const double wind_direction_levels [ num_wind_direction_colors ] =
             { 
                -9999.0 , -8888.0 ,
                45, 
                90, 
                135,
                180,
                225,
                270,
                325,
                360
             };       

// -------------------------------------------------------------------------------------

void writeHydroViewDefaultColorDataFile( char * outputFileName )
{
    FILE *outputFilePtr = NULL;
 
    outputFilePtr = fopen( outputFileName, "w" );
   
    if ( outputFilePtr == NULL )
    {
        fprintf( stderr, "Unable to open the output file %s for writing!", outputFileName );
        exit ( -1 );
    }
 
    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Height",
                                   "HEIGHT",
                                   height_colors, height_levels, num_height_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Precipitation",
                                   "PRECIP",
                                   precip_colors, precip_levels, num_precip_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Flood Stage Departure",
                                   "FS_DEPARTURE",
                                   fs_departure_colors, fs_departure_levels, num_fs_departure_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Percent Flood Flow",
                                   "PERCENT_FLOOD_Q",
                                   percent_flood_flow_colors, percent_flood_flow_levels, num_percent_flood_flow_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Snow Water Equiv",
                                   "SWE",
                                   swe_colors, swe_levels, num_swe_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "SWE Change",
                                   "SWE-CHANGE",
                                   swe_change_colors, swe_change_levels, num_swe_change_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Temperature",
                                   "TEMPERATURE",
                                   temperature_colors, temperature_levels, num_temperature_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Temp Change",
                                   "TEMP-CHANGE",
                                   temperature_change_colors, temperature_change_levels, num_temperature_change_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Dewpoint",
                                   "DEWPOINT",
                                   dewpoint_colors, dewpoint_levels, num_dewpoint_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Dewpoint Change",
                                   "DEWPOINT-CHANGE",
                                   dewpoint_change_colors, dewpoint_change_levels, num_dewpoint_change_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Relative Humidity",
                                   "REL-HUMIDITY",
                                   relative_humidity_colors, relative_humidity_levels, num_relative_humidity_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Wind Speed",
                                   "WIND-SPEED",
                                   wind_speed_colors, wind_speed_levels, num_wind_speed_colors );

    writeColorDataSetToOutputFile( outputFilePtr,
                                   "Wind Direction",
                                   "WIND-DIRECTION",
                                   wind_direction_colors, wind_direction_levels, num_wind_direction_colors );

    fclose ( outputFilePtr );
    outputFilePtr = NULL;


}

/*
void writeColorDataSetToOutputFile( FILE * outputFilePtr, 
                                    const char * colorUseDisplayName, 
                                    const char * colorUseDatabaseName, 
                                    const char * colorNames[], const double scaleValues[],
                                    int numOfValues )
{
    int i = 0;

    fprintf( outputFilePtr, "%s|%s|%d\n", colorUseDisplayName, colorUseDatabaseName, numOfValues );
    for ( i = 0; i < numOfValues; i++ )
    {
        fprintf( outputFilePtr, "%s %1.2f\n", colorNames[ i ], scaleValues[ i ] );
    }
    fprintf( outputFilePtr, "\n" ); 
}
*/

void loadColorThresholdArray(ColorThresholdArray *colorArray,
                 char *colorUseName,
                 time_t duration,
                 const NamedColorSetGroup *defaultNamedColorSetGroupPtr,
                 Widget widget)
{
    char header[] = "loadColorThresholdArray(): ";
         
    int numColors = 0;
    int numLevels = 0;
    int index = 0;
    int i = 0;
    int count = 0;
    long size = 0;
    int allocated = 0;
    
    char * userId = getlogin();
    const char * applicationName = getApplicationName();
      
    printf("%s  userId = :%s: applicationName = :%s: \n", header, userId, applicationName);                                   
     
      
      
    ColorValue *head =  get_colorvalues ( 
                                         userId,
                                         applicationName, 
                                         colorUseName,
                                         duration , 
                                         'E',
                                         &numColors ,
                                         &numLevels,
                                         defaultNamedColorSetGroupPtr
                                        );
                                        
                                        
                               
      ColorValue *ptr = NULL;
      
      if (head)
      {
         ptr = (ColorValue *) ListFirst(&head->list);
         count = ListCount(&head->list);
      
      /*
           the first two entries in the table are for the
           missing and default (< minimum threshold) colors
      */
      
         colorArray->length = count - 2;
         size = colorArray->length * sizeof(ColorThreshold);
         colorArray->thresholds = (ColorThreshold *) malloc(size);
      
         if (colorArray->thresholds)
         {
              allocated = 1;
         }
         else
         {
              allocated = 0;
         }
      
      
         for(i=0; ptr != NULL; i++) 
         {
             if ( i == 0)
             {
                 strcpy(colorArray->missingColorName, ptr->color_name);
             }
               
             else if (i == 1)
             {
                 strcpy(colorArray->defaultColorName, ptr->color_name);
             }
               
            else /* a regular threshold */
            {    
                index = i - 2;
                if (allocated)
                {
                    strcpy(colorArray->thresholds[index].colorName,
                           ptr->color_name);
                 
                    colorArray->thresholds[index].value =
                           ptr->threshold_value;                 
                }
              
            }
                   
            ptr = (ColorValue *) ListNext(&ptr->node);
        }
        
        FreeColorValue(head);
    }                                         
    return;
}

// ---------------------------------------------------------------------------
NamedColorSetGroup * get_default_hv_colors ( )
{
    char header[] = "get_default_hv_colors(): ";
    static NamedColorSetGroup * pColorSetGroup = NULL;
    NamedColorUseSet * pColorUseSet = NULL;
    int durationInSeconds = 0;
    int hours24Duration = 24 * SECONDS_PER_HOUR;
     
     
    printf("%s  inside function \n", header);
     
    //this routine ends up being used in the color threshold editing window
    //At one point, we realloc'ed every time and it messed up the remembered
    //default color sets in the color threshold editing window, causing a segmentation
    //violation
    if (pColorSetGroup == NULL)
    {
     /* Create a color use group for each of the PDC Time-Step Mode elements */

        //  "HEIGHT
        pColorUseSet = initializeNamedColorUseSet ( "HEIGHT", "Height",
                                               num_height_colors,
                                               height_levels,
                                               height_colors,
                                               "GRAY30", "GREEN",
                                               durationInSeconds );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
       
        
        
        //  "HEIGHT_ABOVE_FS"
        pColorUseSet = initializeNamedColorUseSet ( "FS_DEPARTURE", "Depth Above Flood Stage",
                                               num_fs_departure_colors,
                                               fs_departure_levels,
                                               fs_departure_colors,
                                               "GRAY30", "GREEN",
                                               durationInSeconds );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
    
    
        //  "PERCENT_FLOOD_FLOW"
        
        pColorUseSet = initializeNamedColorUseSet ( "PERCENT_FLOOD_Q", "Percent Flood Flow",
                                               num_percent_flood_flow_colors,
                                               percent_flood_flow_levels,
                                               percent_flood_flow_colors,
                                               "GRAY30", "GREEN",
                                               durationInSeconds );
                                               
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );


        // PRECIP
        pColorUseSet = initializeNamedColorUseSet ( "PRECIP", "Precip",
                                               num_precip_colors,
                                               precip_levels,
                                               precip_colors,
                                               "GRAY30", "GRAY40",
                                               durationInSeconds);
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
 
 
 
        // swe
        pColorUseSet = initializeNamedColorUseSet ( "SWE", "Snow-Water Equivalent",
                                               num_swe_colors,
                                               swe_levels,
                                               swe_colors,
                                               "GRAY30", "GRAY40",
                                               durationInSeconds );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );


        // swe-change
        pColorUseSet = initializeNamedColorUseSet ( "SWE-CHANGE", "SWE Change",
                                               num_swe_change_colors,
                                               swe_change_levels,
                                               swe_change_colors,
                                               "GRAY30", "GRAY40",
                                               hours24Duration );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
        
        
        // temperature
        pColorUseSet = initializeNamedColorUseSet ( "TEMPERATURE", "Temperature",
                                               num_temperature_colors,
                                               temperature_levels,
                                               temperature_colors,
                                               "GRAY30", "GRAY40",
                                               durationInSeconds );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
        
        
       // temperature-change
        pColorUseSet = initializeNamedColorUseSet ( "TEMP-CHANGE", "Temperature Change", 
                                               num_temperature_change_colors,
                                               temperature_change_levels,
                                               temperature_change_colors,
                                               "GRAY30", "GRAY40",
                                               hours24Duration );
                                               
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );


        // dewpoint
        pColorUseSet = initializeNamedColorUseSet ( "DEWPOINT", "Dewpoint",
                                               num_dewpoint_colors,
                                               dewpoint_levels,
                                               dewpoint_colors,
                                               "GRAY30", "GRAY40",
                                               durationInSeconds );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
        
        
         // dewpoint change
        pColorUseSet = initializeNamedColorUseSet ( "DEWPOINT-CHANGE", "Dewpoint Change",
                                               num_dewpoint_change_colors,
                                               dewpoint_change_levels,
                                               dewpoint_change_colors,
                                               "GRAY30", "GRAY40",
                                               hours24Duration );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
        
        
        //relative humidity
        pColorUseSet = initializeNamedColorUseSet ( "REL-HUMIDITY", "Relative Humidity",
                                               num_relative_humidity_colors,
                                               relative_humidity_levels,
                                               relative_humidity_colors,
                                               "GRAY30", "GRAY40",
                                               durationInSeconds );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
        
        
        
        // wind speed
        pColorUseSet = initializeNamedColorUseSet ( "WIND-SPEED", "Wind Speed",
                                               num_wind_speed_colors,
                                               wind_speed_levels,
                                               wind_speed_colors,
                                               "GRAY30", "GRAY40",
                                               durationInSeconds );
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
        
        
        // wind direction
        pColorUseSet = initializeNamedColorUseSet ( "WIND-DIRECTION", "Wind Direction",
                                               num_wind_direction_colors,
                                               wind_direction_levels,
                                               wind_direction_colors,
                                               "GRAY30", "YELLOW" ,
                                               durationInSeconds);
                                               
        pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
        
    }


   return pColorSetGroup;
}

// ---------------------------------------------------------------------------
void loadColorThresholdArrayByPcOptions(const pc_options_struct *pc_options )
{
    char header[] = "loadColorThresholdArrayByPcOptions(): ";
     
    static enum TimeStepDataElement previousElement = TIME_STEP_DATA_ELEMENT_COUNT;
    static enum InstPrecipSelection previousInstSelection = PRECIP_TIME_COUNT;
       
    char * color_use_name = NULL;
         
    char ELEMENT_USE_ARRAY[TIME_STEP_DATA_ELEMENT_COUNT][COLOR_USE_NAME_LENGTH + 1] = 
    {
        //RIVER
        "HEIGHT", //STAGE_POOL_TSDE,
        "HEIGHT", //FLOW_STORAGE_TSDE,
        "FS_DEPARTURE", //DEPTH_ABOVE_FS_TSDE,
        "PERCENT_FLOOD_Q", //PERCENT_FLOOD_FLOW_TSDE,
   
        // RAIN  
        "PRECIP",//INSTANTANEOUS_PRECIP_TSDE,
        "PRECIP", //HOURLY_PRECIP_TSDE,
        "PRECIP", //THREE_HOUR_PRECIP_TSDE,
        "PRECIP", //SIX_HOUR_PRECIP_TSDE,
        "PRECIP", //DAILY_PRECIP_TSDE,
   
        // SNOW 
        "SWE", //SNOW_WATER_EQUIV_TSDE,
        "SWE-CHANGE", //SWE_24_HOUR_CHANGE_TSDE,
   
        // TEMPERATURE
        "TEMPERATURE",  //TEMPERATURE_TSDE,
        "TEMP-CHANGE", //TEMP_24_HOUR_CHANGE_TSDE,
        "TEMPERATURE",  //TEMP_MAX_TSDE,
        "TEMPERATURE", //TEMP_MIN_TSDE,
   
        // HUMIDITY 
        "DEWPOINT", //DEWPOINT_TSDE,
        "DEWPOINT-CHANGE",// DEWPOINT_24_HOUR_CHANGE_TSDE,
        "REL-HUMIDITY",  //RELATIVE_HUMIDITY_TSDE,
   
        // WIND
        "WIND-SPEED", //WIND_SPEED_TSDE,
        "WIND-DIRECTION" //WIND_DIRECTION_TSDE,
    };
    
    
    int ELEMENT_DURATION_ARRAY[TIME_STEP_DATA_ELEMENT_COUNT] = 
    {
        //RIVER
        0, //STAGE_POOL_TSDE,
        0, //FLOW_STORAGE_TSDE,
        0, //DEPTH_ABOVE_FS_TSDE,
        0, //PERCENT_FLOOD_FLOW_TSDE,
   
        // RAIN  
        1,//INSTANTANEOUS_PRECIP_TSDE,
        1,//HOURLY_PRECIP_TSDE,
        3,//THREE_HOUR_PRECIP_TSDE,
        6,//SIX_HOUR_PRECIP_TSDE,
        24, //DAILY_PRECIP_TSDE,
   
        // SNOW 
        0, //SNOW_WATER_EQUIV_TSDE,
        24, //SWE_24_HOUR_CHANGE_TSDE,
   
        // TEMPERATURE
        0,  //TEMPERATURE_TSDE,
        24, //TEMP_24_HOUR_CHANGE_TSDE,
        0,  //TEMP_MAX_TSDE,
        0, //TEMP_MIN_TSDE,
   
        // HUMIDITY 
        0, //DEWPOINT_TSDE,
        24,// DEWPOINT_24_HOUR_CHANGE_TSDE,
        0,  //RELATIVE_HUMIDITY_TSDE,
   
        // WIND
        0, //WIND_SPEED_TSDE,
        0, //WIND_DIRECTION_TSDE,
    };
    
     
    const int instantaneous_minutes_array [PRECIP_TIME_COUNT] = 
                   {30, 60, 120, 180, 240, 360, 720, 1080, 1440 };
                   
    int duration_in_hours;
    int minutes = 0;
    int duration_in_seconds = 0;
    int needToReloadColorThresholds = 0;  
     
    TimeStepDataElement selectedTimeStepElement =  pc_options->selectedTimeStepElement;
    TimeStepDataElementType element_type = (TimeStepDataElementType)  pc_options->element_type;
    InstPrecipSelection instPrecipSelection =  pc_options->inst_precip_accum_time_selection;
    
     
    //determine if the color thresholds need to be reloaded   
    if  (previousElement != selectedTimeStepElement ) 
    {
        needToReloadColorThresholds = 1;    
    }
  
    else if ((selectedTimeStepElement == INSTANTANEOUS_PRECIP_TSDE) &&
            (previousInstSelection != instPrecipSelection  ) )
    {
        needToReloadColorThresholds = 1;    
    }  
    
    //hardcode this so that it always reloads
    needToReloadColorThresholds = 1;
    
      // load the ColorThresholdArray, if needed
    if (needToReloadColorThresholds  )  
    {
        printf("%s loading color thresholds \n", header);
        
        previousElement = selectedTimeStepElement;
        previousInstSelection = instPrecipSelection;
      
      
        printf("%s selectedTimeStepElement = %d \n", 
                header, selectedTimeStepElement);
      
        color_use_name = ELEMENT_USE_ARRAY[selectedTimeStepElement];
        duration_in_hours = ELEMENT_DURATION_ARRAY[selectedTimeStepElement];
        
        duration_in_seconds = duration_in_hours * SECONDS_PER_HOUR;
       
        //  RAIN_TIME_STEP_TYPE      
        if (element_type == RAIN_TIME_STEP_TYPE)
        {
            if (selectedTimeStepElement == INSTANTANEOUS_PRECIP_TSDE)
            {
                minutes = instantaneous_minutes_array[instPrecipSelection];
                duration_in_seconds =  minutes * SECONDS_PER_MINUTE;    
            }
        }   
  
    
        Widget map_widget =  _get_map_widget(0);
        HvColorList *hcl = getHvColorList();
     
        NamedColorSetGroup *default_color_set_groupPtr = get_default_hv_colors();
    
        loadColorThresholdArray(& hcl->color_threshold_array,
                 color_use_name, duration_in_seconds,
                  default_color_set_groupPtr,
                  map_widget) ;
                  
    } //end if needToReloadColorThresholds
      

    return;

} //end loadColorThresholdArrayByPcOptions
// -----------------------------------------------------------------------

// -----------------------------------------------------------------------


char * determineValueColor(double value,  const pc_options_struct * pc_options)
{
    
      HvColorList  *hcl = getHvColorList();
      char * color_name = NULL;
        
   
      color_name = (char *)  determineColorByThreshold(value,
                                              MISSING_VAL,
                                              (const ColorThresholdArray *) & hcl->color_threshold_array );
      
      return color_name;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/whfs_lib/src/HvAreal/RCS/hv_color_threshold.c,v $";
 static char rcs_id2[] = "$Id: hv_color_threshold.c,v 1.6 2007/07/17 20:07:20 gsood Exp $";}
/*  ===================================================  */

}
/***********************************************************************/
