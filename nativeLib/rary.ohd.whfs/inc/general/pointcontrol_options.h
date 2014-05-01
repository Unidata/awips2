#ifndef POINTCONTROL_OPTIONS_H
#define POINTCONTROL_OPTIONS_H


#include <time.h>

#include "DbmsDefs.h"

//#define DISABLED_TIME_STEP 0

/* definitions */

#define MAXLEN_TIME 30
#define MAXLEN_HOUR 30
#define MAXLEN_AREALIST 150


/* possible values for data type */ 


typedef enum AdHocDataElementType
{
   RIVER_AD_HOC_TYPE,
   RAIN_AD_HOC_TYPE,
   SNOW_AD_HOC_TYPE,
   TEMP_AD_HOC_TYPE,
   OTHER_AD_HOC_TYPE
	
} AdHocDataElementType;


typedef enum TimeStepDataElementType
{
   RIVER_TIME_STEP_TYPE,
   RAIN_TIME_STEP_TYPE,
   SNOW_TIME_STEP_TYPE,
   TEMPERATURE_TIME_STEP_TYPE,
   HUMIDITY_TIME_STEP_TYPE,
   WIND_TIME_STEP_TYPE,
   TIME_STEP_TYPE_COUNT
} TimeStepDataElementType;


typedef enum RiverStationFilter
{
    ALL_STATIONS_RSF,	
	STREAM_STATIONS_RSF,
	RESERVOIR_STATIONS_RSF
} RiverStationFilter;

typedef enum PrecipPeFilter
{
    PC_AND_PP_PPF,   
    PC_ONLY_PPF,
    PP_ONLY_PPF
} PrecipPeFilter;

/*const char  TimeStepDataElementTypeStrings [BUFSIZ][TIME_STEP_TYPE_COUNT] = {   };
*/

typedef enum TimeStepDataElement
{
   // RIVER 	
   STAGE_POOL_TSDE,
   FLOW_STORAGE_TSDE,
   DEPTH_ABOVE_FS_TSDE,
   PERCENT_FLOOD_FLOW_TSDE,
   
   // RAIN  
   INSTANTANEOUS_PRECIP_TSDE,
   HOURLY_PRECIP_TSDE,
   THREE_HOUR_PRECIP_TSDE,
   SIX_HOUR_PRECIP_TSDE,
   DAILY_PRECIP_TSDE,
   
   // SNOW 
   SNOW_WATER_EQUIV_TSDE,
   SWE_24_HOUR_CHANGE_TSDE,
   
   // TEMPERATURE
   TEMPERATURE_TSDE,
   TEMP_24_HOUR_CHANGE_TSDE,
   TEMP_MAX_TSDE,
   TEMP_MIN_TSDE,
   
   // HUMIDITY 
   DEWPOINT_TSDE,
   DEWPOINT_24_HOUR_CHANGE_TSDE,
   RELATIVE_HUMIDITY_TSDE,
   
   // WIND
   WIND_SPEED_TSDE,
   WIND_DIRECTION_TSDE,
   
   TIME_STEP_DATA_ELEMENT_COUNT
   
} TimeStepDataElement;

#define MAX_DATA_ELEMENT_TYPE 22


typedef enum TimeModeType
{
	LATEST = 0,
	SETTIME,
	MINSELECT,
	MAXSELECT,
	VALUE_CHANGE	
	
} TimeModeType;



typedef enum FilterOperation
{
    SHOW_ALL = 0,
    SHOW_EQUAL,
    SHOW_NOT_EQUAL,
    SHOW_GREATER_EQUAL,
    SHOW_LESS_EQUAL,
    SHOW_GREATER,
    SHOW_LESS,
    NUM_FILTER_OPERATIONS
} FilterOperation;


typedef enum QueryMode
{
	AD_HOC_MODE = 0,
	TIME_STEP_MODE
} QueryMode;


typedef enum InstPrecipSelection
{
	PRECIP_TIME_30_MINUTES,
	PRECIP_TIME_1_HOUR,
	PRECIP_TIME_2_HOURS,
	PRECIP_TIME_3_HOURS,
	PRECIP_TIME_4_HOURS,
	PRECIP_TIME_6_HOURS,
	PRECIP_TIME_12_HOURS,
	PRECIP_TIME_18_HOURS,
	PRECIP_TIME_24_HOURS,
	PRECIP_TIME_COUNT
} InstPrecipSelection;

/* possible values for valuetype */

#define TYPE_VALUE  0
#define TYPE_DEPART 1


/* possible values for stage basis */

#define BASIS_OBS   0
#define BASIS_FCST  1
#define BASIS_MOFO  2
 

/* possible values for process present */

#define PROC_AS_OBS	 1
#define PROC_AS_PROC     0


/* type definitions */

#define DATA_SOURCE_LEN 50
typedef char DataSource[DATA_SOURCE_LEN + 1];

#define MAX_TYPESOURCE_COUNT 50
#define TYPE_SOURCE_STRING_LEN 50 //includes pe and the full name 
typedef char TypeSourceString[TYPE_SOURCE_STRING_LEN + 1];

#define SELECTED_AD_HOC_ELEMENT_FULL_STRING_LEN 30


/* main structure for storing all the point control options */

typedef struct
{   
  
   char query_mode; /* 0 is ad-hoc, 1 = time-step */	
   char	process_mode;
   char	process_selected;
   
   /* these options control what data are retrieved */
   
   char	element_type; //AdHocDataElementType or TimeStepDataElementType (acts as enum value)
   char	selectedAdHocElementString[SHEF_PE_LEN+1]; 
   char	selectedAdHocElementFullString[SELECTED_AD_HOC_ELEMENT_FULL_STRING_LEN+1]; //add extra characters for "Primary" and "PC and PP"   
 
  // char	selectedTimeStepElementString[BUFSIZ];   //this length is just convenient, not exact
   TimeStepDataElement selectedTimeStepElement; // no need to make this a string, enum is simpler
   
   char	PCandPP; //boolean
   char Primary; //boolean
   
   /*
   char	selectedTypeSrc[SHEF_TS_LEN+1];
   */
   
   TimeModeType time_mode;    /* time mode */
   char pc_time_str[MAXLEN_TIME];
   //char pc_hour_str[MAXLEN_HOUR];
   
   
   //instantaneous "hours" selection (includes 30 minutes)
   InstPrecipSelection 	inst_precip_accum_time_selection; 
   time_t 	valid_timet;   /* derived and set only when needed */
   int		dur_hours;
    
 
   char	filter_by_typesource;   
   int  type_source_chosen_count;
   TypeSourceString *type_source_chosen_array;
 
   
   /* the below options do not control what data are retrieved;
      they only control how the data are post-processed */
       
      
   
   char		filter_by_hsa;
   char 	hsa_list[MAXLEN_AREALIST];
   
   char 	filter_by_datasource;
   int		data_sources_chosen_count;
   DataSource	*data_sources_chosen_array;
   
   RiverStationFilter river_station_filter;
   PrecipPeFilter precip_pe_filter;
   
   /* boolean values */
   
   char suppress_missing;
  /*  char suppress_zeros;
   */
   
   char	fcstpts_only;
   
   /*
    * What to display
    */
   char	value;
   char	id;
   char	name;
   char	time;
   char	icon;
   char elevation;
   char paramCode;

   char riverstatus ; /* New addition on 3/20/2003 to support the selective
                         displaying of riverstatus information. Bryon L. */ 
   
   char derive_stage_flow ;
   char	fldlevel;
   char	valuetype;   /* integer */
   
   
   /* integer type value */
   
   char	stagebasis;
   FilterOperation valueFilterOperation;
   float valueFilterValue;
   
   FilterOperation elevFilterOperation;
   float elevFilterValue;
   
} pc_options_struct;

/* Function Prototypes. */

pc_options_struct * get_pc_options();
void initialize_options(pc_options_struct *pc_options);

void adjust_pctimestr ( char * timestr, int dec_inc , long amount ) ;

void set_selected_time_step_element_string_by_element(TimeStepDataElement element);
void pc_set_selected_pe ( const char * physical_element , int datatype ,
                      int selected_position ) ;
void set_current_ts ( const char * typesource ) ;
void set_pcoptions_datatype ( int datatype ) ;
void set_pcoptions_timestring ( ) ;
void pc_set_selected_ts ( const char * typesource );


#endif

