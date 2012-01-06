#ifndef POINTCONTROL_TIMESTEP_H_
#define POINTCONTROL_TIMESTEP_H_

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <List.h>

#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "pointcontrol_pets.h"
#include "pointcontrol_riverstatus.h"
#include "pointcontrol_value.h"
#include "pointcontrol_report.h"
#include "pointcontrol_mgr.h"
#include "rating_util.h"
#include "RiverStatus.h"
#include "time_convert.h"
#include "pointcontrol_options.h"

#define MAX_VALUE_COUNT (24*60)

typedef struct StationEntryDetails
{
  char lid[LOC_ID_LEN +1];
  char hsa[HYD_SERV_LEN + 1];
  char pe[SHEF_PE_LEN + 1 ];
  char shef_dur[SHEF_DUR_CODE_LEN + 1];
  char ts[SHEF_TS_LEN + 1 ];
  char ex[SHEF_EX_LEN + 1];
  double elevation;
  double lon;
  double lat;
  char fcst_pt[BOOL_LEN+1];
  char name[LOC_NAME_LEN +1];
  char dcp[2];
  char observer[2];
  char telm_type[11];
  double valueArray[MAX_VALUE_COUNT] ;
  int value_count;
  time_t start_time;
  time_t incr_time;
} StationEntryDetails;


typedef enum ProcessLineMode
{
	PROCESS_NEW_STATION = 0,
	PROCESS_DATA_SOURCES,
	PROCESS_VALUES
    	
} ProcessLineMode;

typedef struct PdcFileInfo
{
	int success;
	
    int entry_count;
    time_t start_time;
    time_t increment_time;
    
    int value_count;	
	
} PdcFileInfo;
 

StationEntryDetails * getStationDetailsArray();
StationEntryDetails * allocateStationDetails(int newCount);

void getAppsDefaultsValue(const char * token_name, char * token_value);


long get_timestep_file_creation_time();

/* requests for static info */
int load_TimeStepTypeSourceArray();
void pc_loadTimeStepElementTypes();

int * loadTimeStepElementArrayByElementType(TimeStepDataElementType elementType,
										   int *elementCount);



// requests for dynamic data


int pc_timestep_process_request(int retrieval_required);

int get_inst_precip_array_index(InstPrecipSelection selection);



void pc_timestep_read_file(const char * filename,  
                  		  const pc_options_struct * pc_options, 
                  		  PdcFileInfo * fileInfo);
                  		  
void pc_timestep_filter_reports(ReportList* reportListHead,
                             int station_count);
  
void pc_timestep_filter_riverstation_reports( pc_options_struct *pc_options,
                                            ReportList *reportListHead);
                                            
void pc_timestep_filter_by_precip_pe( pc_options_struct *pc_options,
                                              ReportList *reportListHead);
                             
char * get_timestep_full_path_from_options(const pc_options_struct * pc_options,
										   char * full_path);


void pc_timestep_read_header_info(FILE *fptr, 
                      int* num_of_values, 
                      time_t* start_time, 
                      time_t* incr_time);
                      
int process_line(const char * line, 
                 int num_of_values, 
                 ProcessLineMode process_line_mode,
                 time_t start_time,
                 time_t incr_time);
                 
                 
int load_station_info(const char * line, int index, StationEntryDetails *station_details);

void load_values_info(const char* line, 
                           int index, 
                           int num_of_values,
                           time_t start_time,
                           time_t incr_time);
                           
void load_datasource_info(const char* line, 
                          int index); 
                                                 
void create_report_list(int num_of_stations, 
                        int  num_of_values, 
                        time_t start_time, 
                        time_t incr_time,
                        time_t valid_report_time,
                        int in_instant_precip_mode,
                        InstPrecipSelection instant_precip_selection);   
                        
                                                           
void copy_station_details_to_report_node(ReportList *report_node,
										StationEntryDetails *station,
										time_t valid_time,
										int valueIndex);

                                            
int pc_is_reservoir(const char * display_class);
int getIhfsDurCodeFromShefDurCode(char shefDuration);

#endif /*POINTCONTROL_TIMESTEP_H_*/

