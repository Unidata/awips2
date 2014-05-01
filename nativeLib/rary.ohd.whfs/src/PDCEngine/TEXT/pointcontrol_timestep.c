
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pointcontrol_timestep.h"
#include "LookupTable.h"
#include "pointcontrol_derive.h"
#include "time_convert.h"
#include "CodeTimer.h"
#include "ToolDefs.h"
#include "LoadUnique.h"
#include "ShefTs.h"

 static StationEntryDetails * main_station_details = NULL;
 static int station_entry_count = 0;
 static int actual_station_entry_array_size = 0;
 static time_t timestep_file_creation_time = 0;
 
 
 static char fileNameArray[][50] = { "Height.dat", 
 								 "FlowStorage.dat",
 								 "HeightAboveFloodStage.dat", 
 								 "PercentFloodFlow.dat",
 								 "PrecipInstant.dat",
 								 "Precip1Hour.dat",
 								 "Precip3Hour.dat",
 								 "Precip6Hour.dat",
 								 "Precip24Hour.dat",
 								 "SnowWaterEquivalent.dat",
 								 "SnowWaterEquivalentDelta.dat",
 								 "Temperature.dat",
 								 "TemperatureDelta.dat",
 								 "TemperatureMax.dat",
 								 "TemperatureMin.dat",
 								 "DewPoint.dat",
 								 "DewPointDelta.dat",
 								 "RelativeHumidity.dat",
 								 "WindSpeed.dat",
 								 "WindDirection.dat"			 
 							   } ;
  
// ---------------------------------------------------------------------------------------

long get_timestep_file_creation_time()
{
   // this gets the file creation time as described in the files' first line , not the
   // actual file editted time.  This will help during testing, to avoid undesirable times from
   // hand-modified files
   return timestep_file_creation_time;    
}

StationEntryDetails * allocateStationDetails(int newCount)
{
	
	char header[] = "allocateStationDetails";
	int standardSize = 500;
//	static codetimer timer;
//	restart_timer(&timer);
  
    if (actual_station_entry_array_size == 0) //first time
    {
    	actual_station_entry_array_size = standardSize;
    	  	
    	fprintf(stdout, "%s  before initial main_station_details super alloc = %p newCount = %d\n", 
  	    header, main_station_details, newCount);
  
    	main_station_details = (StationEntryDetails*) realloc(main_station_details,
                               (sizeof(StationEntryDetails))*(actual_station_entry_array_size));
                               
        fprintf(stdout, "%s  after initial main_station_details super alloc\n", 
  	        header);
    }
    else if (newCount > actual_station_entry_array_size)
    {
        actual_station_entry_array_size = 2 * newCount;
       
        fprintf(stdout, "%s  before realloc main_station_details = %p newCount = %d\n", 
  	            header, main_station_details, newCount);
  
       	main_station_details = (StationEntryDetails*) realloc(main_station_details,
                               (sizeof(StationEntryDetails))*(actual_station_entry_array_size));
                               
        fprintf(stdout, "%s  after realloc main_station_details\n", 
  	            header);
    }
   
	
//   stop_timer(&timer);
//   print_elapsed_time(&timer, "realloc has taken ", stdout );
                               
	return main_station_details;
}

// ----------------------------------------------------------------------------------------

StationEntryDetails * getStationDetailsArray()
{
 
   return main_station_details;		
}

// ----------------------------------------------------------------------------------------

void getAppsDefaultsValue(const char * token_name, char * token_value)
{
	
 //  char header[] = "getAppsDefaultsValue():" ;
   int gad_token_len = 0;
   int gad_value_len = 0;
   
  
   gad_token_len = strlen(token_name);
   get_apps_defaults((char *) token_name, &gad_token_len, token_value, &gad_value_len); 	  	
  
  // printf("%s token_name = :%s:, token_value = :%s:  \n", header, token_name, token_value);
  
   return;
}

// ----------------------------------------------------------------------------------------

static char * getDataDirectory()
{
   char header[] = "getDataDirectory():" ;
   static char cached_data_directory[BUFSIZ];
   static int first_time = 1;
     
   if (first_time)
   {
   	   first_time = 0;
       getAppsDefaultsValue("pdc_pp_dir", cached_data_directory);  	
   }
   
   printf("%s cached_data_directory = %s\n", header, cached_data_directory);
   
   return cached_data_directory;
}


// ----------------------------------------------------------------------------------------
int * loadTimeStepElementArrayByElementType(TimeStepDataElementType elementType, int *elementCount)
{
	int i = 0;
	static int timeStepElementArray[5];
	char header[] = "loadTimeStepElementArrayByElementType";
	switch(elementType)
	{
		case RIVER_TIME_STEP_TYPE:
		{
		   timeStepElementArray[i++] = STAGE_POOL_TSDE;
		   timeStepElementArray[i++] = FLOW_STORAGE_TSDE;
		   timeStepElementArray[i++] = DEPTH_ABOVE_FS_TSDE;
		   timeStepElementArray[i++] = PERCENT_FLOOD_FLOW_TSDE;
		   
		   break;	
		}	
		
		case RAIN_TIME_STEP_TYPE:
		{
		   timeStepElementArray[i++] = INSTANTANEOUS_PRECIP_TSDE;
		   timeStepElementArray[i++] = HOURLY_PRECIP_TSDE;
		   timeStepElementArray[i++] = THREE_HOUR_PRECIP_TSDE;
		   timeStepElementArray[i++] = SIX_HOUR_PRECIP_TSDE;
		   timeStepElementArray[i++] = DAILY_PRECIP_TSDE; /* 12Z TO 12Z */
			
		   break;	
		}
		
		case SNOW_TIME_STEP_TYPE:
		{
		   timeStepElementArray[i++] = SNOW_WATER_EQUIV_TSDE;
		   timeStepElementArray[i++] = SWE_24_HOUR_CHANGE_TSDE;
			
		   break;	
		}
		
		
		case TEMPERATURE_TIME_STEP_TYPE:
		{
		   timeStepElementArray[i++] = TEMPERATURE_TSDE;
		   timeStepElementArray[i++] = TEMP_24_HOUR_CHANGE_TSDE;
		   timeStepElementArray[i++] = TEMP_MAX_TSDE;
		   timeStepElementArray[i++] = TEMP_MIN_TSDE;	
		   break;	
		}
		
		case HUMIDITY_TIME_STEP_TYPE:
		{
		   timeStepElementArray[i++] = DEWPOINT_TSDE;
		   timeStepElementArray[i++] = DEWPOINT_24_HOUR_CHANGE_TSDE;
		   timeStepElementArray[i++] = RELATIVE_HUMIDITY_TSDE;
		
		   break;	
		}
		
		case WIND_TIME_STEP_TYPE:
		{
		   timeStepElementArray[i++] = WIND_SPEED_TSDE;
		   timeStepElementArray[i++] = WIND_DIRECTION_TSDE;
		   break;	
		}
		
		default:
		{
			fprintf(stderr, "Error in %s, elementType out of range. elementType = %d\n", 
					header, elementType);	
		}
		
	}
	
	*elementCount = i;
	
	return timeStepElementArray;
	
}
// ----------------------------------------------------------------------------------------

/*
int load_TimeStepTypeSourceArray( )
{
    int i = 0;
    pc_pets_struct * pc_petsdata = get_pc_petsdata();
  	 
    // allocate and set the strings to be used for the TimeStep Mode Station Types 
  
    pc_petsdata->timestep_type_source_count = 7;
    int count = pc_petsdata->timestep_type_source_count;
    
    Char100Array timestep_data_sources[] =
             { "GOES - RG", "LARC - RP", "SNOTEL - RM",
               "ALERT - RR", "METAR - RZ", "Reading 2 - R2",
               "Reading 3 - R3" };
  
 
	pc_petsdata->timestep_typeSourceBuffer = (Char100Array *) malloc 
                                                (sizeof (Char100Array) *
                                                (  count));
    for (i = 0; i < count; i++)
    {
    	   strcpy(pc_petsdata->timestep_typeSourceBuffer[i], timestep_data_sources[i]);  	
    }
    
    return count ;
}
*/

// ----------------------------------------------------------------------------------------
void getTimeStepNameForShefTs(char *typeSourceString)
{
   static ShefTs        *tsHead = NULL ;
   static int           first = 1;
    
   ShefTs               *tsPtr = NULL ;
   char                 ts[SHEF_TS_LEN + 1], where[80];

   /* load the data the first time this function is called */

   if (first)
   {
      sprintf(where, " ");
      tsHead = GetShefTs(where);

      if ( tsHead == NULL )
      {
         fprintf ( stderr , "In routine \"addNameFromShefTs\":\n"
                            "Could not load ShefTs information.\n" ) ;
         return ;
      }

      first = 0;
   }

   /* for the given code, append the descriptive name, if one exists. */

   //store the original typesource code (sometimes, a fully specified code/name string might be passed in).
   //in that case, don't append anything to it
   strcpy(ts, typeSourceString);

   tsPtr = (ShefTs *) ListFirst(&tsHead->list);
   while (tsPtr)
   {
      if (strncmp(typeSourceString, tsPtr->ts, SHEF_TS_LEN) == 0)
      {
         //sprintf(typeSourceString, "%s - %s", tsPtr->name, tsPtr->ts);
         sprintf(typeSourceString, "%s - %s", tsPtr->ts, tsPtr->name);
         break;
      }

      tsPtr = (ShefTs *) ListNext(&tsPtr->node);
   }

   return;
}

// ----------------------------------------------------------------------------------------

int load_TimeStepTypeSourceArray ()
{
   // This routine uses a combination of hardcoded and dynamically
   // loaded type sources 
    
   char header[] = "load_TimeStepTypeSourceArray";
  // pc_options_struct * pc_options = get_pc_options();
     pc_pets_struct * pc_petsdata = get_pc_petsdata();
 
   int count = 0;
   int listCount = 0;
 
   char    ** values = NULL;
   char    *typeSourceCode = NULL;
   char    where[255];
   UniqueList   *ulPtr = NULL ;
   UniqueList   *ulHead = NULL ;
   int   ulCount = 0 ;
   /* get a list of the unique pe,ts combinations */
            
   Char100Array hard_coded_type_sources[] =
             { "RG", "RP", "RM", "RR", "RZ" };
   int hard_coded_type_source_count = 5;


   sprintf(where, " WHERE ts like 'R%%' OR ts LIKE 'P%%' order by 1");
   ulHead = LoadUnique("ts", "IngestFilter", where, &listCount);

   if ( ulHead == NULL )
   {
      fprintf ( stderr , "In routine %s:\n"
                         "Could not load a list of unique pe, ts\n"
                         "combinations.\n", header ) ;
      return 0;
   }
   else
   {
        count = listCount + hard_coded_type_source_count;
        pc_petsdata->timestep_type_source_count = count;  
  
        pc_petsdata->timestep_typeSourceBuffer = (Char100Array *) malloc 
                                                    (sizeof (Char100Array) *
                                                    (  count ));
                                                    
         //add all of the hard_coded_type_sources
        int i = 0;
       
        for (i = 0; i < hard_coded_type_source_count; i++)
        {
             memset(pc_petsdata->timestep_typeSourceBuffer[i], 0,  (sizeof (Char100Array) - 1));
             strcpy(pc_petsdata->timestep_typeSourceBuffer[i], hard_coded_type_sources[i]);
             getTimeStepNameForShefTs(pc_petsdata->timestep_typeSourceBuffer[i]);
        }                                             
          
        //keep i at hard_code_type_source_count, not 0!                                            
                                                    
      /* loop thru the unique list and load into the buffer
         for the original, unseparated combinations */
        ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   
        while (ulPtr)
        {
            //values[] contains the type/source code
            values = ParseUnique ( ulPtr, &ulCount );
        
            
            //printf("%s ulPtr->uchar = :%s: \n", header, ulPtr->uchar);
         
            if ( ( values == NULL ) || ( ulCount < 1 ) )
            {
                fprintf ( stderr, "\nIn routine %s:\n"
                              "Could not parse the unique string '%s'.\n",
                              header, ulPtr->uchar );
                typeSourceCode = NULL;
                break;
            }
      
            typeSourceCode = values[0];
      
            //determine if this TS should be added to the list of type sources
            //we don't want to add a type/source that was already hardcoded
            int shouldBeAdded = 1;
            int j = 0;
            for (j = 0; j < hard_coded_type_source_count; j++)
            {
                if (strcmp(hard_coded_type_sources[j], typeSourceCode) == 0)
                {
                    shouldBeAdded = 0;
                    break;    
                }
            }
      
            if (shouldBeAdded)
            {
                memset(pc_petsdata->timestep_typeSourceBuffer[i], 0,  (sizeof (Char100Array) - 1));    
                strcpy(pc_petsdata->timestep_typeSourceBuffer[i], typeSourceCode);
                getTimeStepNameForShefTs(pc_petsdata->timestep_typeSourceBuffer[i]);
                
                i++;
            }
            else
            {
                count --;    
            }
        // printf("%s ts = :%s: \n", header, values[0]);
            FreeParseUnique ( values );
            values = NULL;
            ulPtr = (UniqueList *) ListNext(&ulPtr->node);
          
        }
   
      if (ulHead) 
         FreeUnique(ulHead);
  
   }
  
   pc_petsdata->timestep_type_source_count = count;
  
   return count;
}


// ----------------------------------------------------------------------------------------

int pc_timestep_process_request(int retrieval_required)
{
  char header[] = "pc_timestep_process_request(): ";
  pc_options_struct * pc_options = get_pc_options();
  static PdcFileInfo fileInfo; 
   
  char full_path[BUFSIZ];
  
  
  if (retrieval_required)
  {
  	 get_timestep_full_path_from_options(pc_options, full_path);
     
  //   printf("%s full_path = :%s: \n", header, full_path );
     pc_timestep_read_file(full_path,  pc_options, &fileInfo);
     
     if (fileInfo.success == 0)
     {
     	fprintf(stderr, "%s  Unable to read file.\n", header);
        return 0;	
     }
  }

  time_t validtime = 0;
  char time_string[ANSI_YEARSEC_TIME_LEN + 1];
  memset(time_string, '\0', ANSI_YEARSEC_TIME_LEN + 1);
  
  //this occurs when the PDC gui has not been called
  // and the primary preset is of time step mode
  if (strlen(pc_options->pc_time_str) < 1)
  {
        time(&pc_options->valid_timet);
        set_pcoptions_timestring ( );
  }
 
  sprintf(time_string, "%s:00", pc_options->pc_time_str);
  yearsec_ansi_to_timet(time_string, &validtime);
   
  long in_instant_precip_mode = 0;
  InstPrecipSelection instant_precip_selection = -1;
  if (pc_options->selectedTimeStepElement == INSTANTANEOUS_PRECIP_TSDE)
  {
  	 instant_precip_selection = 
  	      pc_options->inst_precip_accum_time_selection;
  	      
     in_instant_precip_mode = 1;
  	
  }
  
  create_report_list(fileInfo.entry_count, fileInfo.value_count,
                     fileInfo.start_time, fileInfo.increment_time, 
                     validtime, 
                     in_instant_precip_mode,
                     instant_precip_selection);

  pc_timestep_filter_reports(getReportListHead(), fileInfo.entry_count);
  
   
  process_river_threat_index (getReportListHead(), 
                              retrieval_required );
  
  
  
  return fileInfo.entry_count;
  
}


char * get_timestep_full_path_from_options(const pc_options_struct * pc_options,
										   char * full_path)
{
	char * dir_name = getDataDirectory();
    char header[] = "get_timestep_full_path_from_options(): ";
     
     
    TimeStepDataElement element = pc_options->selectedTimeStepElement;
    		
    		
    fprintf(stdout, "%s element = %d  \n", 
    		header, element); 
    		
    char * file_name = fileNameArray[element];		
	
	sprintf(full_path, "%s/%s", dir_name, file_name);
 	
	return full_path;	
}

// ----------------------------------------------------------------------------------------

void pc_timestep_read_file(const char * filename,  
                  		  const pc_options_struct * pc_options, 
                  		  PdcFileInfo * fileInfo)
                  		 
{	
    char header[] = "pc_timestep_read_file(): ";
	FILE *fptr = NULL;
	char * line = NULL;
	int len = 0;
	int read; 
	ProcessLineMode process_line_mode = PROCESS_NEW_STATION;
	int value_count = 0;
	time_t start_time;
	time_t incr_time;
	int entry_count=0;
	
	// Clear out the old array of station entries
	if (main_station_details != NULL)
	{
	    free(main_station_details);
		main_station_details = NULL;
		actual_station_entry_array_size = 0;
		station_entry_count = 0;
	}
	
	
	
	fptr = fopen(filename, "r");
	if (fptr == (FILE*)NULL)
	{
	    fprintf(stdout, "\nfile open error[%s]\n", strerror(errno));
	    fflush(stdout);
	    fileInfo->success = 0;
	    return;
	}

    pc_timestep_read_header_info(fptr, &value_count, &start_time, &incr_time);

    fileInfo->start_time = start_time;
    fileInfo->increment_time = incr_time;
    fileInfo->value_count = value_count;

    fprintf(stdout,"%s got: value_count = [%d] start_time =  [%ld] increment_time =  [%ld]\n",
                    header, value_count, start_time, incr_time);
                    
    
    char ansi_time_string[ANSI_YEARSEC_TIME_LEN+1];
    
    timet_to_yearsec_ansi(start_time, ansi_time_string);
    fprintf(stdout,"%s that translates to  value_count = [%d] start_time =  [%s] increment_time =  [%ld] hours\n",
                    header, value_count,  ansi_time_string, incr_time/SECONDS_PER_HOUR);
               
    
    char end_time_string [ANSI_YEARSEC_TIME_LEN+1];           
    time_t end_time =  start_time+((value_count-1)*incr_time);
    timet_to_yearsec_ansi(end_time, end_time_string);
     
    fprintf(stdout,"%s So, end time = [%s].\n",
                    header, end_time_string);
                   
               
    fflush(stdout);

	while ((read = getline(&line, &len, fptr)) != -1) 
	{
	     if(line[0] == '\n') /*** Means no more entries  ***/
	     {
	       break;
	     }
	     
	     process_line_mode = PROCESS_NEW_STATION;
	     entry_count = process_line(line, value_count,
	                                    process_line_mode,
	                                    start_time, incr_time);
	     
	     process_line_mode = PROCESS_DATA_SOURCES;
	     
	     if (line)
	     {
        	free(line);
        	line = NULL;
	     }
	     
	     
	     if((read = getline(&line, &len, fptr)) != -1)
	     {
	         entry_count = process_line(line, value_count,
	                                        process_line_mode,
	                                        start_time, incr_time);
	                                        
		     if (line)
		     {
	        	free(line);
	        	line = NULL;
		     }
	     }
	     
	     process_line_mode = PROCESS_VALUES;
	     if((read = getline(&line, &len, fptr)) != -1)
	     {
	         entry_count = process_line(line, value_count,
	                                      process_line_mode,
	                                      start_time, incr_time);
	                                      
	         if (line)
		     {
	        	free(line);
	        	line = NULL;
		     }                              
	                                      
	     }
	     
	
	} 
	
	fileInfo->entry_count = entry_count;
    
        
    fprintf(stdout, "%s station_count = %d\n", header, entry_count);     
//    StationEntryDetails * station_details = getStationDetailsArray();

/*
	for( i=0; i < station_count; i++)
	{
		  fprintf(stdout, "%s i = %d\n", header, i);  
		
		  int j=0;
		  fprintf(stdout,"\n**************");
		  fprintf(stdout,"\n Entry [%d]: lid[%s] hsa [%s] pe[%s] ts[%s] ex[%s] ele[%lf] lat[%lf] lon[%lf] fcst_pt[%s] name[%s] \n", 
		               i, station_details[i].lid, 
		               station_details[i].hsa, station_details[i].pe,
		               station_details[i].ts, station_details[i].ex, 
		               station_details[i].elevation ,  
		               station_details[i].lat, station_details[i].lon, 
		               station_details[i].fcst_pt ,  
		               station_details[i].name);
		 fflush(stdout);
		 for(j=0; j<num_of_values;j++)
		 {
		  
		      fprintf(stdout,"[(%d) %lf : %ld]  ", j,  station_details[i].values[j],
		                                     (station_details[i].start_time)
		                                     + (j*(station_details[i].incr_time)));
		      fflush(stdout);
	     }
	}
 */
    
    fileInfo->success = 1;
	
	fclose(fptr);
	 
    return;
}

// ----------------------------------------------------------------------------------------
void pc_timestep_filter_reports(ReportList* reportListHead,
                             int station_count)
{
            
     filter_reports_and_add_info ( get_pc_options(),
		                   		  getReportListHead() );  
		                   	
	 pc_timestep_filter_riverstation_reports(get_pc_options(),
		                   		  			 getReportListHead() );
                                                                                         
     pc_timestep_filter_by_precip_pe(get_pc_options(),
                                     getReportListHead() );
                                              	                   		                         	
     return;                            	
}
// ----------------------------------------------------------------------------------------
void pc_timestep_filter_riverstation_reports( pc_options_struct *pc_options,
											ReportList *reportListHead)
{
		
   ReportList *rPtr = NULL;
 
   // we only want to filter in time step mode with a River element type
   if (pc_options->element_type != RIVER_TIME_STEP_TYPE)  
   {
       return;	
   }
   
   RiverStationFilter filter = pc_options->river_station_filter; 
   
   // There is no filtering performed, so bail
   if (filter == ALL_STATIONS_RSF)
   {
       return;	
   }
   
	
    
  if (reportListHead == NULL)
  {   
       return; 
  } 
    
    
   rPtr = ( ReportList * ) ListFirst ( & reportListHead->list ) ;   

   while ( rPtr )
   {
       int is_reservoir = pc_is_reservoir(rPtr->disp_class);
       if (pc_options->river_station_filter == STREAM_STATIONS_RSF)
       {
           if (is_reservoir)
           {
              rPtr->use = 0;	
           }
       }
       else //RESERVOIR_STATIONS_RSF
       {
           if (	! is_reservoir)
           {
              rPtr->use = 0;	
           }
       }      
  
       rPtr = ( ReportList * ) ListNext ( & rPtr->node );    
   }
	
    return;	
}											

// ----------------------------------------------------------------------------------------
void pc_timestep_filter_by_precip_pe( pc_options_struct *pc_options,
                                              ReportList *reportListHead)
{
   //Note: this sort of filter routine- filters OUT, it never sets rPtr->use to 1,
   //since other filters may set it to 0, and hence filter out the station     
   ReportList *rPtr = NULL;
 
   // we only want to filter in time step mode with a RAIN_TIME_STEP_TYPE that is not
   // INSTANTANEOUS
   if (pc_options->element_type != RAIN_TIME_STEP_TYPE)  
   {
       return;  
   }
   if (pc_options->selectedTimeStepElement == INSTANTANEOUS_PRECIP_TSDE)  
   {
       return;  
   }
   
   
   PrecipPeFilter filter = pc_options->precip_pe_filter; 
   
   // There is no filtering performed, so bail
   if (filter == PC_AND_PP_PPF)
   {
       return;  
   }
   
    
  //there is nothing to filter, so bail
  if (reportListHead == NULL)
  {   
       return; 
  } 
    
    
   rPtr = ( ReportList * ) ListFirst ( & reportListHead->list ) ;   

   while ( rPtr )
   {
       if (filter == PC_ONLY_PPF)
       {
           if (strcmp(rPtr->pe, "PP") == 0 )
           {
              rPtr->use = 0;    
           }
       }
       else if (filter == PP_ONLY_PPF)
       {
           if ( strcmp(rPtr->pe, "PC") == 0)
           {
              rPtr->use = 0;    
           }
       }      
  
       rPtr = ( ReportList * ) ListNext ( & rPtr->node );    
   }
    
    return; 
}                                           

// ----------------------------------------------------------------------------------------


int pc_is_reservoir(const char * display_class)
{
	int result = 0;
	/*
	if (
	     (strcmp(pe, "HP") == 0) ||
	     (strcmp(pe, "HT") == 0) ||
	     (strcmp(pe, "LS") == 0)
	   )
	{
	    result = 1;   	
	}
    */
    
    if (strstr(display_class, "D"))
    {
        result = 1;    
    }
	
	return result;
}

// ----------------------------------------------------------------------------------------

void pc_loadTimeStepElementTypes()
{
	int itemCount = 6;
	ElementTypeText text[6] = { "River",
								"Rain",
								"Snow",
								"Temperature",
							    "Humidity",
							    "Wind" 
   							   };
   							
   	int i = 0;
   	pc_pets_struct *pc_petsdata = get_pc_petsdata(); 
   	
   	/*  free old memory, if needed */
   	if (pc_petsdata->element_type_count > 0)
   	{
   	    free(pc_petsdata->elementTypeTextArray);
   	    pc_petsdata->elementTypeTextArray = NULL;
   	}
   	
   	/* allocate new memory */
   	pc_petsdata->elementTypeTextArray = malloc(itemCount * sizeof(ElementTypeText));
   	pc_petsdata->element_type_count = itemCount;	
   	
   	
   	for (i = 0; i < itemCount; i++)
   	{
   	    strcpy(pc_petsdata->elementTypeTextArray[i], text[i]);
   		printf("pc_loadTimeStepElementTypes(): elementTypeText %s = \n", 
   				 pc_petsdata->elementTypeTextArray[i]);
   		
   	} 
   						
   	return;						
}

// ----------------------------------------------------------------------------------------


void pc_timestep_read_header_info(FILE *fptr, 
                      int* num_of_values, 
                      time_t* start_time, 
                      time_t* incr_time)
{
     char header[] = "pc_timestep_read_header_info() ";
	 int header_line_cnt = 0;
	 char str[80];
     char date_time_string[ANSI_YEARSEC_TIME_LEN + 1];
     char date_string[ANSI_YEARSEC_TIME_LEN + 1];
     char time_string[ANSI_YEARSEC_TIME_LEN + 1];
	 int len=0;
	 char *line=NULL;
	 int read;
     int scannedVariableCount = 0;
	
	 while ((read = getline(&line, &len, fptr)) != -1) 
	 {
		 header_line_cnt++;
         
         if (header_line_cnt == 1)
         {
             time_t timet = 0;
             scannedVariableCount = sscanf(line, "Created on: %s %s", date_string, time_string);
             
             if (scannedVariableCount == 2)
             {
                 sprintf(date_time_string, "%s %s", date_string, time_string);
                 yearsec_ansi_to_timet(date_time_string, &timet);
                 //store this for later use
                 timestep_file_creation_time = timet;
             }
             else
             {
                
             }
             printf("%s line = :%s:", header, line);
             printf("%s date_string = :%s:", header, date_time_string);
              
             continue;
         }
		 else if (header_line_cnt < 6 ) //skip rest of first 6 lines (2..6)
		 {
		     continue;
		 }
	  
	     if(fgets(str, 80, fptr) != (char*) NULL)
	     {
	         sscanf(str,"%d %ld %ld", num_of_values, start_time, incr_time);
	         //Java code will be changed to store seconds instead
	   //  *start_time = (*start_time)/1000; /* Convert milli secs to secs */
	         break;
	     }
	 }
	 return;
}

// ----------------------------------------------------------------------------------------
int process_line(const char* line, 
                 int num_of_values, 
                 ProcessLineMode process_line_mode,
                 time_t start_time,
                 time_t incr_time)
{
  char header[] = "process_line(): ";

  //printf("%s line = :%s: \n", header, line);

  StationEntryDetails * station_details = getStationDetailsArray();
 
  if(process_line_mode == PROCESS_NEW_STATION)
  {
  	  station_details = allocateStationDetails(station_entry_count + 1);
      load_station_info(line, station_entry_count, station_details);
  }
  
  else if (process_line_mode == PROCESS_DATA_SOURCES )
  {
      load_datasource_info(line, station_entry_count);
  }
  
  else if (process_line_mode == PROCESS_VALUES)
  {
      load_values_info(line, station_entry_count, num_of_values, start_time, incr_time);
      station_entry_count++;
  }
  
  else
  {
  	  fprintf(stderr, "%s Invalid ProcessLineMode = %d\n", 
  	          header, (int) process_line_mode);
  }
  
  return station_entry_count;
}

// ----------------------------------------------------------------------------------------

int load_station_info(const char* line, int index, 
                       StationEntryDetails *station_details_array)
{ 
  int success = 1;
  char header[] = "load_station_info(): ";
  char *str = ( char *) line;
 
  char * lid_token = NULL;
  char * hsa_token = NULL;
  char * petse_token = NULL;
  char * elev_token = NULL;
  char * lat_token = NULL;
  char * lon_token = NULL; 
  char * fcstpt_token = NULL;
  char * name_token = NULL;
    
  StationEntryDetails * station = &station_details_array[index];

  lid_token = strtok(str, " ");
  hsa_token = strtok(NULL, " ");
  petse_token = strtok(NULL, " ");
  elev_token = strtok(NULL, " ");
  lat_token = strtok(NULL, " ");
  lon_token = strtok(NULL, " ");
  fcstpt_token = strtok(NULL, " ");
  name_token = strtok(NULL, "\n");
  
  // logical AND all of these together.  They all need to be != NULL
  
  long all_not_null = 
                      ( lid_token && hsa_token &&
                      petse_token && elev_token && 
                      lat_token && lon_token &&
                      fcstpt_token && name_token ) ;
                                          
                      
  if (! all_not_null)
  {
      success = 0;	
  }


  if (success)
  {
    
      memset(station->lid, '\0', LOC_ID_LEN+1);
	  strncpy(station->lid, lid_token, LOC_ID_LEN);
      
      memset(station->hsa, '\0', HYD_SERV_LEN+1);
	  strncpy(station->hsa, hsa_token, HYD_SERV_LEN);	 
	 
	  station->pe[0] = petse_token[0];
	  station->pe[1] = petse_token[1];
	  station->pe[2] = '\0';
      station->shef_dur[0] = petse_token[2];
      station->shef_dur[1] = '\0';
      
	  station->ts[0] = petse_token[3];
	  station->ts[1] = petse_token[4];
	  station->ts[2] = '\0';
	  station->ex[0] = petse_token[5];
	  station->ex[1] = '\0';
	  
	  station->elevation = atof(elev_token);
	 
	  station->lat = atof(lat_token);
	  station->lon = atof(lon_token);
      
      memset(station->fcst_pt, '\0', BOOL_LEN+1);
	  strncpy(station->fcst_pt , fcstpt_token, BOOL_LEN);
	
	  name_token[strlen(name_token)]='\0';
	    
      memset(station->name, '\0', LOC_NAME_LEN +1+1);
	  strncpy(station->name, name_token, LOC_NAME_LEN +1);
  }
  else
  {
  	  fprintf(stdout,"%s: failed at line[%s] index[%d] \n", header, line, index);
  }
  
  return success;
}


// ----------------------------------------------------------------------------------------


void load_values_info(const char* line, 
                           int index, 
                           int value_count,
                           time_t start_time,
                           time_t incr_time)
{
  int cnt = 0;
  char *str =  (char *) line;
  char * token = NULL;
  StationEntryDetails * station_details_array = getStationDetailsArray();
  StationEntryDetails * station = &station_details_array[index];
  
  //char header[] = "load_values_info(): ";
  //fprintf(stdout, "%s process second line[%s] index[%d]", header, line,index);
 // fflush(stdout);


  station->start_time = start_time;
  station->incr_time = incr_time;
  

  if (value_count > MAX_VALUE_COUNT)
  {
      value_count = MAX_VALUE_COUNT;	
  } 
  
  station->value_count = value_count;
     
   // if the entire time series is declared to be missing
  if (strncmp(str,"MISSING", 7) == 0)
  {
        //fprintf(stdout, "%s FOUND MISSING in time series \n", header);
        for (cnt = 0; cnt < value_count; cnt++)
        {
            station->valueArray[cnt] = MISSING_VAL;
        }
  }
  else //there is some actual data in the time series
  {  
     
      while(cnt < value_count)
      {
                
         if(cnt == 0)
             token = strtok(str, " ");
         else if (cnt != value_count -1)
             token = strtok(NULL, " ");
         else
             token = strtok(NULL, "\n");    
           
         if (token != NULL)
         {
             station->valueArray[cnt] = atof(token);	
         }  
       
         cnt++;
      }
      
  }
  
  return;
}

// ----------------------------------------------------------------------------------------


void load_datasource_info(const char* line, 
                          int index)
{

  StationEntryDetails *station_details = getStationDetailsArray();	
	
  char *str = (char *) line;

  strcpy(station_details[index].dcp, strtok(str," "));
  strcpy(station_details[index].observer, strtok(NULL," "));
  strcpy(station_details[index].telm_type, strtok(NULL,"\n"));
  
  return;
  
}

// ----------------------------------------------------------------------------------------


void create_report_list(int num_of_stations, 
                        int  num_of_values, 
                        time_t start_time, 
                        time_t incr_time,
                        time_t valid_report_time,
                        int in_instant_precip_mode,
                        InstPrecipSelection instant_precip_selection)
{
	 int i = 0;
	 //int j = 0;
	 //time_t loop_time;
	 int useful_loop_index = -1;
	 char header[] = "create_report_list(): ";
	 char validtime_string[BUFSIZ];
	 char starttime_string[BUFSIZ];
	 ReportList * new_report_list_head_ptr =NULL;
	 ReportList * new_report_node_ptr = NULL;
	 ReportList * reportListHead = getReportListHead();
	 StationEntryDetails * station_details_array = getStationDetailsArray();
     StationEntryDetails *station  = NULL;
     
	 fprintf(stdout,"%s beginning \n", header);	 
	 long count = 0;
	 
	 time_t time_diff = valid_report_time - start_time;
	 if (incr_time > 0)
	 {
	 	 if (in_instant_precip_mode)
	 	 {
	 	 	 useful_loop_index = 
	 	 	      get_inst_precip_array_index(instant_precip_selection);	 	 	
	 	 }
	 	 else
	 	 {
	         useful_loop_index = time_diff / incr_time;
	 	 }
	     
	     fprintf(stderr, "%s useful_loop_index = %d = time_diff = %ld / incr_time = %ld\n",
	     	        header, useful_loop_index, time_diff, incr_time);	
	     
	      fprintf(stderr, "%s num_of_values = %d\n",
	     	        header, num_of_values);	
	     
	     
	     if  (! ( (useful_loop_index >=0) && (useful_loop_index < num_of_values)  ) )
	     {
	     	useful_loop_index = -1;
	     	fprintf(stderr, "ERROR in %s problem with start_time = %ld\n.  Check roundness.",
	     	        header, start_time);	
	        timet_to_yearsec_ansi(valid_report_time, (char *) validtime_string);
	        timet_to_yearsec_ansi(start_time , (char *) starttime_string);
	        
	        fprintf(stderr, "%s valid_report_time = :%s:  start_time = :%s \n",
	     	        header, validtime_string, starttime_string);	
	     }
	 }
	 else
	 {
	     fprintf(stderr, "ERROR  in %s increment time is not positive. It = %ld\n", header, incr_time);	
	 }
	 
	 
	 //verify that I got a decent loop index 
	 if (useful_loop_index == -1)
	 {
	    	fprintf(stdout, "%s  useful_loop_index = %d num_of_values = %d \n",
	    	         header, useful_loop_index, num_of_values); 	
	 }
	 
	 
	 // for each station entry, (there can be more than 1 entry per lid)
	 for( i=0; i < num_of_stations; i++)
     {
	     station = &station_details_array[i];	 
//		 fprintf(stdout,"\n**************");
//		 fflush(stdout);
	       	
	     new_report_node_ptr = (ReportList*) calloc(1, sizeof(ReportList));
	     copy_station_details_to_report_node(new_report_node_ptr, 
	                                         station, 
	                                         valid_report_time,
	                                         useful_loop_index);
    
	     if (new_report_list_head_ptr == NULL)
	     {
	         new_report_list_head_ptr = new_report_node_ptr;  
	         // fprintf(stdout,"\n List init....[%p]",temp_report_list);
	         // fflush(stdout);
	         ListInit(&new_report_list_head_ptr->list);
	     }
	     // fprintf(stdout,"\n List add");
	     // fflush(stdout);
	     ListAdd(&new_report_list_head_ptr->list, &new_report_node_ptr->node);
	     count++;
  	
	 } //end for i
		
	  // free up old report list
	  if (reportListHead != NULL)
	  {
	     FreeReportList(reportListHead);
	     reportListHead = NULL;	
	  }
	  
	  setReportListHead(new_report_list_head_ptr);


/*
	  reportListHead = getReportListHead();

	  
	  while(new_report_node_ptr != NULL)
	  {
	     fprintf(stdout,"\n Entry[%ld] lid[%s] ps[%s] ts[%s] ex[%s] " 
	    "pr[%f] dur[%ld] shef_qual[%s] qual[%ld] name[%s] "
	    " use[%d] val[%lf] v2[%lf] vt[%ld] bt[%ld]"
	    " thr[%c] lat[%lf] lon[%lf] dis[%s]", 
	    cnt, temp_ptr->lid, temp_ptr->pe, temp_ptr->ts, temp_ptr->extremum,
	    temp_ptr->probability, temp_ptr->dur, temp_ptr->shef_qual_code, temp_ptr->quality_code, temp_ptr->name,
	    temp_ptr->use, temp_ptr->value , temp_ptr->value2, temp_ptr->validtime , temp_ptr->basistime ,
	    temp_ptr->threat_index , temp_ptr->latitude , temp_ptr->longitude, temp_ptr->disp_class);
	    fflush(stdout);

	     
	    new_report_node_ptr=(ReportList*) ListNext(&new_report_node_ptr->node);
	  }
*/
	
	  fprintf(stdout,"%s ending \n", header);

} //end create_report_list


int get_inst_precip_array_index(InstPrecipSelection selection)
{
	int index = -1;

    char header[] = "get_inst_precip_array_index() : ";	

	switch (selection)
	{
	    case PRECIP_TIME_30_MINUTES:
	    	index = 24;
	    	break;
	    
		case PRECIP_TIME_1_HOUR:
		    index = 23;
			break;
			
	    case PRECIP_TIME_2_HOURS:
		    index = 22;
			break;
			
		case PRECIP_TIME_3_HOURS:
		    index = 21;
			break;
			
		case PRECIP_TIME_4_HOURS:
		    index = 20;
			break;	
			
	    case PRECIP_TIME_6_HOURS:
		    index = 18;
			break;		
					
		 case PRECIP_TIME_12_HOURS:
		    index = 12;
			break;
		
		 case PRECIP_TIME_18_HOURS:
		    index = 6;
			break;	
			
		 case PRECIP_TIME_24_HOURS:
		    index = 0;
			break;		
            
         default:
         
            break;   	
	}
	printf("%s selection = %d index = %d \n", header, selection, index);
	 	 	 
	return index;	 	 
	 	 	 
}



void copy_station_details_to_report_node(ReportList *report_node,
										StationEntryDetails *station,
										time_t valid_time,
										int valueIndex)
{
			
	//char header[] = "copy_station_details_to_report_node(): ";										
    strcpy(report_node->lid,station->lid);
    
    memset(report_node->pe, '\0', SHEF_PE_LEN +1);
    strncpy(report_node->pe,station->pe, SHEF_PE_LEN);
    
    memset(report_node->ts, '\0', SHEF_TS_LEN +1);
    strncpy(report_node->ts,station->ts, SHEF_TS_LEN);
    
    
    memset(report_node->extremum, '\0', SHEF_EX_LEN +1);
    strncpy(report_node->extremum,station->ex, SHEF_EX_LEN);
    
    report_node->probability = 1.0;
    report_node->dur = getIhfsDurCodeFromShefDurCode(station->shef_dur[0]);
    
    memset(report_node->shef_qual_code, '\0', 2);
    strncpy(report_node->shef_qual_code ,"Q", 1);
    
    report_node->quality_code = 1;
    
    memset(report_node->name, '\0', REPORTLIST_STATIONNAME_LEN + 1);
    strncpy(report_node->name, station->name, REPORTLIST_STATIONNAME_LEN);
    
   
    report_node->use = 1;
    
    if ((valueIndex >= 0) & (valueIndex < station->value_count))
    {
        report_node->value = station->valueArray[valueIndex];
        report_node->value2 = station->valueArray[valueIndex];
    }
    else
    {
    	 report_node->value = MISSING;
    	 report_node->value2 = MISSING;
    }
    
    report_node->validtime = valid_time; 
    report_node->basistime = valid_time;

    report_node->threat_index = THREAT_MISSING_DATA; //note: had been set to 'R', literally
    report_node->latitude = station->lat;
    report_node->longitude = (-1) * (station->lon);
    
    memset(report_node->disp_class, '\0', 2);
    strncpy(report_node->disp_class , "F",1 ); // note: had been set to "F", literally
    
    /*
     * #define DISP_CLASS_FCSTPT     "F"
		#define DISP_CLASS_RIVER     "R"
		#define DISP_CLASS_RESERVOIR "D"
		#define DISP_CLASS_PRECIP    "P"
		#define DISP_CLASS_SNOW      "S"
		#define DISP_CLASS_TEMP      "T"
		#define DISP_CLASS_OTHER     "O"
    
     * */
    
  /*  
    fprintf(stdout,"%s lid[%s] hsa [%s] pe[%s] ts[%s] ex[%s] ele[%lf] lat[%lf] lon[%lf] fcst_pt[%s] name[%s] \n",
               header, 
               station->lid,
               station->hsa, station->pe,
               station->ts, station->ex,
               station->elevation ,
               station->lat, station->lon,
               station->fcst_pt ,
               station->name);
   */
   //  fprintf(stdout,"[(%d) %lf : %ld]  ", j,  station->values[valueIndex],
   //                                  (station->start_time)
   //                                  + (valueIndex*(station->incr_time)));
     fflush(stdout);

     
												
    return;								
											
}


int getIhfsDurCodeFromShefDurCode(char shefDuration)
{
    
    /*
  0 | I       | Instantaneous
    1 | U       | 1 Minute
   15 | C       | 15 Minutes
   30 | J       | 30 Minutes
 1001 | H       | 1 Hour
 1002 | B       | 2 Hour
 1003 | T       | 3 Hour
 1004 | F       | 4 Hour
 1006 | Q       | 6 Hour
 1008 | A       | 8 Hour
 1012 | K       | 12 Hour
 1018 | L       | 18 Hour
 2001 | D       | 1 Day
 2007 | W       | 1 Week
 3001 | M       | 1 Month
 4001 | Y       | 1 Year
 5000 | Z       | Unspecified
 5001 | S       | Seasonal
 5002 | R       | Period of Record
 5004 | P       | Total Since 7 AM
 5005 | X       | Unknown
    
*/  
    
    
    int intCode = -1;
    
    
    int length = 22;
    
    char charCodeArray[22] = "IUCJHBTFQAKLDWMYZSRPX"; //21 characters that I care about
    
    int intCodeArray[22] = { 0, 1, 15, 30,
               1001, 1002, 1003, 1004, 1006, 1008, 1012, 1018, 
               2001, 2007,
               3001,
               4001,
               5000, 5001, 5002, 5004, 5005  };
               
              
    int i = 0;
    
    for (i = 0; i < length; i++)
    {
       if (shefDuration == charCodeArray[i])
       {
           intCode = intCodeArray[i];
           break;   
       }        
    }
    
    if (intCode == -1)
    {
        printf("getIhfsDurCodeFromShefDurCode(): unidentified dur code = %c\n", shefDuration);    
    }
    
    return intCode;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

