/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
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

#include <stdio.h>
#include <string.h>

#include "pointcontrol_presets.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_options.h"
#include "pointcontrol_datasource.h"
#include "pointcontrol_dispatch.h"
#include "pointcontrol_timestep.h"
#include "PointDataPresets.h"
#include "TokenizeOptionString.h"
#include "GeneralUtil.h"
#include "BinarySearch.h"

enum PCoptions {  InstantaneousPrecipAccumTime,  
	             SelectedTypeSources /*was CurTs */ , DataType , DeriveStageFlow , DurHours , 
			     FilterByDataSource , DateTime , ElevFilterOperation, ElevFilterValue ,
			     FloodLevel , FcstOnly , ShowIcon , ShowId ,
			     ShowName , NumSources , SelectedAdHocPE /*was CurPE  */ , PCandPP ,
			     Primary , ProcessSelected , SelectedQueryMode , ShowRiverStatus ,
			     StageBasis , ShowElevation, StreamStationFilter,
			     SuppressMissing , ShowParamCode ,
			     SourceList ,  SelectedTimeStepElement, ServiceBackup , SuppressZero ,
			     ShowTime , TimeMode , TimeStepPrecipPeFilter,
                 FilterByTypeSource , ShowValue ,
                
			     ValueFilterOperation , ValueType , ValueFilterValue , WFOlist ,
                 NumPCoptions } ;

//These MUST be in alphabetical order here !!! It will break otherwise!!!
//Also, they must be in the same order as their corresponding enum value
//They are currently arranged on each line to make their matches easier to find.
static const char * PCoptionStrings [ NumPCoptions ]
                                        = { "at" ,
                                        	"ct" , "da" , "de" , "dh" ,
                                        	"ds" , "dt" , "eo" , "ev" ,
                                        	"fl" , "fo" , "ic" , "id" , 
                                        	"nm" , "ns" , "pe" , "pp" ,
                                        	"pr" , "ps" , "qm" , "rs" ,
                                        	"sb" , "se",  "sf" ,
                                            "sm" , "sp" ,
                                        	"sr" , "st" , "sv" , "sz" , 
                                        	"ti" , "tm" , "tp",
                                            "ts" , "vl", 
                                        	"vo" , "vt" , "vv" , "wf" } ;

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
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
/****************************************************************************
  This is a wrapper routine around the GetPointDataPresets dbgen routine.
  It has been implemented because the hv_areal library also needs to
  access the PointDataPresets table, but it should not have direct
  access to the pPresetHead variable in PointControl.  At the same time,
  the PointDataPresets table should be read as infrequently as possible,
  so whoever accesses this table should keep the linked list pointed to
  by pPresetHead up to date.  This accessor function permits this.
*****************************************************************************/

/* This is the head pointer to the linked list of PointDataPresets
   structures. */
static  PointDataPresets * pPresetHead = NULL ;

PointDataPresets * get_PointDataPresetsList ( )
{
   static char where [ ] = " ORDER BY preset_rank ASC";

   /* Retrieve the option sets from the PointDataPresets table. */
   pPresetHead = GetPointDataPresets ( where ) ;

   return pPresetHead ;
}

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
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
PointDataPresets * get_PointDataPresetsHead ( )
{
   return pPresetHead ;
}

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
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

void free_PointDataPresets ( )
{
   FreePointDataPresets ( pPresetHead ) ;
   pPresetHead = NULL ;
}

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
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
int set_pc_options_using_presets ( int pos )
{
   int status = 0 ;
   OptionValuePair * pOptionValuePair = NULL ;
   PointDataPresets * pPresetHead = NULL ;
   PointDataPresets * pPresetNode = NULL ;

   pPresetHead = get_PointDataPresetsHead ( ) ;

   if ( pPresetHead != NULL )
   {
      pPresetNode = ( PointDataPresets * ) ListNth ( & pPresetHead->list ,
                                                     pos ) ;
      if ( pPresetNode == NULL )
      {
         fprintf ( stderr , "\nIn routine 'set_pc_options_using_presets':\n"
                            "Could not retrieve node number %d from the\n"
                            "PointDataPreset linked list.\n" , pos ) ;
         status = -1 ;
      }
      else
      {
      	 printf("\n*************************\n set_pc_options_using_presets(): %s\n ********************\n",pPresetNode->preset_string); 
      	
         /* Parse the option string and set the point control settings to
            reflect the predefined options. */
         status = TokenizeOptionString ( pPresetNode->preset_string ,
                                         & pOptionValuePair ) ;
 
         if ( status != TOKENIZE_OPTION_OK )
         {
            fprintf ( stderr , "\nIn routine 'set_pc_options_using_presets':\n"
                      "An error was encountered by TokenizeOptionString."
                      "\nError code is %d.  Cannot process Preset Option"
                      "\nid %s.\n" , status , pPresetNode->preset_id ) ;
            status = -1 ;
         }
         else
         {
            set_pc_options ( pOptionValuePair) ;
         }

         if ( pOptionValuePair != NULL )
         {
            FreeOptionValueList ( & pOptionValuePair ) ;
            pOptionValuePair = NULL ;
         }
      }
   }
   else
   {
      status = -1 ;
   }

   return status ;
}

/* --------------------------------------------------------------------------------------*/
/********************************************************************
   Set the point control options to their defaults.
   There are three tokens that allow values to be specified
   externally: 
   hv_durhours=#
   hv_riverbasis=obds|fcst|maxobsfcst
   hv_pets=PRIMARY|PCPP|<pe>|<pets> where pe for H,Q,P,T,S only
   
   *****************************************************************/

void set_pc_options ( OptionValuePair * pOptionValuePair )
{
   char header[] = "set_pc_options() ";
   /* Even if there is an option value list to process,
      first set all of the members of the pc_options_struct to a default 
      value.  This needs to be done just in case not all of the options
      are specified in the predefined option set.  We need to make sure
      that all options have a valid value. */

   /* Set the pe[ts] specification to PRIMARY. Note that this used to be
      retrieved from the "hv_pets" token. */
   /* Default is to use River data for Primary PE, highest ranked TS */

   pc_options_struct * pc_options = get_pc_options();

   pc_options->element_type = RIVER_AD_HOC_TYPE ;
   strcpy(pc_options->selectedAdHocElementString, "") ;
   pc_options->Primary = 1 ;
   pc_options->PCandPP = 0 ;
      
   pc_options->filter_by_typesource = 0 ;
   pc_options->type_source_chosen_count = 0;
   /*
   strcpy(pc_options->type_sources_chosen, "") ;
   */
   
   /* Set the shef processing mode based on the shef_procobs token.
      This determines whether to comingle the processed data with the
      observations or to treat them separately. */
   if (check_ShefProcObs())
      pc_options->process_mode = PROC_AS_OBS ;
   else
      pc_options->process_mode = PROC_AS_PROC ;

   pc_options->process_selected = 0 ;

   /* Time mode settings. The duration used to be read from the 
      hv_durhours token.  Now it is given a default value of 24. */
   pc_options->time_mode = LATEST ;
   time(&pc_options->valid_timet) ;
   pc_options->dur_hours = 24 ;
   
 //  printf("%s pc_options->valid_timet = %ld \n", header, pc_options->valid_timet);
   
   
   /* No support provided for predefining the filter by source options. */
   pc_options->filter_by_datasource = 0 ;
   pc_options->data_sources_chosen_count = 0 ;
  
   /* Make sure that by default filtering by service backup is not
      enabled. */ 
   pc_options->filter_by_hsa = 0 ;
   memset ( pc_options->hsa_list , '\0' , MAXLEN_AREALIST ) ;

   /* Data filter settings */
   pc_options->suppress_missing = 0 ;
   /*
   pc_options->suppress_zeros   = 0 ;
   */
   pc_options->fcstpts_only     = 1 ;
   
   /* map display options */
   pc_options->value = 1 ;
   pc_options->id    = 1 ;
   pc_options->name  = 0 ;
   pc_options->time  = 0 ;
   pc_options->icon  = 1 ;
   
   pc_options->fldlevel = 0 ;
   pc_options->valuetype = TYPE_VALUE ;
   
   /* Set the river basis to BASIS_MOFO as the default.
      This used to be read from the hv_riverbasis token.
      Now this token is only used to set the riverstatus basis used
      to color the gauge icons. */
   pc_options->stagebasis = BASIS_MOFO ;

   /* Determine the initial state of the display river status toggle. */
   pc_options->riverstatus = ( char ) is_riverstatus_displayed ( ) ;
	 
   printf("%s datatype=%d;curpe=%2s;Primary/PCandPP/TS=%d/%d/%d;\n"
	  "dur=%d;basis=%d\n", header,
	  pc_options->element_type, pc_options->selectedAdHocElementString,
	  pc_options->Primary, pc_options->PCandPP,
	  pc_options->filter_by_typesource,
	  pc_options->dur_hours, pc_options->stagebasis);  
      
   if ( pOptionValuePair != NULL )
   {
      pc_set_options_from_optionvalue_pairs ( pOptionValuePair ) ;
   }

   return;
}

/***********************************************************************/
static int compare_preset_ids ( void * id_to_compare , 
                                void * id_in_array )
{
   char * string_to_compare = ( char * ) id_to_compare ;
   char ** string_in_array = ( char ** ) id_in_array ;

   return strcmp ( string_to_compare , * string_in_array ) ;
}

/* --------------------------------------------------------------------------------------*/
void pc_set_options_from_optionvalue_pairs ( OptionValuePair * pOptionValuePair )
{
   char header[] = "pc_set_options_from_optionvalue_pairs(): ";
   char * hours_minutes = NULL ;
   char ** option_name = NULL ;
   char * pColon = NULL ;
   int count ;
   int hours = 0 ;
   int i ;
   int min_count ;
   int minutes = 0 ;
   int number_of_days = 0 ;
   int value_length ;
   int hsa_list_length ;
   OptionValuePair * pOptionNode = NULL ;
   time_t current_time ;
   struct tm * pTmStruct = NULL ;
   pc_options_struct * pc_options = get_pc_options();

   /* A linked list of one or more OptionValuePair structures has
      been passed into this routine. */
   pOptionNode = ( OptionValuePair * ) ListFirst 
                                       ( & pOptionValuePair->list ) ; 

   while ( pOptionNode != NULL )
   {
      option_name = ( char ** ) binary_search ( PCoptionStrings , 
                                                pOptionNode->option_name , 
                                                NumPCoptions , 
                                                sizeof ( char * ) ,
                                                compare_preset_ids ) ;

      if ( option_name == NULL )
      {
         fprintf ( stderr , "\nIn routine 'pc_process_optionvalue_pairs':\n"
                            "%s is not a valid pointcontrol option\n"
                            "identifier. It has been skipped.\n" , 
                             pOptionNode->option_name ) ;
      }
      else
      {
         for ( i = 0 ; i < NumPCoptions ; ++ i )
         {
            if ( strcmp ( PCoptionStrings [ i ] , * option_name ) == 0 ) break ;
         }

         switch ( i )
         {
         	
         	case InstantaneousPrecipAccumTime :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->inst_precip_accum_time_selection = ( char )
                    atoi ( pOptionNode->value_string [ 0 ] ) ;
                    
                  printf("%s pc_options->inst_precip_accum_time = :%d: \n", 
                  		 header, pc_options->inst_precip_accum_time_selection);  
               }
        
               break ;
         	
            case SelectedTypeSources :           
               
               if ( pOptionNode->number_of_values > 0 )
               {
               	
        		   count = pOptionNode->number_of_values;

                   for ( i = 0 ; i < count; i++  )
                   {
                       memset ( pc_options->type_source_chosen_array [ i ] , '\0' ,
                                TYPE_SOURCE_STRING_LEN + 1) ;
                       strncpy ( pc_options->type_source_chosen_array [ i ] ,
                                 pOptionNode->value_string [ i ] ,
                                 TYPE_SOURCE_STRING_LEN ) ;
                   }

                  pc_options->type_source_chosen_count = count ;
               }
               

               break ;

            case DataType :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->element_type = ( char )
                    atoi ( pOptionNode->value_string [ 0 ] ) ;
               }
        
               break ;

            case DeriveStageFlow :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->derive_stage_flow = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case DurHours :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->dur_hours =
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case FilterByDataSource :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->filter_by_datasource = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case DateTime :

               printf("%s \n", header);

               if ( pOptionNode->number_of_values == 2 )
               {
                  number_of_days = atoi ( pOptionNode->value_string [ 0 ] ) ;
                  value_length = strlen ( pOptionNode->value_string [ 1 ] ) ;
                 
                  if ( value_length > 0 )
                  {
                     hours_minutes = ( char * ) malloc ( sizeof ( char ) *
                                                         value_length + 1 ) ; 

                     if ( hours_minutes != NULL )
                     {
                        strcpy ( hours_minutes , 
                                 pOptionNode->value_string [ 1 ] ) ;
                        pColon = strtok ( hours_minutes , ":" ) ; 
     
                        if ( pColon != NULL )
                        {
                           hours = atoi ( pColon ) ;

                           pColon = strtok ( NULL , ":" ) ;
                     
                           if ( pColon != NULL )
                           {
                              minutes = atoi ( pColon ) ; 
                           }
                        }

                        time ( & current_time ) ;
                        pTmStruct = gmtime ( & current_time ) ;
             
                        /* Set the hours and minutes to those retrieved
                           from the presets group. Hours and minutes
                           are absolute while the day is relative. */
                        pTmStruct->tm_hour = hours ;
                        pTmStruct->tm_min = minutes ;
                        pTmStruct->tm_sec = 0 ;
                        current_time = gm_mktime ( pTmStruct ) ;
                        current_time += number_of_days * 24 * 60 * 60 ;
                         
                        pc_options->valid_timet = current_time ;

                        free ( hours_minutes ) ;
                        hours_minutes = NULL ;
                     }
                  }
               }
               
               break ;
               
               
               
               case ElevFilterOperation:
            
               if ( pOptionNode->number_of_values > 0 )
               {
                    pc_options->elevFilterOperation =
                        atoi( (char *) pOptionNode->value_string[0] );
                           
                    if  (
                            ( (int) pc_options->elevFilterOperation < SHOW_ALL) ||
                            ( (int) pc_options->elevFilterOperation >= NUM_FILTER_OPERATIONS)
                        )
                        
                    {
                        pc_options->elevFilterOperation = SHOW_ALL;
                    }              
               }
               else
               {
                   pc_options->elevFilterOperation = SHOW_ALL;
               }
                              
               break;
               
               
            case ElevFilterValue:
            
               if ( pOptionNode->number_of_values > 0 )
               { 
               	 
            	    pc_options->elevFilterValue = atof( (char *) pOptionNode->value_string [0]);
            	    printf("%s pc_options->elevFilterValue = %f  pOptionNode->value_string = :%s: \n",
            	          header,
            	          pc_options->elevFilterValue,
            	          (char *) pOptionNode->value_string[0]
            	          );
               }
                
               break;

            case FloodLevel :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->fldlevel = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;
       
            case FcstOnly :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->fcstpts_only = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case ShowIcon :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->icon = ( char )
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case ShowId :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->id = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case ShowName :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->name = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case NumSources :


               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->data_sources_chosen_count = 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case SelectedAdHocPE :

               if ( pOptionNode->number_of_values > 0 )
               {
                  memset ( pc_options->selectedAdHocElementString , '\0' , SHEF_PE_LEN + 1 ) ;
                  strncpy ( pc_options->selectedAdHocElementString , 
                            pOptionNode->value_string [ 0 ] , 
                            SHEF_PE_LEN ) ;
               }

               break ;

            case PCandPP :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->PCandPP = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case Primary :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->Primary = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case ProcessSelected :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->process_selected = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;
               
            case SelectedQueryMode :
            
               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->query_mode	= (char) atoi(pOptionNode->value_string[0]);
               //   printf("%s pc_options->queryMode = %d \n", header, pc_options->query_mode);
                  if (pc_options->query_mode != TIME_STEP_MODE)
                  {
                  	 pc_options->query_mode = AD_HOC_MODE;
                  }
              //    if (DISABLED_TIME_STEP)
             //     {
            //      	 pc_options->query_mode = AD_HOC_MODE;
            //      }
                  
                  /* 0 is Ad_hoc mode, 1 is time-step mode */
               }
			   break;
			   
            case ShowRiverStatus :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->riverstatus = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case StageBasis :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->stagebasis = ( char )
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;
               
            case ShowElevation :
               
               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->elevation =  
                  			 (char) atoi ( pOptionNode->value_string [ 0 ]) ;  	
               }	
               break;

		    case StreamStationFilter :
               
               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->river_station_filter =  
                  			 (char) atoi ( pOptionNode->value_string [ 0 ]) ;  	
               }	
               break;


            case SuppressMissing :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->suppress_missing = ( char )
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;
               
            case ShowParamCode :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->paramCode = ( char )
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;
               

            case SourceList :

               if ( pOptionNode->number_of_values > 0 )
               {
                  count = get_AdHocDataSourceList( );

                  if ( count < pOptionNode->number_of_values )
                  {
                     min_count = count;
                  }
                  else
                  {
                     min_count = pOptionNode->number_of_values;
                  }

                  for ( i = 0 ; i < min_count; i++  )
                  {
                     memset ( pc_options->data_sources_chosen_array [ i ] , '\0' ,
                              DATA_SOURCE_LEN + 1 ) ;
                     strncpy ( pc_options->data_sources_chosen_array [ i ] ,
                               pOptionNode->value_string [ i ] ,
                               DATA_SOURCE_LEN ) ;
                  }

                  pc_options->data_sources_chosen_count = min_count ;
              
               }

               break ;
               
               
            case SelectedTimeStepElement:
            {
                if ( pOptionNode->number_of_values > 0 )
                {
                	pc_options->selectedTimeStepElement = atoi(pOptionNode->value_string [ 0 ] );
                }	
            	
               break;	
            }

            case ServiceBackup :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->filter_by_hsa = ( char )
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case SuppressZero :

               if ( pOptionNode->number_of_values > 0 )
               {
                  char suppressZero = ( char )
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
                            
                  /*
                   * This suppressZero options has been replaced by value filtering.
                   * This case is for backward compatibility with older presets saved in the
                   * database.
                   * */
                  if (suppressZero)
                  {
                      pc_options->valueFilterOperation = SHOW_NOT_EQUAL;
                      pc_options->valueFilterValue = 0.0;	
                  }
               }

               break ;

            case ShowTime :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->time = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;

            case TimeMode :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->time_mode = ( char )
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
                  // printf("%s pc_options->lsminmax = %c \n", header, pc_options->time_mode);
               }

               break ;
               
             case TimeStepPrecipPeFilter :
               
               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->precip_pe_filter =  
                             (char) atoi ( pOptionNode->value_string [ 0 ]) ;   
               }    
               break;
                   

            case FilterByTypeSource :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->filter_by_typesource = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

  			
               break ;
    
    
            case ShowValue :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->value = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;
                          
           case ValueFilterOperation :
                if ( pOptionNode->number_of_values > 0 )
                {
                    pc_options->valueFilterOperation =
                       atoi((char *)pOptionNode->value_string[0]);  
  
                    if  (
                         ( (int) pc_options->valueFilterOperation < SHOW_ALL) ||
                         ( (int) pc_options->valueFilterOperation >= NUM_FILTER_OPERATIONS)
                        )
                    {
                   
                        pc_options->valueFilterOperation = SHOW_ALL;
                    }
                }
                else
                {
                     pc_options->valueFilterOperation = SHOW_ALL;
                }
                             
               break;

            case ValueType :

               if ( pOptionNode->number_of_values > 0 )
               {
                  pc_options->valuetype = ( char ) 
                             atoi ( pOptionNode->value_string [ 0 ] ) ;
               }

               break ;
               
            case ValueFilterValue :
                if ( pOptionNode->number_of_values > 0 )
                {
            		pc_options->valueFilterValue = atof( (char *)pOptionNode->value_string [0]);	
                }
                
               break;    
               

            case WFOlist :

               hsa_list_length = 0 ;

               if (pOptionNode->number_of_values > 0)
               {
                  memset ( pc_options->hsa_list , '\0' , MAXLEN_AREALIST ) ;

                  for ( i = 0 ; i < pOptionNode->number_of_values ; ++ i )
                  {
                     value_length =
                           strlen ( pOptionNode->value_string [ i ] ) ;

                     /* Add the length of the hsa id plus one for a trailing
                        space. */
                     hsa_list_length += value_length + 1 ;

                     if ( hsa_list_length > MAXLEN_AREALIST )
                     {
                        break ;
                     }

                     strcat ( pc_options->hsa_list ,
                              pOptionNode->value_string [ i ] ) ; 
                     strcat ( pc_options->hsa_list , " " ) ;
                  }

                  pc_options->hsa_list [ MAXLEN_AREALIST - 1  ] = '\0' ;

               }
               else
               {
                  strcpy ( pc_options->hsa_list , ALL_AREAS ) ;
               }

               set_serv_bkup_info ( pc_options->hsa_list ) ;

               break ;

            default :
          
               fprintf ( stderr , "\nIn routine "
                                  "'pc_process_optionvalue_pairs':\n"
                                  "Unrecognized option name %s.\n" ,
                                  * option_name ) ;

               break ;
         
         }
      }
         
      pOptionNode = ( OptionValuePair * ) ListNext ( & pOptionNode->node ) ;
   }
}

/* --------------------------------------------------------------------------------------*/


char * build_pointdata_preset_string_from_options ( )
{
   char header[] = "build_pointdata_preset_string_from_options(): ";
   char * copy_string = NULL ;
   char * pCharToken = NULL ;
   char * pComma = NULL ;
   char * preset_string = NULL ;
   char option_values_string [ BUFSIZ ] ;
   int data_hours ;
   int data_minutes ;
   int i ;
   int j ;
   int option_values_length ;
   int preset_string_length = 0 ;
   int relative_days ;
   int relative_days_in_seconds ;
   int hsa_list_length ;
   int use_special_date_time_string = 0;
   time_t current_time ;
   time_t current_day_midnight_time ;
   time_t data_time ;
   struct tm * pCurrentTm = NULL ;
   struct tm * pDataTm = NULL ;
   TimeStepDataElement element = -1;
   pc_options_struct * pc_options = get_pc_options();

   /* get the service backup information and load into the options
      structure. */
   get_serv_bkup_info ( pc_options->hsa_list ) ;

   /* Allocate memory for the preset_string. */
   preset_string = ( char * ) malloc ( ( DOUBLE_REMK_LEN + 1 ) *
                                       sizeof ( char ) ) ;

   if ( preset_string == NULL )
   {
      return NULL ;
   }

   memset ( preset_string , '\0' , DOUBLE_REMK_LEN + 1 ) ;

   for ( i = 0 ; i < NumPCoptions ; ++ i )
   {
      switch ( i )
      {
      	
      	 case InstantaneousPrecipAccumTime :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->inst_precip_accum_time_selection) ;
                      
          //  printf("%s pc_options->inst_precip_accum_time = :%d: \n", 
         //         		 header, pc_options->inst_precip_accum_time);  
         break ;
                 
      	
      	
         case SelectedTypeSources :
/*
            sprintf ( option_values_string , "%s=%s;" ,
                      PCoptionStrings [ i ] , pc_options->selectedTypeSrc ) ;
            break;
*/            
            if ( pc_options->type_source_chosen_count > 0 )
            {
               /* Update the selected datasource information. */

               sprintf ( option_values_string , "%s=" ,
                         PCoptionStrings [ i ] ) ;                 

               for ( j = 0 ; j < pc_options->type_source_chosen_count ; j++ )
               {
                  if ( j > 0 )
                  {
                     strcat ( option_values_string , "," ) ;
                  }
 
                  strcat ( option_values_string , 
                           pc_options->type_source_chosen_array [ j ] ) ;
               } 

               strcat ( option_values_string , ";" ) ;
            }
            else
            {
               continue ;
            }

            break ;

         case DataType :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->element_type ) ;
            break ;

         case DeriveStageFlow :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , 
                      ( int ) pc_options->derive_stage_flow ) ;
            break ;

         case DurHours :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , pc_options->dur_hours ) ;
            break ;

         case FilterByDataSource :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] ,
                      ( int ) pc_options->filter_by_datasource ) ;
            break ;

         case DateTime :
     
            /* The processing of the Point Data Preset date time
               was modified to use a relative day and an absolute
               time. Get the current time in seconds. */
            time ( & current_time ) ;
            pCurrentTm = gmtime ( & current_time ) ; 
   
            /* Set the current hour, minute and second to zero. */
            pCurrentTm->tm_hour = 0 ;
            pCurrentTm->tm_min = 0 ;
            pCurrentTm->tm_sec = 0 ;

            /* Recreate the current time with the modified hour, minute
               and second values. */
           current_day_midnight_time = mktime ( pCurrentTm ) ;

            /* Retrieve the data time. */
            data_time = pc_options->valid_timet ;
            pDataTm = gmtime ( & data_time ) ;

            /* Store the hour and minute of the data time. */
            data_hours = pDataTm->tm_hour ;
            data_minutes = pDataTm->tm_min ;

            /* Set the data hour, minute and second to zero. */
            pDataTm->tm_hour = 0 ;
            pDataTm->tm_min = 0 ;
            pDataTm->tm_sec = 0 ;

            /* Recreate the data time with the modified hour, minute
               and second values. */
            data_time = mktime ( pDataTm ) ;

            /* Subtract the modifed data time from the modified current time
               to get the relative number of days. */
            relative_days_in_seconds = data_time - current_day_midnight_time ;

            relative_days = relative_days_in_seconds / ( 24 * 60 * 60 ) ;


            //if in Time STEP MODE and
            // the hour the user is trying to save as a preset is the
            // current top of hour, then don't save any time at all
            if (pc_options->query_mode == TIME_STEP_MODE)
            {
                long topOfHour = current_time / SECONDS_PER_HOUR;
                topOfHour *= SECONDS_PER_HOUR;
                
                printf("%s  pc_options->valid_timet = %ld topOfHour = %ld \n", 
                        header, pc_options->valid_timet, topOfHour);
                
                if (pc_options->valid_timet == topOfHour)
                {
                    use_special_date_time_string = 1;
                }    
            }

            if (use_special_date_time_string) //this means the latest time
            {
                sprintf ( option_values_string , "%s=;" ,
                      PCoptionStrings [ i ]);
            }
            else  //use normal data time string
            {
                sprintf ( option_values_string , "%s=%d,%d:%d;" ,
                      PCoptionStrings [ i ] , relative_days , data_hours ,
                      data_minutes ) ;
            }
                      
            printf("%s option_values_string = :%s:\n", header, option_values_string);          
                      
            break ;
            
         case ElevFilterOperation:
         	  sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->elevFilterOperation ) ;
            break ;
     
 		 case ElevFilterValue:
         	  sprintf ( option_values_string , "%s=%6.2f;" ,
                      PCoptionStrings [ i ] , pc_options->elevFilterValue) ;
            break ;
     
         case FloodLevel :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , (int) pc_options->fldlevel ) ;
            break ;

         case FcstOnly :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , 
                      ( int ) pc_options->fcstpts_only ) ;
            break ;

         case ShowIcon :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->icon ) ;
            break ;

         case ShowId :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->id ) ;
            break ;


         case ShowName :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->name ) ;
            break ;

         case NumSources :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] ,  
                      pc_options->data_sources_chosen_count ) ;
            break ;

         case SelectedAdHocPE :

            sprintf ( option_values_string , "%s=%2s;" ,
                      PCoptionStrings [ i ] , pc_options->selectedAdHocElementString ) ;
            break ;
            
       
         case PCandPP :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->PCandPP ) ;
            break ;

         case Primary :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->Primary ) ;
            break ;

         case ProcessSelected :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , 
                      ( int ) pc_options->process_selected ) ;
            break ;
            
         case SelectedQueryMode :
         
         	sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , 
                      ( int ) pc_options->query_mode ) ;
         	break;

         case ShowRiverStatus :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , 
                      ( int ) pc_options->riverstatus ) ;
            break ;

         case StageBasis :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->stagebasis ) ;
            break ;
            
            
         case ShowElevation :
            
            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->elevation ) ;
         
            break;
            
         case StreamStationFilter :
            
            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->river_station_filter ) ;
         
            break;

         case SuppressMissing :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , 
                      ( int ) pc_options->suppress_missing ) ;
            break ;
            
         case ShowParamCode :
            
            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->paramCode ) ;
         
            break;   

         case SourceList :

            if ( pc_options->data_sources_chosen_count > 0 )
            {
               /* Update the selected datasource information. */

               sprintf ( option_values_string , "%s=" ,
                         PCoptionStrings [ i ] ) ;

               for ( j = 0 ; j < pc_options->data_sources_chosen_count ; ++ j )
               {
                  if ( j > 0 )
                  {
                     strcat ( option_values_string , "," ) ;
                  }
 
                  strcat ( option_values_string , 
                           pc_options->data_sources_chosen_array [ j ] ) ;
               } 

               strcat ( option_values_string , ";" ) ;
            }
            else
            {
               continue ;
            }

            break ;
            
  
         case SelectedTimeStepElement :
          
            element = pc_options->selectedTimeStepElement;
          
            printf("%s element = %d\n", header, element);
            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] ,(int) element ) ;
            break ;
          
         case ServiceBackup :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , 
                      ( int ) pc_options->filter_by_hsa ) ;
            break ;
/*
         case SuppressZero :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , 
                      ( int ) pc_options->suppress_zeros ) ;
            break ;
*/
         case ShowTime :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->time ) ;
            break ;

         case TimeMode :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->time_mode ) ;
            printf("%s TimeMode option_values_string = %s\n", header, option_values_string);
            break ;
            
         case TimeStepPrecipPeFilter :
            
            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->precip_pe_filter ) ;
         
            break;

         case FilterByTypeSource :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->filter_by_typesource ) ;
            break ;

         case ShowValue :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->value ) ;
            break ;
            
            
         case ValueFilterOperation :
            
            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->valueFilterOperation) ;
            break;

         case ValueType :

            sprintf ( option_values_string , "%s=%d;" ,
                      PCoptionStrings [ i ] , ( int ) pc_options->valuetype ) ;
            break ;
            
         case ValueFilterValue :
            
            sprintf ( option_values_string , "%s=%6.2f;" ,
                      PCoptionStrings [ i ] , pc_options->valueFilterValue) ;
            break;

         case WFOlist :

            hsa_list_length = strlen ( pc_options->hsa_list ) ;

            if ( hsa_list_length > 0 )
            {
               copy_string = ( char * ) malloc ( hsa_list_length * 
                                                  sizeof ( char ) + 1 ) ;

               if ( copy_string != NULL )
               {
                  sprintf ( option_values_string , "%s=" ,
                            PCoptionStrings [ i ] ) ; 

                  strcpy ( copy_string , pc_options->hsa_list ) ;
                  pCharToken = strtok ( copy_string , " " ) ;

                  while ( pCharToken != NULL )
                  {
                     strcat ( option_values_string , pCharToken ) ;
                     strcat ( option_values_string , "," ) ;
                     pCharToken = strtok ( NULL , " " ) ;
                  }

                  pComma = strrchr ( option_values_string , ',' ) ;
                  * pComma = ';' ;

                  free ( copy_string ) ;
                  copy_string = NULL ;
               }
            }
            else
            {
               continue ;
            }
              
            break ;

         default :

            break ;
      }

      option_values_length = strlen ( option_values_string ) ;

      preset_string_length += option_values_length ;

      if ( preset_string_length > DOUBLE_REMK_LEN )
      {
         fprintf ( stderr , "\nIn routine 'build_pointdata_preset_string':\n"
                            "The preset group is too long.  It will be %d\n"
                            "characters long after the addition of '%s',\n"
                            "but is only allowed to be %d characters long.\n"
                            "The contents of the group before it exceeded\n"
                            "the limit was '%s'.\n" , preset_string_length ,
                             option_values_string , DOUBLE_REMK_LEN ,
                             preset_string ) ;
         if ( preset_string != NULL )
         {
            free ( preset_string ) ;
            preset_string = NULL ;
         }
        
         return NULL ;
      }

      strcat ( preset_string , option_values_string ) ;
   }
   
   printf("%s final preset_string = %s\n", header, preset_string);

   return preset_string ;
}


/* --------------------------------------------------------------------------------------*/

/*
FilterOperation getFilterOperationFromPresetString(const char * presetCodeString)
{
	
	  char filterStringArray[NUM_FILTER_OPERATIONS][4] = { "*", "=", "!=", ">=", "<=", ">", "<" };
  
      FilterOperation operation = -1;
      int i = 0;
           
      for (i = 0; i < NUM_FILTER_OPERATIONS; i++)
      {
      		   if (strcmp(presetCodeString, filterStringArray[i]) == 0)
               {
               		 operation = i;
               		 break;
               }	
      }
               
      return operation;
}
*/
/* --------------------------------------------------------------------------------------*/

