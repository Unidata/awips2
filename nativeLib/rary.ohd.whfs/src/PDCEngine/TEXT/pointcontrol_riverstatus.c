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

#include <string.h> /* For BUFSIZE definition. */

#include "GeneralUtil.h"
#include "pointcontrol_derive.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_options.h"
#include "pointcontrol_pets.h"
#include "pointcontrol_report.h"
#include "pointcontrol_riverstatus.h"


/*******************************************************************************
* MODULE NUMBER:   1
* MODULE NAME:     pc_get_river_options_struct
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
//note was static
pc_options_struct * pc_get_river_options_struct ( )
{
   static int dur_hours = 24 ;
   static char stagebasis = BASIS_MOFO ;
   static int first = 1 ;
   int i ;
   static pc_options_struct pc_options_RIVER ;
   static int initialized = 0;
   
   if (initialized != 1)
   {
       initialized = 1;
       initialize_options(&pc_options_RIVER);
   }
   
   pc_options_struct * pc_options = get_pc_options();

   if ( first == 1 )
   {
      first = 0 ;

       /* Define the required members of the pc_options_RIVER structure. */
      if ( check_ShefProcObs ( ) )
      {
         pc_options_RIVER.process_mode = PROC_AS_OBS;
      }
      else
      {
         pc_options_RIVER.process_mode = PROC_AS_PROC;
      }

      pc_options_RIVER.process_selected = 0;

      pc_options_RIVER.query_mode = AD_HOC_MODE;
      pc_options_RIVER.element_type = RIVER_AD_HOC_TYPE;
      strcpy(pc_options_RIVER.selectedAdHocElementString, "xx");
      pc_options_RIVER.PCandPP = 0;
      pc_options_RIVER.Primary = 1;

      pc_options_RIVER.filter_by_typesource = 0;
     
     
   
      strcpy(pc_options_RIVER.type_source_chosen_array[0], "xx");
      pc_options_RIVER.type_source_chosen_count = 1;
      
      
      /* Time mode settings. The duration is set later in this function. */
      pc_options_RIVER.time_mode = LATEST;

      time(&pc_options_RIVER.valid_timet);
      pc_options_RIVER.dur_hours = 24;

      pc_options_RIVER.filter_by_hsa = 0 ;

      /* data source filtering */
      pc_options_RIVER.filter_by_datasource = 0;
      pc_options_RIVER.data_sources_chosen_count = 0;

      for (i = 0; i < pc_options_RIVER.data_sources_chosen_count; i++)
      {
         strcpy(pc_options_RIVER.data_sources_chosen_array[i], "dummy");
      }

      /* data filter settings. get the data for all locations,
         not just the forecast points. */
      pc_options_RIVER.suppress_missing = 0;
      /*
      pc_options_RIVER.suppress_zeros   = 0;
      */
      pc_options_RIVER.fcstpts_only = 0 ;

      /* map display options */
      pc_options_RIVER.value = 1;
      pc_options_RIVER.id   = 1;
      pc_options_RIVER.name  = 0;
      pc_options_RIVER.time  = 0;
      pc_options_RIVER.icon  = 1;
      pc_options_RIVER.fldlevel = 0;
      pc_options_RIVER.valuetype = TYPE_VALUE;

      pc_options_RIVER.stagebasis = BASIS_MOFO;
      
      pc_options_RIVER.dur_hours = 24;
   }

   /* Do not overwrite the river status duration hours and
      stage basis if currently in time step mode. */
   if ( pc_options->query_mode != TIME_STEP_MODE )
   { 
      /* August 13, 2004 - Modified this routine so that riverstatus duration
         hours and stage basis are handled in a different way. */ 
      if ( ( pc_options->selectedAdHocElementString [ 0 ] == 'H' ) ||
           ( pc_options->selectedAdHocElementString [ 0 ] == 'Q' ) ||
           ( ( int ) pc_options->Primary == 1 ) )
      {
         dur_hours = pc_options->dur_hours ;
         pc_options_RIVER.dur_hours = dur_hours ;
         stagebasis = pc_options->stagebasis ;
         pc_options_RIVER.stagebasis = stagebasis ;
      }
      else
      {
         pc_options_RIVER.dur_hours = dur_hours ;
         pc_options_RIVER.stagebasis = stagebasis ;
      }
   }
         
   return & pc_options_RIVER ;
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

static ReportList * pRiverStatusList = NULL ;

void process_river_threat_index ( ReportList * repHead , 
                                  int retrieval_required )
{
   char header[] = "process_river_threat_index(): ";
   int debug = 0;
   int stagebasis_has_changed = 0 ;
   double action_value ;
   int compare_factor ;
   double flood_value ;
   static int previous_stagebasis = 99 ; 
   double river_value ;
   pc_options_struct * pRiverOptionsStruct = NULL ;
   ReportList * pReportListNode = NULL ;
   ReportList * pRiverStatusNode = NULL ;
   rs_info_struct * pRsInfo = NULL ;
   pc_options_struct * pc_options = get_pc_options();

   /* Test to determine if there are any reports to process.  If
      not then return from this routine now. */
   if ( repHead == NULL )
   {
      /* Ciao */
      return ;
   }

   /* Does the user even want the river icons colored based on the 
      threat index? */
   if ( pc_options->riverstatus == 0 )
   {
      /* Adios */
      return ;
   }  

   /* Make sure the settings are correct in the point control
      structure which has been set aside specifically for
      the processing of river data. */
   pRiverOptionsStruct = pc_get_river_options_struct ( ) ;

   if ( pRiverOptionsStruct->stagebasis != previous_stagebasis )
   {
      previous_stagebasis = pRiverOptionsStruct->stagebasis ;
      stagebasis_has_changed = 1 ;  
   }

   /* Determine if it is necessary to retrieve the latest riverstatus 
      information */
   if ( ( pRiverStatusList == NULL ) || ( stagebasis_has_changed == 1 ) ||
        ( retrieval_required == 1 ) )
   {
      /* Free the memory (if any) currently associated with the linked list
         of RiverStatus structures. */  
      if ( pRiverStatusList != NULL )
      {
         FreeRiverStatusList ( & pRiverStatusList->list ) ; 
         pRiverStatusList = NULL ;
      }

      /* Call "pc_process_onetime" to retrieve the latest river status
         colors */
      pRiverStatusList = pc_process_onetime ( * pRiverOptionsStruct ) ;

      if ( pRiverStatusList == NULL )
      {
         /* Note that all of the threat indexes should already be set to
            missing ('Z') at this point in the report list. Just exit this
            function.  No more work should be required. */
         return ;
      }
   }

   /* For each station in the report list passed into this routine,
      check to see if it has a corresponding entry in the river data
      list.  If it does, then find the flood stage, flood flow, action
      stage, and action flow data for the station and determine the 
      threat index. */
   pReportListNode = ( ReportList * ) ListFirst ( & repHead->list ) ;
   pRiverStatusNode = ( ReportList * ) ListFirst ( 
                                       & pRiverStatusList->list ) ;

   while ( ( pReportListNode != NULL ) && ( pRiverStatusNode != NULL ) )
   {
      /* Compare the station name in the report list to the station
         name in the river status list. */
      compare_factor = strcmp ( pReportListNode->lid , 
                                pRiverStatusNode->lid ) ;
                                
   //   printf("%s pReportListNode->lid = %s  pRiverStatusNode->lid = %s compare_factor = %d\n", header,
   //   	                 pReportListNode->lid , pRiverStatusNode->lid, compare_factor);


	  debug = 0;	
      if ( compare_factor == 0 )
      {
      	
         /* Check to determine if the river station has an observed or
            forecast value. */
         river_value = pRiverStatusNode->value ;

         if ( pRiverStatusNode->pe[0] != 'H' &&
              pRiverStatusNode->pe[0] != 'Q' &&
              pRiverStatusNode->pe[0] != '-' )
         {
            printf ( "%s: Invalid river status pe: %s\n", header,
                     pRiverStatusNode->pe );
         }
         
        // if (debug)
        // {
        // 	 printf("%s lid = %s river_value = %lf\n", header,
      	//                 pReportListNode->lid , river_value);
        // }

         if ( river_value != ( double ) MISSING_VAL )
         {
            pReportListNode->threat_index = THREAT_MISSING_STAGE ;  

            /* Retrieve the river status information for this node. */
            pRsInfo = get_rs_info ( pReportListNode->lid ) ;

            if ( pRsInfo != NULL )
            {
               /* Determine if the station is reporting stage or flow. */
               if ( pRsInfo->pe[0] == 'Q' )
               {
                  /* This is a river flow station. */
                  flood_value = pRsInfo->fq ;
                  action_value = pRsInfo->aq ;
               }
               else
               {
                  /* This is a river stage station. */
                  flood_value = pRsInfo->fs ;
                  action_value = pRsInfo->as ;
                  
               }
               
             

               if ( action_value != ( double ) MISSING_VAL )
               {
                  if ( river_value < action_value )
                  {
                     pReportListNode->threat_index = THREAT_NONE ;  
                  }
                  else
                  {
                     pReportListNode->threat_index = THREAT_ACTION ;
                  }
               }

               if ( flood_value != ( double ) MISSING_VAL )
               {
                  if ( ( river_value < flood_value ) && 
                       ( action_value == ( double ) MISSING_VAL ) )
                  {
                     pReportListNode->threat_index = THREAT_NONE ;
                  }
                  else if ( river_value >= flood_value )
                  {
                  	//  if (debug)
               		//  {
               		//		printf("%s lid = %s  river_value = %lf flood_value = %lf\n",
               		//				 header, pReportListNode->lid, river_value, flood_value);
               		//  }
               		  
               		//  printf("%s lid = %s  river_value = %lf flood_value = %lf\n",
               		//				 header, pReportListNode->lid, river_value, flood_value);
                  	
                     pReportListNode->threat_index = THREAT_FLOOD ;
                  }
               }
            }

         }

         pReportListNode = ( ReportList * ) ListNext ( 
                                                & pReportListNode->node ) ;
                                                
     // as of OB7.2, we don't want to advance automatically here, because
     // we can have duplicate virtual stations in the report list, so we want the
     // second station to match what has already been found                                           
     //    pRiverStatusNode = ( ReportList * ) ListNext (
     //                                          & pRiverStatusNode->node ) ;
      }
      else if ( compare_factor < 0 )
      {
         /* Retrieve the next node in the report list. */
         pReportListNode = ( ReportList * ) ListNext ( 
                                                & pReportListNode->node ) ;
      }
      else
      {
         /* Retrieve the next report in the riverstatus list. */
         pRiverStatusNode = ( ReportList * ) ListNext (
                                               & pRiverStatusNode->node ) ;
      }
      
   }
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
void FreeRiverStatusList ( )
{
   if ( pRiverStatusList != NULL )
   {
      FreeReportList ( pRiverStatusList ) ;
      pRiverStatusList = NULL ;
   }
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
void get_floodinfo(char         *lid,
                   char         *pe,
                   float        *flood_level,
                   float        *action_level)
{
   rs_info_struct       *rs_info;
  

   /* initialize */

   *flood_level  = MISSING_VAL;
   *action_level = MISSING_VAL;

   if (pe[0] == 'H' || pe[0] == 'Q')
   {
      rs_info = get_rs_info(lid);

      if (rs_info != NULL)
      {
         if (pe[0] == 'H')
         {
            if (rs_info->fs != MISSING_VAL) *flood_level  = rs_info->fs;

            if (rs_info->as != MISSING_VAL) *action_level = rs_info->as;
         }

         else if (pe[0] == 'Q')
         {
            if (rs_info->fq != MISSING_VAL) *flood_level  = rs_info->fq;

            if (rs_info->aq != MISSING_VAL) *action_level = rs_info->aq;
         }
      }
   }

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
