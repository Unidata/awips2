
/*******************************************************************************
* FILENAME:             pointcontrol_dispatch.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
* 	This modules is the dispatcher between the original, Ad hoc mode PDC engine and the new
*   Time Step Mode engine. 
*    MODULE 1:          get_AdHocAndTimeStepDataSourceLists 
* DESCRIPTION:          Th
*                        
* ORIGINAL AUTHOR:      Unknown
* CREATION DATE:        1/10/06
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        1/10/06     Chip Gobs          Created from new and existing functions.
********************************************************************************
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "pointcontrol_dispatch.h"
#include "pointcontrol_options.h"
#include "pointcontrol_pets.h"
#include "pointcontrol_datasource.h"
#include "pointcontrol_timestep.h"


#ifdef OLD_WAY
int get_AdHocAndTimeStepDataSourceLists ( )
{
    //char header[] = "get_AdHocAndTimeStepDataSourceLists(): ";
    int count = 0 ;
    int count2 = 0;
    int maxCount = 0;
    pc_options_struct * pc_options = get_pc_options();
   
    
    count = get_AdHocDataSourceList();
 
    count2 =  load_TimeStepTypeSourceArray();  
 
  
    maxCount = count;
    if (count2 > count)
    {
         maxCount = count2;	
    }
     
	/* now allocate instances of datasource for later use with pc_options */
	
	if ( pc_options->data_sources_chosen_array == NULL )
	{
	    pc_options->data_sources_chosen_array = (DataSource *)
	                                  malloc(sizeof(DataSource) *  maxCount);
	
	    if ( pc_options->data_sources_chosen_array == NULL )
		{
	        fprintf ( stderr , "\nIn routine 'get_AdHocAndTimeStepDataSourceLists':\n"
	                           "Could not allocate memory for the array of\n"
	                           "data sources.\n" ) ;
	        return -1 ;
		}
	
	    pc_options->data_sources_chosen_count = 0 ;
	}
	 
   return maxCount ;
}
#endif


/******************************************************************************
   get_pc_elementTypes()
   return an array of the physical element types for the ad_hoc mode
   ***************************************************************************/
void pc_engine_LoadElementTypes(QueryMode queryMode)
{
	if (queryMode == AD_HOC_MODE)
	{
		
		pc_engine_LoadAdHocElementTypes();

	}
	else
	{
		pc_loadTimeStepElementTypes();
	}	
	
}

/* ------------------------------------------------------------------------------------*/
