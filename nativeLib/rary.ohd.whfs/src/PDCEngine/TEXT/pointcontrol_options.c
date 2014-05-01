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

#include "pointcontrol_options.h"
#include "time_convert.h"


 
    				
/*
 * 
 * pc_options_struct * get_pc_options()
 * 
 * The pc_options variable used to be global and held in the HvDisplayControl.c file.
 * Now, pc_options is not global, but can be accessed as a pointer through the get_pc_options()
 * function in pointcontrol_options.c,  which is part of the engine.
 * 
 * */		
 pc_options_struct * get_pc_options()
 {
     static pc_options_struct pc_options;
     static int initialized = 0;
     
     //printf("get_pc_options()\n");
     
     if (initialized == 0)
     {
        initialized = 1;
        initialize_options(&pc_options);
     }
     
     return &pc_options;	
 	
 }		
 
 // ------------------------------------------------------------------------------------
 void initialize_options(pc_options_struct *pc_options)
 {
     printf("initialize_options() If you are seeing this more than once, something is wrong\n");
    
    
     pc_options->type_source_chosen_array = (TypeSourceString *)
                                      malloc(sizeof(TypeSourceString) *  MAX_TYPESOURCE_COUNT);
    
     if ( pc_options->type_source_chosen_array == NULL )
     {
            fprintf ( stderr , "\nIn routine 'get_pc_options() ':\n"
                               "Could not allocate memory for the array of\n"
                               "type/sources.\n" ) ;
      
     }
    
     printf("initialize_options() setting initialized to 1 \n");
 
     pc_options->type_source_chosen_count = 0 ;           
    
     return;
    
 }
 
 
 // ------------------------------------------------------------------------------------
  
	   

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
void set_pcoptions_timestring ( )
{
   	
   pc_options_struct * pc_options = get_pc_options();
   time_t       utimet ;
   struct tm    * utm = NULL ;   

   /* set the endtime */
   utimet = pc_options->valid_timet;
   utm = gmtime(&utimet);

   memset(pc_options->pc_time_str, 0, MAXLEN_TIME);
   sprintf(pc_options->pc_time_str, "%4.4d-%2.2d-%2.2d %2.2d:%2.2d",
           1900+utm->tm_year, utm->tm_mon + 1, utm->tm_mday,
           utm->tm_hour, 0);

   return ;
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
void adjust_pctimestr(char      *timestr,
                    int         dec_inc,
                    long        amount)
{
   int          y, m, d, h, mi, s=0;
   struct tm    utm;
   struct tm    *utmPtr;
   time_t       utimet;

   sscanf(timestr,
          "%4d-%2d-%2d %2d:%2d", &y, &m, &d, &h, &mi);
  
   utm.tm_sec  = s;
   utm.tm_min  = mi;
   utm.tm_hour = h;
   utm.tm_mday = d;
   utm.tm_mon  = m - 1;
   utm.tm_year = y - 1900;

   utimet = gm_mktime(&utm);

   if (dec_inc > 0)
      utimet = utimet + amount;
   else
      utimet = utimet - amount;


   utmPtr = gmtime(&utimet);

   s  = utmPtr->tm_sec;
   mi = utmPtr->tm_min;
   h  = utmPtr->tm_hour;
   d  = utmPtr->tm_mday;
   m  = utmPtr->tm_mon + 1;
   y  = utmPtr->tm_year + 1900;

   sprintf(timestr,
           "%.4d-%.2d-%.2d %.2d:%.2d", y, m, d, h, mi);
  
   return;
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

void set_pcoptions_datatype ( int datatype )
{
    
	 pc_options_struct * pc_options = get_pc_options();

    if ((datatype >= 0) &&  (datatype <= MAX_DATA_ELEMENT_TYPE))
    {
          pc_options->element_type = datatype ;
    }
    else
    {
       fprintf ( stderr , "\nIn routine 'set_pcoptions_datatype':\n"
                          "Invalid datatype: %d.\n" , datatype ) ;
    }
}

void pc_set_selected_pe ( const char * physical_element , int datatype , 
                      int selected_position )
{
   char header[] = "pc_set_selected_pe():  ";
pc_options_struct * pc_options = get_pc_options();

   pc_options->PCandPP = 0 ;
   pc_options->Primary = 0 ;

   if ( physical_element != NULL )
   {
      memset ( pc_options->selectedAdHocElementString , '\0' , SHEF_PE_LEN + 1 ) ;
      strncpy ( pc_options->selectedAdHocElementString , 
                physical_element , SHEF_PE_LEN ) ;
      
      memset ( pc_options->selectedAdHocElementFullString , '\0' , SELECTED_AD_HOC_ELEMENT_FULL_STRING_LEN + 1 ) ;
      strncpy ( pc_options->selectedAdHocElementFullString ,
                physical_element , SELECTED_AD_HOC_ELEMENT_FULL_STRING_LEN ) ;
   
   }

   printf("%s selectAdHocElementFullString =  %s \n",
          header,  pc_options->selectedAdHocElementFullString );
  


   if (pc_options->query_mode == AD_HOC_MODE)
   {
	   /* Toggle the states of the pc_options Primary and PCandPP 
	      flags. */
	   if ( selected_position == 1 && datatype == RIVER_AD_HOC_TYPE )
	   {
	      pc_options->Primary = 1 ;
	   }
	   
	   if ( selected_position == 1 && pc_options->element_type == RAIN_AD_HOC_TYPE )
	   {
	      pc_options->PCandPP = 1 ;
	   }
   }

   return ;

}

   
