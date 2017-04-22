#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pointcontrol_legend.h"
#include "time_convert.h"
#include "pointcontrol_timestep.h"


/***********************************************************************/
char * getPDCLegendString()
{   
    
    pc_options_struct * pc_options = get_pc_options();
    static char * legendString = NULL;
    
    if (pc_options->query_mode == TIME_STEP_MODE)
    {
        legendString = getPDCTimeStepLegendString(pc_options);   
      
    }
    else //AD_HOC MODE
    {
        legendString = getPDCAdHocLegendString(pc_options);   
    }
        
    
    return legendString;
}

// --------------------------------------------------------------------------
char * getPDCTimeStepLegendString(pc_options_struct * pc_options)
{
    char timestep_strings[][50] = {"Stage/Pool",
                                       "Flow/Storage",
                                       "Depth Above Flood Stage",
                                       "Percent Flood Flow",
                                       
                                       "Instantaneous Precip",
                                       "Hourly Precip Total",
                                        "3-Hour Precip Total",
                                        "6-Hour Precip Total",
                                       "24-Hour Precip Total", 
                                       
                                       "Snow-Water Equiv",
                                       "SWE 24-Hour Change",
                                       
                                       "Temperature (F)",
                                       "Temp. 24-Hour Change",
                                       "Temp. 24-Hour Max",
                                       "Temp. 24-Hour Min",
                                       
                                        "Dewpoint (F)",
                                        "Dewpoint Change (F)",
                                        "Relative Humidity",
                                        "Wind Speed",
                                        "Wind Direction"
                                        
                                        };
                                    
    char inst_precip_strings[][50] = { 
                                        "30 Min.",
                                        "1 Hour",
                                        "2 Hours",
                                        "3 Hours",
                                        "4 Hours",
                                        "6 Hours",
                                        "12 Hours",
                                        "18 Hours",
                                        "24 Hours"
                                     };
    
    char * legendString = (char *) malloc(100 * sizeof(char));
        
    TimeStepDataElementType element_type = pc_options->element_type;
    int element = pc_options->selectedTimeStepElement;
        
    
    if ((element >= 0) &&
       (element < TIME_STEP_DATA_ELEMENT_COUNT))  //element is in legal range
    {           
       if (element ==  INSTANTANEOUS_PRECIP_TSDE)
       {
           InstPrecipSelection selection = pc_options->inst_precip_accum_time_selection;
           if ((selection >= 0) &&
              (selection < PRECIP_TIME_COUNT) )
           {
            
               char date_string[80];
               timet_to_yearmin_ansi(get_timestep_file_creation_time(), date_string);
                         
               sprintf(legendString, "%s - %s ending at %s Z",
                         timestep_strings[element],
                         inst_precip_strings[selection],
                         date_string);
           }
       }
       else if (element_type ==  RAIN_TIME_STEP_TYPE) // but not inst. precip
       {
        
           sprintf(legendString, "%s ending at %s Z",
                         timestep_strings[element],
                         pc_options->pc_time_str);
       }
       else if ( (element == TEMP_MAX_TSDE) || 
                 (element == TEMP_MIN_TSDE ) ) 
       {
        
           sprintf(legendString, "%s ending near %s Z",
                         timestep_strings[element],
                         pc_options->pc_time_str);
       }
       else
       {
           sprintf(legendString, "%s for %s Z",
                         timestep_strings[element],
                         pc_options->pc_time_str);
       }
       
    }
    else //element is out of range - this would be bad, should not ever happen
    {
       strcpy(legendString, "Time Step Mode - ERROR");
    }
    
    return legendString;
    
}

//--------------------------------------------------------------------------
char * getPDCAdHocLegendString(pc_options_struct * pc_options)
{
    char header[] = "getPDCAdHocLegendString(): ";
    
    char * legendString = (char *) malloc(100 * sizeof(char));
    char timeModeStrings[][20] = 
    {
        "Latest Value",
        "Value",
        "Min Value",
        "Max Value",
        "Value Change"
    };
    
    TimeModeType time_mode = pc_options->time_mode;
    int primary = pc_options->Primary;
    char primaryString[] = "River - Primary PE";
    
    int pc_and_pp = pc_options->PCandPP;
    char pc_and_ppString[] = "Rain Total - PC AND PP";
    
    char * elementString = NULL;
        
    pc_pets_struct * petsdata = get_pc_petsdata();
    printf("%s pc_options->element_type = %d\n", header,
            pc_options->element_type);
            
    printf("%s petsdata->element_type_count = %d\n", header,
            petsdata->element_type_count);
  
    int element_type = pc_options->element_type;
    
    printf("%s element_type = %d \n", header, element_type);
    
    
    
    //make sure that we have all the normal element types   
    if (petsdata->element_type_count == 0)
    {
        pc_engine_LoadAdHocElementTypes();   
    } 
        
 
    
    //if element_type is in legal range
    if ( (element_type >= 0)  && (element_type < petsdata->element_type_count) )
    {
        if ( petsdata->elementTypeTextArray[element_type] != NULL)
        {
            
            //set up the way to display the physical element string
            if (primary)
            {
                elementString = primaryString;
            }
            else if (pc_and_pp)
            {
                elementString = pc_and_ppString;
            }
            else
            {
                elementString = pc_options->selectedAdHocElementFullString;
            }
            
            
            
            if (time_mode == LATEST)
            {  
                
                if (element_type == RAIN_AD_HOC_TYPE)
                {
                    //example RAIN - PC AND PP - Total Over last 4 hours
                    time_t current_time = 0;
                    time(&current_time);
                    current_time /= 3600;
                    current_time *= 3600;
                    char timeString[30];
                    
                    timet_to_yearsec_ansi(current_time, timeString);
                    
                    if (pc_options->dur_hours == 1)
                    {
                         sprintf(legendString, "%s over %d hour ending at latest hour (%s Z) ", 
                         elementString,
                         pc_options->dur_hours,
                         timeString);
                    }
                    else
                    {
                         sprintf(legendString, "%s over %d hours ending at latest hour (%s Z) ", 
                             elementString,
                             pc_options->dur_hours,
                             timeString);
                    } 
                }
                else // not RAIN_AD_HOC_TYPE
                {
                    //example River - Primary - Latest Value
                    sprintf(legendString, "%s - latest value within last %d hours", 
                            elementString,
                            pc_options->dur_hours);
                }
            }
            
            
            else if ((time_mode == MINSELECT) ||
                     (time_mode == MAXSELECT) ||
                     (time_mode == VALUE_CHANGE))
            {           
                if (element_type == RAIN_AD_HOC_TYPE)
                {
                    //example RAIN - PC AND PP - value over 4 hours ending at 2006 03 17 12:00 Z
                    sprintf(legendString, "%s over %d hours ending at %s", 
                     //   petsdata->elementTypeTextArray[element_type],
                        elementString,
                        pc_options->dur_hours,
                        pc_options->pc_time_str);
                }   
                else //not RAIN_AD_HOC_TYPE
                {
                    //example River Primary PE - Min value over 4 hours ending at 2006 03 17 12:00 Z
                    sprintf(legendString, "%s - %s over %d hours ending at %s", 
                        //petsdata->elementTypeTextArray[element_type],
                        elementString,
                        timeModeStrings[time_mode],
                        pc_options->dur_hours,
                        pc_options->pc_time_str);
                }
                
            }
            
            else // time_mode = SETTIME
            {
                if (element_type == RAIN_AD_HOC_TYPE)
                {
                    //example RAIN - PC AND PP - Value over 4 hours ending at 2006 03 17 12:00 Z
           
                    sprintf(legendString, "%s over %d hours ending at %s", 
                       // petsdata->elementTypeTextArray[element_type],
                        elementString,
                        pc_options->dur_hours,
                        pc_options->pc_time_str);
                     
                }
                else //not RAIN_AD_HOC_TYPE
                {
                    sprintf(legendString, "%s within %d hours of %s", 
                        //petsdata->elementTypeTextArray[element_type],
                        elementString,
                        pc_options->dur_hours,
                        pc_options->pc_time_str
                       );
                }
            }
        } 
    }   
    else // element out of range , happens upon startup
    {
        printf("%s element out of range \n", header);
        strcpy(legendString, "");
        
    }
    
    return legendString;
    

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

//---------------------------------------------------------------------------------------------

