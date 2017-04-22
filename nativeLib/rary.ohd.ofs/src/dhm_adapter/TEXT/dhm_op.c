/*.......................................
* File: dhm_op.c
* Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
* Date Created: 5/5/06
* Development group: OHD HSEB
* Purpose: This subroutine call runDHM() and get the message if any
* Module(s): 
*     dhm_op
* Module(s) Used:
*     load_dhm_mod_string
* Modified:
* 2/07 uses call to load_dhm_mod_string to get DPRECIP and  DSACST mods 
* all mods are passed to Java Runner through a single string to the run method 
* 3/07 pulled references to upstream basin id and timeseries out of run_dhm
* this information now loaded before executing run method */
#include "dhm_mods.h"
extern int totalNumberPrecipModsFound;
extern int totalNumberSacStateModsFound;
void dhm_op(
    int *startMonthOfRunPeriod, 
    int *startDayOfRunPeriod, 
    int *startYearOfRunPeriod, 
    int *startHourOfRunPeriod,
    int *startMonthOfForecastPeriod,
    int *startDayOfForecastPeriod,
    int *startYearOfForecastPeriod,
    int *startHourOfForecastPeriod,
    int *endMonthOfRunPeriod,
    int *endDayOfRunPeriod,
    int *endYearOfRunPeriod,
    int *endHourOfRunPeriod, 
    char basinID[], 
    int *numberOfCarryoverToSave,
    int carryoverMonthList[MAXCARRYOVERDATES], 
    int carryoverDayList[MAXCARRYOVERDATES], 
    int carryoverYearList[MAXCARRYOVERDATES], 
    int carryoverHourList[MAXCARRYOVERDATES],
    int *futurePrecipitation, 
    int *returnMessageLength, 
    char returnMessage[MAX_MSG_LEN], 
    float timeSeriesAtOutlet[MAX_DHM_TS_LEN], 
    int *pointsInTimeSeries, 
    char operationID[NWSRFS_ID_LENGTH+1],
    int *useRainPlusMeltPrecip)
{
	DhmResult result;
	char sacmodString[MAX_MOD_STRING_LENGTH] = "";
	char precipmodString[MAX_MOD_STRING_LENGTH] = "";	
	char modString[MAX_MOD_STRING_LENGTH] = "";	
	
	int numberOfPModsFound;
	int numberOfSModsFound;
	// get any mods defined for the current dhm-op
	// these were previously loaded into a data structure by mod decoding routines
 
	if (totalNumberPrecipModsFound <numberOfPrecipMods) {
           
	     numberOfPModsFound = load_dhm_precip_mod_string(operationID,precipmodString);
             totalNumberPrecipModsFound = numberOfPModsFound + totalNumberPrecipModsFound;	 
	     
	             
        }
	strcat(modString,precipmodString);
	
	if (totalNumberSacStateModsFound < numberOfDsacstMods) {
           
	     numberOfSModsFound = load_dhm_sac_state_mod_string(operationID,sacmodString);
             totalNumberSacStateModsFound = numberOfSModsFound + totalNumberSacStateModsFound;
	    
	        
        }
	strcat(modString,sacmodString);
	
	result = run_dhm(
	    *startMonthOfRunPeriod, 
	    *startDayOfRunPeriod, 
            *startYearOfRunPeriod, 
            *startHourOfRunPeriod,
	    *startMonthOfForecastPeriod,
            *startDayOfForecastPeriod,
	    *startYearOfForecastPeriod,
            *startHourOfForecastPeriod,
	    *endMonthOfRunPeriod,
	    *endDayOfRunPeriod,
            *endYearOfRunPeriod,
	    *endHourOfRunPeriod, 
            basinID, 
            *numberOfCarryoverToSave, 
            carryoverMonthList, 
	    carryoverDayList,
            carryoverYearList, 
	    carryoverHourList,
	    *futurePrecipitation, 
            modString,
	    *useRainPlusMeltPrecip);
        
	if (result.code==FAILED){
		*returnMessageLength=strlen(result.message);
		strcpy(returnMessage,result.message);		
	}
	else{
		
		int i;	
		for (i=0; i<result.timeSeriesLength; i++) {
			timeSeriesAtOutlet[i]=result.timeSeries[i];		
		}	
		*pointsInTimeSeries=result.timeSeriesLength;
		*returnMessageLength=0;
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}								

