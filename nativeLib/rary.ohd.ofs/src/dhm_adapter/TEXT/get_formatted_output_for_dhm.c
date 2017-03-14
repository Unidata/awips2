/*.......................................
* File: get_formatted_output_for_dhm.c
* Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
* Date Created: 5/5/06
* Development group: OHD HSEB
* Purpose: This subroutine prints all DHM-OP arguments to the screen
* Module(s): 
*     get_formatted_output_for_dhm
*.......................................
* Modified:
*    3/07 - removed upBasinId input and replaced with upBasinIds and numOfUpBasinIds to print out multiple upStreamBasins
*    9/07 - add useRainPlusMelt flag (0=no, 1=yes)
*  Routine to get formatted output string                                      *
*  input - int *startMonthOfRunPeriod - month of start observation time        *
*  input - int *startDayOfRunPeriod - date of start observation time           *
*  input - int *startYearOfRunPeriod - year of start observation time          *
*  input - int *startHourOfRunPeriod - hour of start observation time          *
*  input - int *startMonthOfForecastPeriod - month of start forecast time      *
*  input - int *startDayOfForecastPeriod - date of start forecast time         *
*  input - int *startYearOfForecastPeriod - year of start forecast time        *
*  input - int *startHourOfForecastPeriod - hour of start forecast time        *
*  input - int *endMonthOfRunPeriod - month of end run                         *
*  input - int *endDateOfRunPeriod - date of end run                           *
*  input - int *endYearOfRunPeriod - year of end run                           *
*  input - int *endHourOfRunPeriod - hour of end run                           *
*  input - char *precipDataPath - directory of gridded precipitation data      *
*  input - char *basinID - name of basin to model                              *
*  input - char[] upBasinIDsupstream - basin ids in PO array	               *
*  input - int *numOfUpBasinIds - number of upstream basin ids                 *
*  input - char *precipDataPathLength - length of directory of gridded         *
*                                       precipitation data                     *
*  input - char *modelDataPath - directory of DHM model data                   *
*  input - char *modelDataPathLength - length of directory of DHM model data   *
*  input - int *numberOfCarryoverToBeSaved - Number of carryover to be saved   *
*  input - int[] carryoverMonthList - a list of carryover month                *
*  input - int[] carryoverYearList - a list of carryover year                  *
*  input - int[] carryoverDayList - a list of carryover day                    *
*  input - int[] carryoverHourList - a list of carryover hour                  *
*  input - int *furturePrecipitation - number of hours of future precipitation *
*  input - int *useRainPlusMelt - flag (0=no, 1=yes)                           *
*  output - char formattedOutput[1024] - formatted string of all dhm settings  *
*  output - int *formattedOutputLength - length of formattedOutput             */

#include "dhm.h"

void get_formatted_output_for_dhm(
    int *startMonthOfRunPeriod,
    int *startDayOfRunPeriod,
    int *startYearOfRunPeriod,
    int *startHourOfRunPeriod,
    int *startMonthOfForecastPeriod,
    int *startDayOfForecastPeriod,
    int *startYearOfForecastPeriod,
    int *startHourOfForecastPeriod,
    int *endMonthOfRunPeriod,
    int *endDateOfRunPeriod,
    int *endYearOfRunPeriod,
    int *endHourOfRunPeriod, 
    char *basinID,	
    char upBasinIDs[], 
    int *numOfUpBasinIds,
    char *precipDataPath, 
    char *DHMModelDataPath,
    char *d2dDataPath, 
    char *d2dNotifyDataPath,
    int *numberOfCarryoverToBeSaved, 
    int carryoverMonthList[MAXCARRYOVERDATES],
    int carryoverDayList[MAXCARRYOVERDATES], 
    int carryoverYearList[MAXCARRYOVERDATES],
    int carryoverHourList[MAXCARRYOVERDATES],
    int *futurePrecipitation,
    int *useRainPlusMelt,   
    char formattedOutput[2048],	
    int *formattedOutputLength
    )
{
    char numberOfCarryoverToBeSavedOutput[1024];
    char basinIDOutput[100];
    char upBasinIDOutput[100];
    char futurePrecipitationOutput[100];
    char headerOutput[100];
    char endOutput[100];
    char startObservationOutput[100];
    char startForecastOutput[100];
    char endRunOutput[100];		
    char precipitationDataDirectoryOutput[MAX_DIRECTORY_NAME_LENGTH];
    char DHMModelDataDirectoryOutput[MAX_DIRECTORY_NAME_LENGTH];
    char d2dDataDirectoryOutput[MAX_DIRECTORY_NAME_LENGTH];
    char d2dNotifyDataDirectoryOutput[MAX_DIRECTORY_NAME_LENGTH];
    char precipDataType[100]="";	
    int i;
    char upBasinID[NWSRFS_ID_LENGTH+1];
	

    sprintf(headerOutput,"%s",
          "\n******************************DHM Operation****************************\n");
    sprintf(startObservationOutput,"* Start of run period: %d/%d/%d %dZ\n",
	  *startMonthOfRunPeriod, *startDayOfRunPeriod, *startYearOfRunPeriod, 
          *startHourOfRunPeriod);
	
    sprintf(startForecastOutput,"* Start of forecast period : %d/%d/%d %dZ\n",
	  *startMonthOfForecastPeriod, *startDayOfForecastPeriod, 
          *startYearOfForecastPeriod,*startHourOfForecastPeriod);
	
    sprintf(endRunOutput,"* End of run period: %d/%d/%d %dZ\n",
	  *endMonthOfRunPeriod, *endDateOfRunPeriod,
          *endYearOfRunPeriod, *endHourOfRunPeriod);
	
    sprintf(basinIDOutput,"* Basin ID : %s\n",basinID);	
	
    sprintf(upBasinIDOutput,"* Upstream Basin ID : ");
	
    for (i=0;i<*numOfUpBasinIds;i++){
        strncpy(upBasinID,&(upBasinIDs[i*NWSRFS_ID_LENGTH]),NWSRFS_ID_LENGTH);
            upBasinID[NWSRFS_ID_LENGTH]='\0';
            remove_trailing_space(upBasinID);
	    sprintf(upBasinIDOutput,"%s %s",upBasinIDOutput,upBasinID);
    }
	
    strcat(upBasinIDOutput,"\n");
		
    sprintf(precipitationDataDirectoryOutput,
          "* precipitation data directory : %s\n",precipDataPath);
    sprintf(DHMModelDataDirectoryOutput,"* DHM Model data directory : %s\n",
    	  DHMModelDataPath);
    sprintf(d2dDataDirectoryOutput,
          "* D2D data directory : %s\n",d2dDataPath);
    sprintf(d2dNotifyDataDirectoryOutput,
          "* D2D Notify bin directory : %s\n",d2dNotifyDataPath);
    sprintf(futurePrecipitationOutput,
    	  "* number of hour(s) of future precipitation: %d\n",*futurePrecipitation);
	
    if (*numberOfCarryoverToBeSaved)
    {
        sprintf(numberOfCarryoverToBeSavedOutput,
              "* %d carryover date(s) will be saved\n",*numberOfCarryoverToBeSaved);
	for (i=0;i<*numberOfCarryoverToBeSaved;i++) 
	    sprintf(numberOfCarryoverToBeSavedOutput,
            	  "%s* Carryover date %d: %d/%d/%d %dZ\n",
		  numberOfCarryoverToBeSavedOutput, i+1, carryoverMonthList[i],
	          carryoverDayList[i], carryoverYearList[i], carryoverHourList[i]);
    }
    else	    
        sprintf(numberOfCarryoverToBeSavedOutput,
              "* Carryover will not be saved\n");
    if(*useRainPlusMelt){
        sprintf(precipDataType,
	      "* Precip Data Type: RAIN PLUS MELT \n");
    }
    else{
        sprintf(precipDataType,
	      "* Precip Data Type: MPE \n");
    }
    
    sprintf(endOutput,
          "***********************************************************************\n");

    sprintf(formattedOutput, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s", 
    	    headerOutput, startObservationOutput, 
    	    startForecastOutput, endRunOutput, basinIDOutput, upBasinIDOutput,
            precipitationDataDirectoryOutput, 
	    DHMModelDataDirectoryOutput, d2dDataDirectoryOutput, 
            d2dNotifyDataDirectoryOutput, futurePrecipitationOutput,
  	    numberOfCarryoverToBeSavedOutput, precipDataType, 
            endOutput);
	
    *formattedOutputLength=strlen(formattedOutput);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
