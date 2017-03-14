/*.......................................
* File: initialize_runner_with_args.c
* Author(s): DHM Team
* Date Created: 5/5/07
* Development group: OHD HSEB
* Purpose: This routine serves as a wrapper to create_runner_with_args_named, 
* which creates a Runner object and send_inflows_to_runner, which pushes
* upstream flows to the runner if applicable
* Module(s): 
*     initialize_runner_with_args
* Module(s) Used:
*     create_runner_with_args_named 
*     send_inflows_to_runner
*.......................................
* Modified:
* 10/07 added start date and time args to Runner constructor; allows for copying data only 1x at Runne creation
 */
#include "dhm.h"
		       
void initialize_runner_with_args(
                       char basinID[],
		       float upstreamFlowTimeSeries[], 
                       int indicesToInputData[], 
		       char upBasinIds[],
		       int* startOfTSindex,
                       int* endOfTSindex, 
		       int* nInflow,
		       char precipDataPath[],
                       char DHMModelDataPath[],
                       char d2dDataPath[],
                       char dhmNotifyDataPath[],
                       char geoCoordDataPathAndFileName[],
                       char precipXmrgSrcPath[],
                       char DHMModelDataSrcPath[],
		       char gridOutputConfigurationPath[],
		       int* startMonth,
                       int* startDay,
                       int* startYear,
                       int* startHour,
		       char errorMessage[MAX_MSG_LEN], 
		       int* errorMessageLength){ 
     
    
    char temporaryErrorMessage[MAX_MSG_LEN];
    
    strcpy(errorMessage,"");
    
    create_runner_with_args_named(
        RUNNER_CLASS,
	precipDataPath,
	DHMModelDataPath,
	d2dDataPath,
	dhmNotifyDataPath,
	geoCoordDataPathAndFileName,
	precipXmrgSrcPath,
	DHMModelDataSrcPath,
	gridOutputConfigurationPath,
	startMonth,
        startDay,
        startYear,
        startHour,
	temporaryErrorMessage);
   
    *errorMessageLength = strlen(temporaryErrorMessage);
     
    if (*errorMessageLength > 0) {
        strcat(errorMessage,temporaryErrorMessage);
        return;
    }
     
    send_inflows_to_runner(
        basinID,
	upstreamFlowTimeSeries,
	indicesToInputData,
	upBasinIds,
	startOfTSindex,
	endOfTSindex,
	nInflow,
	temporaryErrorMessage);  
    
    *errorMessageLength = strlen(temporaryErrorMessage);
    
    if (*errorMessageLength > 0) {
        strcat(errorMessage,temporaryErrorMessage);
    }
    return;
	

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
