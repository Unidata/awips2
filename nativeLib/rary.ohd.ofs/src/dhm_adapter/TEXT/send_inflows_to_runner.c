/*.......................................
* File: send_inflows_to_runner.c
* Author(s): DHM Team
* Date Created: 5/5/07
* Development group: OHD HSEB
* Purpose: This routine uses jni calls to create a Runner object; the Runner
* constructor takes file paths/directories as arguments
* Module(s): 
*     send_inflows_to_runner
* Module(s) Used:
*     buildUpstreamFlowArray
*     remove_trailing_space
*     getJavaErrorMessage
* jni modules used: 
*     NewStringUTF
*     GetMethodID
*     CallObjectMethod
*     ExceptionOccurred
*     ExceptionCheck
*     ExceptionClear
*     GetStringUTFChars
*     ReleaseStringUTFChars
*     DeleteLocalRef 
*.......................................
* Modified:
* */
#include "dhm.h"
		       
void send_inflows_to_runner(
                            char basinID[],
			    float upstreamFlowTimeSeries[], 
                            int indicesToInputData[], 
			    char upBasinIds[],
			    int *startOfTSindex,
                            int *endOfTSindex, 
			    int *nInflow, 
			    char errorMessage[MAX_DHM_TS_LEN]){ 
    
    int i,j;
    int inputDataStartingIndex;
    int timeSeriesLength;
    int runnerFailed=0;
    char upBasinID[NWSRFS_ID_LENGTH+1];
    float inflowTimeSeries[MAX_DHM_TS_LEN];
    jstring jupBasinID;
    jstring jbasinID;
    jfloatArray jUpstreamFlowTSArray;
    jmethodID methodID;

     
    
    methodID = (*env)->GetMethodID(env, runnerClass, "loadUpstreamChannelFlows", "([FL"STRING_CLASS";L"STRING_CLASS";)V");
    
    if (methodID==NULL) {
	strcpy(errorMessage,"runner does not have receiveDhmopInflows() method\n");
	goto cleanup;
	return;
    }
    
    for (i=0;i<*nInflow;i++){
        
        strncpy(upBasinID,&upBasinIds[i*NWSRFS_ID_LENGTH],NWSRFS_ID_LENGTH);
        upBasinID[NWSRFS_ID_LENGTH]='\0';	
        remove_trailing_space(upBasinID);        
	 
	//index in indicesToInputData is index stored in NWSRFS T array 
	//subtract 1; the D array starts at 1
	//SHOULD WE CHECK TO MAKE SURE INDEX IS >0; MAY NEED TO FILL IN UNUSED WITH -1?
	
	inputDataStartingIndex = indicesToInputData[i] - 1 + *startOfTSindex;
	
	jbasinID = (*env)->NewStringUTF(env,basinID);
    	if (jbasinID == NULL) {
            strcpy(errorMessage,"Error occurred while running DHM-OP: Could not create basin id");
	    goto cleanup;
	}
	
	jupBasinID = (*env)->NewStringUTF(env,upBasinID);
    	if (jupBasinID == NULL) {
            strcpy(errorMessage,"Error occurred while running DHM-OP: Could not create upstream basin id");
	    goto cleanup;
	}
	 
	if (strcmp(upBasinID,"") == 0){
           strcpy(errorMessage,"Error occurred while running DHM-OP: Error in reading basin Id");
	    goto cleanup;
        }
	 
        timeSeriesLength=*endOfTSindex - *startOfTSindex+1;
	for (j=0;j<timeSeriesLength;j++) {
                inflowTimeSeries[j]=upstreamFlowTimeSeries[inputDataStartingIndex+j-1];		
	}
	 
	jUpstreamFlowTSArray = buildUpstreamFlowArray(inflowTimeSeries,timeSeriesLength);
	
	if (jUpstreamFlowTSArray == NULL){
	    strcpy(errorMessage,"Error occurred while running DHM-OP: Could not create inflow time series array\n");
	    goto cleanup;
	}
	 
	(*env)->CallObjectMethod(env, runner, methodID, jUpstreamFlowTSArray, jupBasinID, jbasinID);
        
	//check for exception returned from call to load upstream flows
	if ((*env)->ExceptionCheck(env)) {
            runnerFailed = 1;

            jthrowable error=(*env)->ExceptionOccurred(env);
            (*env)->ExceptionClear(env);
        
            char * javaMsg;
            jstring msg = getJavaErrorMessage(error);
            if (msg==NULL) { 
                strcpy(errorMessage, 
                    "An unexpected error occurred and the error description could not be retrieved");
                goto cleanup;
            }

            javaMsg = (char *)(*env)->GetStringUTFChars(env, msg, 0);
            if (javaMsg==NULL) {
                strcpy(errorMessage, 
                    "An unexpected error occurred and the error description could not be converted to UTF-8 characters");
                goto cleanup;
            }

            sprintf(errorMessage, 
                "Unexpected error occurred while running dhm: %.1000s", javaMsg);
        
            (*env)->ReleaseStringUTFChars(env, msg, javaMsg);
            (*env)->DeleteLocalRef(env, error);        
            goto cleanup;

        }
	
	// no exception;clean up jfloatArray; 
	(*env)->DeleteLocalRef(env, jUpstreamFlowTSArray);
    }
    
    return;
    
    cleanup:
    (*env)->DeleteLocalRef( env, runnerClass );
    (*env)->DeleteLocalRef( env, runner );
     
    if ((*env)->ExceptionCheck(env) || runnerFailed==1) {
        (*env)->ExceptionClear(env);
        return;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
