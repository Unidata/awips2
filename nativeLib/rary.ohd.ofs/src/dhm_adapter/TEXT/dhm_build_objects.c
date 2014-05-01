/*.......................................
* File: dhm_build_objects.c
* Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
* Date Created: 5/5/06

* Development group: OHD HSEB
* Purpose:         various routines using jni to interface nwsrfs to java based
*                 distributed modeling framework
*                  - dhmfailed() create a failed run DHM-OP message for display in nwsrfs 
*                  - dhmsucceeded() create a succeeded run DHM-OP message for displany in nwsrfs
*                  - buildDateTime() create a SimpleDateAndTime java object from nwsrfs date/time 
*                    variables
*                  - createCarryOverDateList() create a list of SimpleDateAndTime java objects 
*                    from nwsrfs carryover dates
*                 - buildUpstreamFlowArray() create a java array of the upstream flow 
*                   time series
*                 
* Module(s): 
*     dhmfailed
*     dhmsucceeded
*     buildDateTime
*     getJavaErrorMessage 
*     createCarryOverDateList
*     buildUpstreamFlowArray
*                                
* Date Modified:  
*                 3/07  added another argument to buildDhmRunnerPaths (precipXmrgSrcPath) - source of xmrg files; 
*                        possibly used for copying 
*                 4/07  added another argument to buildDhmRunnerPaths (jDHMModelDataSrcPath) - source of dhm data (parameters, states, pet); 
*                        possibly used for copying                            
*.......................................*/
#include "dhm.h"


void remove_trailing_space(char *inputString)
{
    if(inputString == NULL) return;
    if(inputString == "") return;
    
    int indexOfString =strlen(inputString)-1;
    while ((inputString[indexOfString]==' '))
    {
        inputString[indexOfString]='\0';
	    indexOfString --;
	    if (indexOfString < 0) break;
    }
}

DhmResult dhmfailed(char * message) 
{
    DhmResult r;
    r.code = FAILED;
    r.message = message;
    return r;
}

DhmResult dhmsucceeded(float timeSeries[MAX_DHM_TS_LEN], int length) {
    DhmResult r;
    r.code = SUCCEEDED;
    r.message = "";
    
    int i;
    
    for (i=0; i<length; i++) {
        r.timeSeries[i]=timeSeries[i];
    }
    r.timeSeriesLength = length;    
    return r;
}

jstring getJavaErrorMessage(jthrowable error){
    
    jmethodID getMessageMethod;
    jclass throwableClass = (*env)->GetObjectClass(env, error);

    if (throwableClass==NULL) goto cleanup;
    getMessageMethod = (*env)->GetMethodID(env, 
           throwableClass,"getMessage", "()L"STRING_CLASS";");
    if (getMessageMethod==NULL) goto cleanup;
    jobject jo =(*env)->CallObjectMethod(env, error, getMessageMethod);

 
    cleanup:
    (*env)->DeleteLocalRef(env, throwableClass);
    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionClear(env);       
        return NULL;        
    }
    return (jstring) jo;
}

jobject buildDhmRunnerPaths(jstring jprecipDataPath, 
                            jstring jDHMModelDataPath,
                            jstring jd2dDataPath, 
                            jstring jdhmNotifyDataPath, 
			    jstring jgeoCoordDataPathAndFileName,
			    jstring jprecipXmrgSrcPath,
			    jstring jDHMModelDataSrcPath,
			    jstring jgridOutputConfigurationPath) {
        
    jclass dhmRunnerPathsClass;
    jobject dhmRunnerPathsObject;
    jmethodID constructor;
   
    dhmRunnerPathsClass = (jclass) getClass(RUNNER_PATHS_CLASS);

    if (dhmRunnerPathsClass==NULL) goto cleanup;
  
    constructor = (*env)->GetMethodID(env, dhmRunnerPathsClass, 
                                      "<init>", 
                                      "(L"STRING_CLASS";L"STRING_CLASS";"
                                      "L"STRING_CLASS";L"STRING_CLASS";"
                                      "L"STRING_CLASS";L"STRING_CLASS";"
				      "L"STRING_CLASS";L"STRING_CLASS";)V");             
    
   
    if (constructor == NULL) goto cleanup;
    
    dhmRunnerPathsObject = (*env)->NewObject(env, dhmRunnerPathsClass, constructor, 
                                 jprecipDataPath, jDHMModelDataPath, jd2dDataPath, 
                                 jdhmNotifyDataPath, jgeoCoordDataPathAndFileName, 
				 jprecipXmrgSrcPath, jDHMModelDataSrcPath,
				 jgridOutputConfigurationPath);

    cleanup:
    (*env)->DeleteLocalRef(env, dhmRunnerPathsClass);
    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionClear(env);
        return NULL;
    }

    return dhmRunnerPathsObject;
        
}


jobject buildDateTime(int day, int month, int year, int hour) {
        
    jclass simpledateandtimeclass;
    jobject timeObject;
    jmethodID constructor;

    simpledateandtimeclass = (jclass) getClass(SIMPLE_DATE_AND_TIME_CLASS);

    if (simpledateandtimeclass == NULL) goto cleanup;

    constructor = (*env)->GetMethodID(env, simpledateandtimeclass, "<init>", "(IIII)V");

    if (constructor == NULL) goto cleanup;

    timeObject = (*env)->NewObject(env, simpledateandtimeclass, constructor, 
                                 day, month, year, hour);

    cleanup:
    (*env)->DeleteLocalRef(env, simpledateandtimeclass);
    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionClear(env);
        return NULL;
    }

    return timeObject;
        
}

jobjectArray createCarryOverDateList(int numberOfCarryoverToBeSaved, 
                                         int carryoverMonthList[MAXCARRYOVERDATES], 
                                         int carryoverDayList[MAXCARRYOVERDATES],    
                                         int carryoverYearList[MAXCARRYOVERDATES],
                                         int carryoverHourList[MAXCARRYOVERDATES]){
        
    int i;
    jobject tmpDateObject;

    jsize jNumberOfCarryoverToBeSaved = numberOfCarryoverToBeSaved; 

    jobjectArray carryoverDateList;
    jclass simpledateandtimeclass;

    simpledateandtimeclass = (jclass) getClass(SIMPLE_DATE_AND_TIME_CLASS);
        
    if (simpledateandtimeclass == NULL) goto cleanup;
        
    carryoverDateList = (jobjectArray)(*env)->NewObjectArray(env, 
                                                             jNumberOfCarryoverToBeSaved,
                                                             simpledateandtimeclass, NULL);
    if (carryoverDateList == NULL) goto cleanup;

    for (i=0;i<numberOfCarryoverToBeSaved;i++){
    
        tmpDateObject=buildDateTime(carryoverDayList[i], carryoverMonthList[i],
                                    carryoverYearList[i], carryoverHourList[i]);
        if (tmpDateObject==NULL) goto cleanup;
        (*env)->SetObjectArrayElement(env, carryoverDateList,i,tmpDateObject);
        if ((*env)->ExceptionCheck(env)) goto cleanup;
        (*env)->DeleteLocalRef(env, tmpDateObject);
        
    }

    cleanup:
    (*env)->DeleteLocalRef(env, simpledateandtimeclass);

    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionClear(env);
        return NULL;
    }
    return carryoverDateList;
}

char *getErrorMsg( const char detailedMsg[] ) {

    const static char * header = "Error occurred while running DHM-OP";
    char *fullMsg = NULL;
        
    int size = strlen(header) + 2 + strlen(detailedMsg) + 1;
        
    fullMsg = (char *) malloc(size * sizeof(char*));
    if(fullMsg == NULL) {
        return NULL;
    }
    memset(fullMsg,'\0', size);
    sprintf(fullMsg, "%s: %s.", header, detailedMsg);
        
    return fullMsg;
}

jfloatArray buildUpstreamFlowArray(float tsData[], int tsDataSize) {
    
    jfloatArray jUpstreamFlowTimeSeries;
    jsize jtsDataSize=tsDataSize;
    
    jUpstreamFlowTimeSeries = (*env)->NewFloatArray(env,jtsDataSize);
    if (jUpstreamFlowTimeSeries == NULL)goto cleanup;
        
    (*env)->SetFloatArrayRegion(env,jUpstreamFlowTimeSeries,0,jtsDataSize,tsData);
        
    cleanup:

    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionClear(env);
        return NULL;
    }
    return jUpstreamFlowTimeSeries;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

