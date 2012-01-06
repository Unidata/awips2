/*.......................................
* File: run_dhm_with_class_name.c
* Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
* Date Created: 5/5/06

* Development group: OHD HSEB
* Purpose:         routine using jni to interface DHM-OP 
*                  and java-based DHM framework-This routine allows user
*                  to pass class name in the argument lists to perform specific
*                  testing for that class name.
* Module(s):
*     run_dhm
* Module(s) Used:
*     buildDateTime
*     createCarryOverDateList
*     getClass
*     getJavaErrorMessage
*     dhmfailed
*     dhmsucceeded
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
*
* Date Modified: 10/02/06 Passing another string argument contain 
*                         geo-coord path and filename to 
*                         the dhm runner  
*                 3/07  removed Runner class instantiation out of the run_dhm method; 
*                       added new argument to pass precip src path, used to build
*			dhmRunnerPaths and possible copying of precip xmrg files for ifp 
*                 4/07  added new argument to pass dhm data src path, used to build
*                       dhmRunnerPaths and possible copying of dhm data fles for ifp                            
*                10/07  add argument (int useRainPlusMeltPrecip ,1=yes 0=no) converted to boolean prior to sending to runner  
*.......................................*/
#include "dhm.h"

DhmResult run_dhm(
    int startMonthOfRunPeriod, 
    int startDayOfRunPeriod, 
    int startYearOfRunPeriod, 
    int startHourOfRunPeriod,
    int startMonthOfForecastPeriod,
    int startDayOfForecastPeriod,
    int startYearOfForecasPeriod,
    int startHourOfForecastPeriod,
    int endMonthOfRunPeriod,
    int endDateOfRunPeriod,
    int endYearOfRunPeriod,
    int endHourOfRunPeriod,        
    char basinID[],        
    int numberOfCarryoverToBeSaved, 
    int carryoverMonthList[MAXCARRYOVERDATES], 
    int carryoverDayList[MAXCARRYOVERDATES],
    int carryoverYearList[MAXCARRYOVERDATES], 
    int carryoverHourList[MAXCARRYOVERDATES],
    int futurePrecipitation,
    char modsString[],
    int useRainPlusMeltPrecip) {
        
    char errorMessage[2000] = "";
    int runnerFailed = 0;
    char *upBasinIDchar = NULL; 
    jmethodID constructor;
    jmethodID runMethodID;
    jstring jBasinID;
    jfloatArray jUpstreamFlowTSArray;
    jobjectArray carryoverDateList;
    jobject dhmresult;    
    jsize lengthOfTs;
    jfloatArray joutletTS;
    jmethodID outletMethodID;
    jclass dhmresultclass;
    jstring jModsString;
    
    int j;
    jboolean juseRainPlusMeltPrecip;
    
    float timeSeries[MAX_DHM_TS_LEN];
    
    jBasinID = (*env)->NewStringUTF(env, basinID);
    if (jBasinID == NULL) goto cleanup; 
    
    jModsString = (*env)->NewStringUTF( env, modsString );
    if (jModsString == NULL) goto cleanup;
    
    jobject startOfRunDate = buildDateTime(startDayOfRunPeriod, 
                                         startMonthOfRunPeriod, 
                                         startYearOfRunPeriod, 
                                         startHourOfRunPeriod);
    if (startOfRunDate==NULL) {
        sprintf(errorMessage, "Could not create start of run period date");
        goto cleanup;
    }

    jobject endOfRunPeriod = buildDateTime(endDateOfRunPeriod, 
                                         endMonthOfRunPeriod, 
                                         endYearOfRunPeriod, 
                                         endHourOfRunPeriod);
    if (endOfRunPeriod==NULL) {
        sprintf(errorMessage, "Could not create end of run period date");
        goto cleanup;
    }

    jobject startOfForecastPeriod = buildDateTime(startDayOfForecastPeriod, 
                                                startMonthOfForecastPeriod, 
                                                startYearOfForecasPeriod, 
                                                startHourOfForecastPeriod);
    if (startOfForecastPeriod==NULL) {
        sprintf(errorMessage, "Could not create start of forecast period date");
        goto cleanup;
    }
     
    if (useRainPlusMeltPrecip==1) {
        juseRainPlusMeltPrecip = true;
    }
    else {
        juseRainPlusMeltPrecip = false;
    }
    
    runMethodID = (*env)->GetMethodID(env, runnerClass, "run",                                                                             
                                        "(L"SIMPLE_DATE_AND_TIME_CLASS";"
                                        "L"SIMPLE_DATE_AND_TIME_CLASS";L"SIMPLE_DATE_AND_TIME_CLASS";"
                                        "L"STRING_CLASS";"
                                        "[L"SIMPLE_DATE_AND_TIME_CLASS";IL"STRING_CLASS";Z)L"RESULT_CLASS";");
     
    if (runMethodID==NULL) {
        sprintf(errorMessage, "Runner object does not have run() method");
        goto cleanup;
    }
        
    carryoverDateList=createCarryOverDateList(numberOfCarryoverToBeSaved, 
                                                carryoverMonthList,carryoverDayList, 
                                                carryoverYearList, carryoverHourList);
    if (carryoverDateList==NULL) {
        sprintf(errorMessage, "Could not create list of carryover dates");
        goto cleanup;
    }
       
    dhmresult = (*env)->CallObjectMethod(env, runner, runMethodID,
                        startOfRunDate, endOfRunPeriod, startOfForecastPeriod, 
                        jBasinID, carryoverDateList, futurePrecipitation, 
                        jModsString, juseRainPlusMeltPrecip);               
     
     
    if ((*env)->ExceptionCheck(env)) {

        runnerFailed = 1;

        jthrowable error=(*env)->ExceptionOccurred(env);
        (*env)->ExceptionClear(env);
        
        char * javaMsg;
        jstring msg = getJavaErrorMessage(error);
        if (msg==NULL) { 
            sprintf(errorMessage, 
                    "An unexpected error occurred and the error description could not be retrieved");
            goto cleanup;
        }

        javaMsg = (char *)(*env)->GetStringUTFChars(env, msg, 0);
        if (javaMsg==NULL) {
            sprintf(errorMessage, 
                    "An unexpected error occurred and the error description could not be converted to UTF-8 characters");
            goto cleanup;
        }

        sprintf(errorMessage, 
                "Unexpected error occurred while running dhm: %.1000s", 
                javaMsg);
        
        (*env)->ReleaseStringUTFChars(env, msg, javaMsg);
        (*env)->DeleteLocalRef(env, error);        
        goto cleanup;

    }
    
    dhmresultclass = (jclass) getClass(RESULT_CLASS);
    
    
    if (dhmresultclass==NULL) {
        sprintf(errorMessage, "An error occurred finding the DhmResult class"); 
        goto cleanup;
    }
        
    outletMethodID = (*env)->GetMethodID(env, dhmresultclass, "getOutletTimeSeries", "()[F");
    
    if (outletMethodID==NULL) {
        sprintf(errorMessage, "An error occurred finding the getOutletTimeSeries method of the DhmResult class"); 
        goto cleanup;
    }

    
    joutletTS = (jfloatArray) (*env)->CallObjectMethod(env, dhmresult, outletMethodID);
    if (joutletTS==NULL) {
        sprintf(errorMessage, "An error occurred retrieving the outlet time series from the DhmResult object"); 
        goto cleanup;
    }
    
    lengthOfTs = (*env)->GetArrayLength( env, joutletTS );
    jfloat *tsValues = (*env)->GetFloatArrayElements( env, joutletTS, 0 );
   
    for (j=0; j<lengthOfTs; j++) {
        timeSeries[j] = tsValues[j];
    }

    (*env)->ReleaseFloatArrayElements( env, joutletTS, tsValues, 0 );
    (*env)->DeleteLocalRef( env, dhmresultclass );
     
    
    cleanup:
    (*env)->DeleteLocalRef( env, runnerClass );
    (*env)->DeleteLocalRef( env, runner );
     
    if ((*env)->ExceptionCheck(env) || runnerFailed==1) {
        (*env)->ExceptionClear(env);
        return dhmfailed( ( char * ) getErrorMsg( errorMessage ) );
    }
    return dhmsucceeded( timeSeries, lengthOfTs );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
