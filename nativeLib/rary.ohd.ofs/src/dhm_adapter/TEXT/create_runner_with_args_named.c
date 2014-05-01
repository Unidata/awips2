/*.......................................
* File: create_runer_with_args_named.c
* Author(s): DHM Team
* Date Created: 5/5/07
* Development group: OHD HSEB
* Purpose: This routine uses jni calls to create a Runner object; the Runner
* constructor takes file paths/directories as arguments
* Module(s): 
*     create_runner_with_args_named
* Module(s) Called: 
*     buildDhmRunnerPaths
*     getClass
* jni modules: 
*     NewStringUTF
*     GetMethodID
*     NewObject
*     DeleteLocalRef
*     ExceptionCheck 
*.......................................
* Modified:
* 10/07 add startOfRun date params (month, day, year, hour) for creating dateTimeObject and passing to Runner constructor 
*/
#include "dhm.h"

void create_runner_with_args_named(
    const char *runnerClassName, 
    char precipDataPath[],
    char DHMModelDataPath[],
    char d2dDataPath[],
    char dhmNotifyDataPath[],
    char geoCoordDataPathAndFileName[],
    char precipXmrgSrcPath[],
    char DHMModelDataSrcPath[],
    char gridOutputConfigurationPath[],
    int *startMonth,
    int *startDay,
    int *startYear,
    int *startHour,
    char returnMessage[] ) {

    jmethodID constructor;
    jstring jPrecipDataPath;
    jstring jDHMModelDataPath;
    jstring jd2dDataPath;
    jstring jdhmNotifyDataPath;
    jstring jgeoCoordDataPathAndFileName;
    jstring jprecipXmrgSrcPath;
    jstring jDHMModelDataSrcPath;
    jstring jgridOutputConfigurationPath;
    
    char tempErrorMsg[200];
    
    strcpy(returnMessage,"");
    
    jPrecipDataPath = (*env)->NewStringUTF(env, precipDataPath);
    if (jPrecipDataPath == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not convert constructor argument %s.\n",precipDataPath);
        goto cleanup; 
    }       

    jDHMModelDataPath = (*env)->NewStringUTF(env, DHMModelDataPath);
    if (jDHMModelDataPath == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not convert constructor argument %s.\n",DHMModelDataPath);
        goto cleanup; 
    }       
    
    jd2dDataPath = (*env)->NewStringUTF(env, d2dDataPath);
    if (jd2dDataPath == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not convert constructor argument %s.\n",d2dDataPath);
        goto cleanup; 
    }       
    
    jdhmNotifyDataPath = (*env)->NewStringUTF(env, dhmNotifyDataPath);
    if (jdhmNotifyDataPath == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not convert constructor argument %s.\n",dhmNotifyDataPath);
        goto cleanup; 
    }       
    
    jgeoCoordDataPathAndFileName = (*env)->NewStringUTF(env, geoCoordDataPathAndFileName);
    if (jgeoCoordDataPathAndFileName == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not convert constructor argument %s.\n",geoCoordDataPathAndFileName);
        goto cleanup; 
    }       
    
    jprecipXmrgSrcPath = (*env)->NewStringUTF(env, precipXmrgSrcPath);
    if (jprecipXmrgSrcPath == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not convert constructor argument %s.\n",precipXmrgSrcPath);
        goto cleanup; 
    }       
    
    jDHMModelDataSrcPath = (*env)->NewStringUTF(env, DHMModelDataSrcPath);
    if (jDHMModelDataSrcPath == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not convert constructor argument %s.\n",DHMModelDataSrcPath);
        goto cleanup; 
    }
    
    jgridOutputConfigurationPath = (*env)->NewStringUTF(env, gridOutputConfigurationPath);
    if (jgridOutputConfigurationPath == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not convert constructor argument %s.\n",gridOutputConfigurationPath);
        goto cleanup; 
    }           
   
    jobject dhmRunnerPaths = buildDhmRunnerPaths( 
                                              jPrecipDataPath,
                                              jDHMModelDataPath,
                                              jd2dDataPath,
                                              jdhmNotifyDataPath,
                                              jgeoCoordDataPathAndFileName,
					      jprecipXmrgSrcPath,
					      jDHMModelDataSrcPath,
					      jgridOutputConfigurationPath);
    
    if ( dhmRunnerPaths == NULL){
        sprintf(tempErrorMsg, "Error occurred while running DHM-OP: Could not create dhmRunnerPaths");
        goto cleanup;
    }
    
    jobject startOfRunDate = buildDateTime(*startDay,*startMonth,*startYear,*startHour);
    if (startOfRunDate==NULL) {
        sprintf(tempErrorMsg, "Could not create start of run period date");
        goto cleanup;
    }
    
    runnerClass = (jclass) getClass(runnerClassName);
    if (runnerClass == NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not find Runner class named %s.\n",runnerClassName);
	goto cleanup;
    }
     
    constructor = (*env)->GetMethodID(env, runnerClass, "<init>","(L"RUNNER_PATHS_CLASS";L"SIMPLE_DATE_AND_TIME_CLASS";)V");
    if (constructor == NULL) {
	sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not create constructor for runner class: %s.\n",runnerClassName);
	goto cleanup;
    }
    
    runner =(*env)->NewObject(env, runnerClass, constructor, dhmRunnerPaths,startOfRunDate);
    if (runner==NULL) {
        sprintf(tempErrorMsg,"Error occurred while running DHM-OP: Could not create runner object for runner class: %s.\n", runnerClassName);
	goto cleanup;
    }
    return;
    
    // the block below is used to catch exceptions when creating Runner Class
    // and cleanup C memory references to Java objects
    cleanup:
    (*env)->DeleteLocalRef( env, runnerClass );
    (*env)->DeleteLocalRef( env, runner );
    
    if ((*env)->ExceptionCheck(env) ) {
	jthrowable error=(*env)->ExceptionOccurred(env);
        (*env)->ExceptionClear(env);
        
        char * javaMsg;
        jstring msg = getJavaErrorMessage(error);
        if (msg==NULL) { 
            sprintf(returnMessage, 
                    "%s: An unexpected error occurred and the java error description could not be retrieved", tempErrorMsg);
            return;
	}

        javaMsg = (char *)(*env)->GetStringUTFChars(env, msg, 0);
        if (javaMsg==NULL) {
            sprintf(returnMessage, 
                    "%s: An unexpected error occurred and the java error description could not be converted to UTF-8 characters", tempErrorMsg);
            return;
	}

        sprintf(returnMessage, 
                "%s: the java error description is: %.1000s", 
                tempErrorMsg, javaMsg);
        
        (*env)->ReleaseStringUTFChars(env, msg, javaMsg);
        (*env)->DeleteLocalRef(env, error);
	return;        
    }
     
    sprintf(returnMessage,"%s", tempErrorMsg);   
    return;   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
