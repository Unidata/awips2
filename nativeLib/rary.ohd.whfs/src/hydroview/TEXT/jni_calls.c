
#include "jni_calls.h"
#include <stdlib.h>
#include <stdio.h>


static JNIEnv *env=NULL; 
static JavaVM *jvm=NULL; 

static int started = 0;

void startjvm() 
{
     
    if (! started)
    {
        JavaVMInitArgs vm_args;
        JavaVMOption options[1];
        char classpath[BUFSIZ];
        
        sprintf(classpath, "-Djava.class.path=%s", getenv("CLASSPATH"));
        options[0].optionString = classpath;
    
        vm_args.version = JNI_VERSION_1_4;
        vm_args.ignoreUnrecognized=JNI_TRUE;
        vm_args.nOptions=1;
        vm_args.options=options;
    
        JNI_GetDefaultJavaVMInitArgs(&vm_args);
    
        JavaVM **javaVMDoublePtr = (JavaVM **)&jvm;
        JNIEnv ** envDoublePtr = &env;
        void * vm_argsPtr = (void *) &vm_args;
    
        JNI_CreateJavaVM(javaVMDoublePtr, (void **) envDoublePtr, vm_argsPtr);
        
        started = 1;
    }

}

void closejvm()
{
    if (jvm != NULL)
    {
        (*jvm)->DestroyJavaVM(jvm);
    }
    started = 0;
}

jclass getClass(const char* javaClassName) 
{
    return (*env)->FindClass(env, javaClassName);
}

void callTimeSeriesLiteThroughJNI(const char *locationId, 
                                  const char *obsParamCode, 
                                  const char *fcstParamCode,
                                  const char *jdbcUrl) 
{
    
    //char header[] = "callTimeSeriesLiteThroughJNI() ";
 
    char* tsLiteClassName = "ohd/hseb/timeserieslite/TimeSeriesLite";
    char* pdcDrawManagerClassName = "ohd.hseb.timeserieslite.pdc.PDCDrawingMgr";
    jclass tsLiteClass;
    jmethodID mainMethodID;
    char errorMessage[2000];
    jstring drawingManagerNameString;
    jstring dbConnString;
    jstring locationIdString;
    jstring paramCodeString1;
    jstring paramCodeString2;
 
    
    drawingManagerNameString = (*env)->NewStringUTF(env, pdcDrawManagerClassName );
    
    dbConnString = (*env)->NewStringUTF(env, jdbcUrl);
    locationIdString = (*env)->NewStringUTF(env, locationId);
    paramCodeString1 = (*env)->NewStringUTF(env, obsParamCode);
    paramCodeString2 = (*env)->NewStringUTF(env, fcstParamCode);
   
    
    tsLiteClass = (jclass) getClass(tsLiteClassName);
    if (tsLiteClass==NULL) 
    {
        sprintf(errorMessage,
            "Could ssnot find Runner class named %s\n",
            tsLiteClassName);
            
        goto cleanup;
    }
    
    mainMethodID = (*env)->GetStaticMethodID(env, tsLiteClass, "show", 
            "(Ljava/lang/String;"
             "Ljava/lang/String;"
             "Ljava/lang/String;"
             "Ljava/lang/String;"
             "Ljava/lang/String;)V");
    if (mainMethodID==NULL) 
    {
        sprintf(errorMessage,"Could not find main method\n");
        goto cleanup;   
    }

    
    (*env)->CallStaticVoidMethod(env, tsLiteClass, mainMethodID,
                                 drawingManagerNameString,
                                 dbConnString,
                                 locationIdString, 
                                 paramCodeString1, 
                                 paramCodeString2);


    cleanup:
     if (tsLiteClass != NULL)
         (*env)->DeleteLocalRef(env, tsLiteClass);
         
    // if (mainMethodID != NULL)
    //     (*env)->DeleteLocalRef(env, mainMethodID);
     
   //  (*env)->DeleteLocalRef(env, drawingManagerName);
  //   (*env)->DeleteLocalRef(env, dbConnString);
   //  (*env)->DeleteLocalRef(env, locationIdString);
    // (*env)->DeleteLocalRef(env, paramCodeString1);
   //  (*env)->DeleteLocalRef(env, paramCodeString2);
    
    
    
    if ((*env)->ExceptionCheck(env)) 
    {
        (*env)->ExceptionClear(env);
        printf(errorMessage);
    }

    return;
}
 
void callColorManagerThroughJNI(const char *jdbcUrl, 
                                const char *logFilePath, 
                                const char *applicationName,
                                const char *userID,
                                const char *defaultColorFilePath,
                                const char *rgbFilePath)
{
    
    char* dialogClassName = "ohd/hseb/color_chooser/ColorChooserDialog";
    jclass dialogClass;
    jmethodID methodID;
    char errorMessage[2000];
    jstring dbConnString;
    jstring logFilePathString;
    jstring applicationNameString;
    jstring userIDString;
    jstring defaultColorFileString;
    jstring rgbFilePathString;
    
 
    
    dbConnString = (*env)->NewStringUTF(env, jdbcUrl);
    logFilePathString = (*env)->NewStringUTF(env, logFilePath);
    applicationNameString = (*env)->NewStringUTF(env, applicationName);
    userIDString = (*env)->NewStringUTF(env, userID);
    defaultColorFileString = (*env)->NewStringUTF(env, defaultColorFilePath);
    rgbFilePathString = (*env)->NewStringUTF(env, rgbFilePath);
   
    
    dialogClass = (jclass) getClass(dialogClassName);

    if (dialogClass==NULL) 
    {
        sprintf(errorMessage,
            "Could not find Runner class named %s\n",
             dialogClassName);
            
        goto cleanup;
    }
    
    methodID = (*env)->GetStaticMethodID(env, dialogClass, "show", 
            "(Ljava/lang/String;"
             "Ljava/lang/String;"
             "Ljava/lang/String;"
             "Ljava/lang/String;"
             "Ljava/lang/String;"
             "Ljava/lang/String;)V");
    if (methodID==NULL) 
    {
        sprintf(errorMessage,"Could not find main method\n");
        goto cleanup;   
    }
    
    (*env)->CallStaticVoidMethod(env, dialogClass, methodID,
                                 dbConnString,
                                 logFilePathString, 
                                 applicationNameString, 
                                 userIDString,
                                 defaultColorFileString,
                                 rgbFilePathString);

    cleanup:
     if (dialogClass != NULL)
         (*env)->DeleteLocalRef(env, dialogClass);
         
    
    
    if ((*env)->ExceptionCheck(env)) 
    {
        (*env)->ExceptionClear(env);
        printf(errorMessage);
    }

    return;
}

long getColorManagerSaveTimeThroughJNI( )
{
    
    char* dialogClassName = "ohd/hseb/color_chooser/ColorChooserDialog";
    jclass dialogClass;
    jmethodID methodID;
    char errorMessage[2000];
    jlong  colorTimeJLong;
    long   colorTimeLong = 0;
 
    dialogClass = (jclass) getClass(dialogClassName);

    if (dialogClass==NULL) 
    {
        sprintf(errorMessage,
            "Could not find Runner class named %s\n",
             dialogClassName);
            
        goto cleanup;
    }
    
    methodID = (*env)->GetStaticMethodID(env, dialogClass, "getLastUpdateTime",
             "()J");
	     
    if (methodID==NULL) 
    {
        sprintf(errorMessage,"Could not find main method\n");
        goto cleanup;   
    }
    
    colorTimeJLong = (*env)->CallStaticLongMethod(env, dialogClass, methodID);
    colorTimeLong = ( long ) colorTimeJLong;
    
        
    cleanup:
     if (dialogClass != NULL)
         (*env)->DeleteLocalRef(env, dialogClass);
         
    
    
    if ((*env)->ExceptionCheck(env)) 
    {
        (*env)->ExceptionClear(env);
        printf(errorMessage);
    }

    return colorTimeLong;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
