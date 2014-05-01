
#include <stdlib.h>
#include <stdio.h>

#include "display_pdc_tsl_jni.h"
#include "mpe_log_utils.h"


static JNIEnv *env=NULL; 
static JavaVM *jvm=NULL; 

static int started = 0;

void startjavavm() 
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
logMessage("Done of first jvm start.\n");
    }

}

void closejavavm()
{
    if (jvm != NULL)
    {
        (*jvm)->DestroyJavaVM(jvm);
    }
    started = 0;
}

jclass getClassName(const char* javaClassName) 
{
    return (*env)->FindClass(env, javaClassName);
}

void callPDCTimeSeriesLiteThroughJNI(const char *jdbcUrl, 
                                     char *stationId,
                                     char *stationParm)
{
    
    char* dialogClassName = "ohd/hseb/timeserieslite/TimeSeriesLite";
    char * pdcDrawingMgr = "ohd.hseb.timeserieslite.pdc.PDCDrawingMgr";    
   
    jclass dialogClass;
    jmethodID methodID;
    char errorMessage[2000];
    jstring dbConnString;
    jstring stationIdString;
    jstring stationParm1String;    
    jstring stationParm2String;
    jstring pdcDrawingMgrString;
    
    pdcDrawingMgrString = (*env)->NewStringUTF(env, pdcDrawingMgr);
    dbConnString = (*env)->NewStringUTF(env, jdbcUrl);
    stationIdString = (*env)->NewStringUTF(env, stationId);
    stationParm1String = (*env)->NewStringUTF(env, stationParm);
    stationParm2String = (*env)->NewStringUTF(env, stationParm);
        
    
    dialogClass = (jclass) getClassName(dialogClassName);

    if (dialogClass==NULL) 
    {
        sprintf(errorMessage,
            "Could not find Runner class named %s\n",
             dialogClassName);
            
        /*goto cleanup;*/
    }
    
    methodID = (*env)->GetStaticMethodID(env, dialogClass, "show", 
            "(Ljava/lang/String;"
             "Ljava/lang/String;"
             "Ljava/lang/String;"             
	     "Ljava/lang/String;"
             "Ljava/lang/String;)V");
    
    if (methodID==NULL) 
    {
        sprintf(errorMessage,"Could not find main method\n");
       /* goto cleanup;   */
    }
    
    (*env)->CallStaticVoidMethod(env, dialogClass, methodID,
                                 pdcDrawingMgrString,
                                 dbConnString,
                                 stationIdString, 
                                 stationParm1String,
				 stationParm2String);

    /*cleanup:*/
    
    if (dialogClass != NULL)
         (*env)->DeleteLocalRef(env, dialogClass);
         
        
    if ((*env)->ExceptionCheck(env)) 
    {
        (*env)->ExceptionClear(env);
       logMessage(errorMessage);
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

    flogMessage ( stdout, "jdbcUrl: %s\n", jdbcUrl );
    flogMessage ( stdout, "logFilePath: %s\n", logFilePath );
    flogMessage ( stdout, "applicationName: %s\n", applicationName );
    flogMessage ( stdout, "userID: %s\n", userID );
    flogMessage ( stdout, "defaultColorFilePath: %s\n", defaultColorFilePath );
    flogMessage ( stdout, "rgbFilePath: %s\n", rgbFilePath );

    flogMessage ( stdout, "env address: %p\n", env );
    
    dbConnString = (*env)->NewStringUTF(env, jdbcUrl);

    flogMessage ( stdout, "Here-1\n" );

    logFilePathString = (*env)->NewStringUTF(env, logFilePath);

    flogMessage ( stdout, "Here-2\n" );

    applicationNameString = (*env)->NewStringUTF(env, applicationName);

    flogMessage ( stdout, "Here-3\n" );

    userIDString = (*env)->NewStringUTF(env, userID);
  
    flogMessage ( stdout, "Here-4\n" );

    defaultColorFileString = (*env)->NewStringUTF(env, defaultColorFilePath);

    flogMessage ( stdout, "Here-5\n" );

    rgbFilePathString = (*env)->NewStringUTF(env, rgbFilePath);

    flogMessage ( stdout, "jdbcUrl: %s\n", jdbcUrl );
    flogMessage ( stdout, "logFilePath: %s\n", logFilePath );
    flogMessage ( stdout, "applicationName: %s\n", applicationName );
    flogMessage ( stdout, "userID: %s\n", userID );
    flogMessage ( stdout, "defaultColorFilePath: %s\n", defaultColorFilePath );
    flogMessage ( stdout, "rgbFilePath: %s\n", rgbFilePath );
   
    dialogClass = (jclass) getClassName(dialogClassName);

    flogMessage ( stdout, "Here-6\n" );

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

    flogMessage ( stdout, "Here-7\n" );
    flogMessage ( stdout, "env address: %p\n", env );
    
    (*env)->CallStaticVoidMethod(env, dialogClass, methodID,
                                 dbConnString,
                                 logFilePathString, 
                                 applicationNameString, 
                                 userIDString,
                                 defaultColorFileString,
                                 rgbFilePathString);
    flogMessage ( stdout, "Here-8\n" );

    cleanup:
     if (dialogClass != NULL)
         (*env)->DeleteLocalRef(env, dialogClass);
         
    
    
    if ((*env)->ExceptionCheck(env)) 
    {
        (*env)->ExceptionClear(env);
       logMessage(errorMessage);
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
 
    dialogClass = (jclass) getClassName(dialogClassName);

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
       logMessage(errorMessage);
    }

    return colorTimeLong;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/display_pdc_tsl_jni.c,v $";
 static char rcs_id2[] = "$Id: display_pdc_tsl_jni.c,v 1.2 2007/11/01 18:18:34 lawrence Exp $";}
/*  ===================================================  */

}
