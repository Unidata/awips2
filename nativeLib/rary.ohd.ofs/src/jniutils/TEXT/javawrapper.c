/*.......................................
* File: javawrapper.c
* Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
* Date Created: 5/5/06
* Development group: OHD HSEB
* Purpose: start_jvm() activate java virtual machine
*          close_jvm() stops java virtual machine
*          getClass() get the java class for javaClassName
* Module(s): startjvm,closejvm,getClass
* jni: http://java.sun.com/j2se/1.4.2/docs/guide/jni/index.html)
* jvm: http://java.sun.com/javase/technologies/hotspot/vmoptions.jsp
* jni types: http://java.sun.com/j2se/1.5.0/docs/guide/jni/spec/types.html#wp276
*.......................................
Modified:
2/07 - add reference to moveLogFileScript(abort_nwsrfs); this script is executed 
when an error is detected by jvm.  It moves error log file to ofs_ouput
4/07 - add option to set jvm system property for monitoring performance using jconsole
if token dhm_monitor_performance is set (e.g. dhm_monitor_performance=1) property set
Note: dhm_monitor_performance is not an official token so unless token manually set option
not available*/

#include "dhm.h"

#define NUM_OF_JVM_OPTIONS 4

void start_jvm(); 
void close_jvm();
jclass getClass(const char* javaClassName);

JNIEnv *env=NULL; 
JavaVM *jvm=NULL; 

/*int jvmTokenExists;*/
void start_jvm() {
    JavaVMInitArgs vm_args;
    JavaVMOption options[NUM_OF_JVM_OPTIONS];
    char classpath[MAXARRAY];
    char garbageCollectioncScheme[MAXARRAY];
    char onErrorCommand[MAXARRAY];
    char optionalCommand[MAXARRAY];
    char temporaryErrorMessage[MAX_MSG_LEN];
   
    
    static char *tokenOfsScripts="ofs_scripts";
    static char *tokenDhmMonitorPerformance="dhm_monitor_performance";
    static char *tokenTurnOffDHM="turn_off_jvm";
    char returnValue[200];
    char moveLogfileScript[MAXARRAY];
    int returnLength;
    int tokenlen, tokenExists;    
    jvmTokenNotExists = 1;
   
    /*added unofficial token to disable jvm */    
    tokenlen = strlen(tokenTurnOffDHM); 
    
    /*return value = 1 if the jvm token is not set - default value*/ 
    /*return       value = 0 if the jvm token does exist , for debuging purpose*/  
    jvmTokenNotExists = get_apps_defaults(tokenTurnOffDHM,&tokenlen,returnValue,
        &returnLength);
    
    tokenlen=strlen(tokenOfsScripts);
    get_apps_defaults(tokenOfsScripts,&tokenlen,returnValue,
    	&returnLength);
    sprintf(moveLogfileScript,"%s/abort_nwsrfs %%p",returnValue);                     
    
    sprintf(classpath, "-Djava.class.path=%s", getenv("CLASSPATH"));
    options[0].optionString = classpath;
    
    sprintf(garbageCollectioncScheme, "-XX:+UseParallelGC");
    options[1].optionString = garbageCollectioncScheme;
    
    /* Echo an error message when a segmentation fault occurs */
    /* Also move log, and core files to ofs_output/logname directory */
    /*sprintf(onErrorCommand, "-XX:OnError=echo %s;%s", errorMessage, mvFileToOutputDir);
    */
    sprintf(onErrorCommand, "-XX:OnError=%s",moveLogfileScript);
    options[2].optionString = onErrorCommand; 
    
    tokenlen=strlen(tokenDhmMonitorPerformance);
    tokenExists = get_apps_defaults(tokenDhmMonitorPerformance,&tokenlen,returnValue,
    	&returnLength);
     
    if (tokenExists == 1) {
        //token does not exists; don't set monitoring option
        sprintf(optionalCommand, "-DNONE");
    }
    else {
        //token exists set option to use jconsole for performance monitoring
        sprintf(optionalCommand, "-Dcom.sun.management.jmxremote");
    }
    options[3].optionString = optionalCommand;
    
    
    vm_args.version = JNI_VERSION_1_4;
    vm_args.ignoreUnrecognized = JNI_FALSE;
    vm_args.nOptions = NUM_OF_JVM_OPTIONS;
    vm_args.options = options;
    /* jvmTokenNotExists returns a positive value if "turn_off_jvm" token does not set */
    /* then start JVM                                       */
    
    if ( jvmTokenNotExists == 1 )
    {        
        JNI_CreateJavaVM( &jvm, (void**)&env, &vm_args );
	printf("Enabling JNI ......\n");
    }
    else
    {
        printf("*****- Turned OFF JVM token is set. *******\n");
	printf("Memory Fault will occur if DHM-OP operation is defined in a SEGMENT\n");
    }
}

void close_jvm() {
    if (jvmTokenNotExists == 1)
    {         
	(*jvm)->DestroyJavaVM(jvm);
    }   
}

jclass getClass(const char* javaClassName) {
	return (*env)->FindClass(env, javaClassName);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


