/*.......................................
* File: dhm_mod_jni_utils.c
* Author(s): DHM Team
* Date Created: 5/5/07
* Development group: OHD HSEB
* Purpose: This files contains a set of routines that use jni to interact with the dhm 
* Java based mod guis
* Module(s):
*     popdownModGui
*     popupModGui
*     popupProperties
*     getModContent
*     is_dhm_mod_available
*     create_dhm_mod_object
* Module(s) Called:
*     getClass
* jni modules: 
*     NewStringUTF
*     GetMethodID
*     CallObjectMethod
*     ExceptionClear
*     ExceptionCheck
*     GetStringUTFChars 
*     NewObject
*.......................................
* Modified:
* */
#include "dhm_mods.h"

jmethodID constructor;
jclass dhmModClass;
jobject dhmModManager;
extern JNIEnv *env;

static int isDhmModDisplaying=0;
extern char basinIdWOpIDStr[2000];   /* set in fcst_ex/set_properties_for_dhm_mods */
extern char dhm_model_dataPath[120]; /* set in fcst_ex/set_properties_for_dhm_mods */
//extern void load_dhm_mod_string(char [], char []);
extern char currentModsStringWOpID[MAX_MOD_STRING_LENGTH*20];
/******************************************************
Routine: void popdownModGui()
Purpose: This routine calls java popdownMod method from
         java based dhmGui
******************************************************/	 
void popdownModGui(){
    jmethodID popdownID;
	
    if (!isDhmModDisplaying) return;
	
    if (dhmModManager!=NULL){
        popdownID = (*env)->GetMethodID(env, dhmModClass, "popdownMod","()V");
	(*env)->CallObjectMethod(env, dhmModManager, popdownID);			
	isDhmModDisplaying=0;
    }
	
}
/******************************************************
Routine: void popupModGui()
Purpose: This routine calls java popupMod method from
         java based dhmGui
******************************************************/	 
void popupModGui(){
    jmethodID popupID;
	
    if (dhmModManager!=NULL){
        popupID = (*env)->GetMethodID(env, dhmModClass, "popupMod","()V");
	(*env)->CallObjectMethod(env, dhmModManager, popupID);
	isDhmModDisplaying=1;
    }
}

/******************************************************
Routine: popupProperties()
Purpose: This routine calls a java method dhmModManager() from
         java based dhmGui

input:   char dhmopModType - type of mod: dprecip, dsacst 
         char segmentName  - Name of segment (8 chars)
	 char dhmopID      - List of Operation IDs (8 chars) with comma separated
	 char startDate    - date set from user - format "YYMMDDHHZ"
	 char endDate      - date set from user - format "YYMMDDHHZ"
         char validDate    - lastcomp date - format "YYMMDDHHZ"
******************************************************/

void popupProperties(char *dhmopModType, char *segmentName, char *dhmopID,
                     char *startDate, char *endDate,
		     char * modsString, char *curBasinID){
    	
    jmethodID methodID;
    jstring jModType, jSegName, jDhmopID;
    jstring jStartDate, jEndDate;
    jstring jBasinID, jdhmModelDataPath;
    jstring jmodsString;
  
    methodID = (*env)->GetMethodID(env, dhmModClass, "popupProperties",
		             "(L"STRING_CLASS";L"STRING_CLASS";"
                             "L"STRING_CLASS";L"STRING_CLASS";"	
			     "L"STRING_CLASS";L"STRING_CLASS";"			     
                             "L"STRING_CLASS";L"STRING_CLASS";)V");

    if (methodID==NULL) {
        printf("ModsManager object does not have popupProperties() method\n");
	
	if ((*env)->ExceptionCheck(env)) {
            (*env)->ExceptionClear(env);       
        }
	
	return;
    }

    jModType = (*env)->NewStringUTF( env, dhmopModType);
    jSegName = (*env)->NewStringUTF( env, segmentName);
    jDhmopID = (*env)->NewStringUTF( env, dhmopID);
    jStartDate = (*env)->NewStringUTF( env, startDate);
    jEndDate   = (*env)->NewStringUTF( env, endDate);
   
    jBasinID = (*env)->NewStringUTF( env, curBasinID);
    jdhmModelDataPath = (*env)->NewStringUTF( env, dhm_model_dataPath);
    jmodsString = (*env)->NewStringUTF( env, modsString); 
    
    (*env)->CallObjectMethod(env, dhmModManager, methodID, jModType,
            jSegName, jDhmopID, jStartDate, jEndDate,
	    jBasinID, jdhmModelDataPath, jmodsString);

    isDhmModDisplaying=1;
}

/******************************************************
Routine: char *getModContent()
Purpose: This routine calls a java method getModContent() from
         java based dhmGui
output: Returns String contains Mod information
******************************************************/
char *getModContent(){
    char *dhmModStr=NULL;
    jmethodID methodID;
    jstring jModString;
    char *str;
	
    methodID = (*env)->GetMethodID(env, dhmModClass, "getModContent", "()L"STRING_CLASS";");
    if (methodID==NULL) {
        printf("dhmModManager object does not have getModContent() method\n");
        return dhmModStr;
    }
	
    jModString = (jstring) ((*env)->CallObjectMethod(env, dhmModManager, methodID));
    str = (char*) (*env)->GetStringUTFChars(env, jModString, NULL);
	
    if(str==NULL) return dhmModStr;
	
    dhmModStr=(char*)malloc(sizeof(char)*strlen(str)+1);
    strcpy(dhmModStr,str);
    (*env)->ReleaseStringUTFChars(env, jModString, str);
	
    return dhmModStr;
}

/******************************************************
Routine: int is_dhm_mod_available()
Purpose: This routine calls a java method getDoneClicked() from
         java based dhmGui
output: Returns int value = 1 if user has created a mod 
                    value = 0 if there is no mod created 
******************************************************/
int is_dhm_mod_available()
{
    jmethodID methodID;
    int doneClicked=0;
    if(dhmModClass != NULL){
        methodID = (*env)->GetMethodID(env, dhmModClass, "getDoneClicked", "()I");
        if (methodID==NULL) {
            printf("dhmModManager object does not have getDoneClicked() method\n");
	    return doneClicked;
        }
    }
    else {
        printf("dhmModClass object does not exist\n");
    }
    doneClicked = (int) (*env)->CallIntMethod(env, dhmModManager, methodID);
    return doneClicked;
}

/******************************************************
Routine: void create_dhm_mod_object()
Purpose: This routine calls  java modManager class () from
         java based dhmGui

******************************************************/
void create_dhm_mod_object(){
    
    dhmModClass = (jclass) getClass(MODS_MANAGER_CLASS);
    if (dhmModClass == NULL) {
        printf("Could not find class ModsManager\n");        
        return;
    }
    
    constructor = (*env)->GetMethodID(env, dhmModClass, "<init>","()V");
    if (constructor == NULL) {
        printf("Could not find constructor ModsManager\n");
        return;
    }

    dhmModManager =(*env)->NewObject(env, dhmModClass, constructor);
    if (dhmModManager==NULL) {
        printf("Could not create new ModsManager object");
        return;
    }
    

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
