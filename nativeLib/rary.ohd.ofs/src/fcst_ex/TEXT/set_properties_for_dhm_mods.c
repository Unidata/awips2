#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "dhm_mods.h"
/*
c   Routine to store basin ID and precipitation gridded data path
c  
*/
char basinIdWOpIDStr[2000] = "";
char dhm_model_dataPath[120]="";
char currentModsStringWOpID[MAX_MOD_STRING_LENGTH*20];
void set_properties_for_dhm_mods(char opIDString[], char basinID[], char DHMModelDataPath[])
{
    static int first = 1;
    /*
    if(basinID != NULL)
    {
       if(first){
        
            strncpy(basinIdWOpIDStr,opIDString,9);
	     first = 0;
	     
       }
       else{
	    strcat(basinIdWOpIDStr,",");
	    strncat(basinIdWOpIDStr,opIDString,9);
	   
	}
	strcat(basinIdWOpIDStr,",");
        strncat(basinIdWOpIDStr,basinID,9);
	printf("basinID = %s basinIdWOpIDStr= %s\n",basinID, basinIdWOpIDStr);
   */
    if(basinID != NULL)
    {
        strncpy(dhm_model_dataPath,DHMModelDataPath,120);
	
	strncpy(basinProps.basinData[basinProps.count].basinID , basinID, NWSRFS_ID_LENGTH+1);
	strncpy(basinProps.basinData[basinProps.count].operationID ,opIDString, NWSRFS_ID_LENGTH+1);
	
	basinProps.count++;
		
    }
   
  
}

extern struct basinPropsStruct basinProps;

char *getBasinID(char opid[]){
    int i;
    static char tempBasin[10]="";    
    for( i = 0; i < basinProps.count; i++)
    {     
        if(strncmp (basinProps.basinData[i].operationID, opid,strlen(opid)) == 0)
	{
	   strncpy(tempBasin,basinProps.basinData[i].basinID,NWSRFS_ID_LENGTH+1);	  
	   
	   return tempBasin;
	}
    }
    return tempBasin;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
