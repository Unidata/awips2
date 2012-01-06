/*.......................................
* File: save_dhm_mod.c
* Author(s): DHM Team
* Date Created: 5/5/07
* Development group: OHD HSEB
* Purpose: routines used to save dhm precip and sac state mods into NWSRSF
* mod format
* Module(s):
*     save_dhm_mod
*     save_dhm_mod_in_b2_format
*     save_dhm_mod_in_b3_format
* Module(s) Called:
*     getModContent
*     load_new_mods_edit
*     FCITZC
*     JULDA
*     parseLineTwoOfModCard
*     
* Modified:
* */
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"
#include "dhm_mods.h"
#include "save_dhm_mod.h"


/******************************************************
Routine: void save_dhm_mod()
Purpose: this routine determines the dhm mod type (b3 or b2 type: three dates or one date) 
         and saves it when user select ok

******************************************************/
void save_dhm_mod(Mods_everythingStruct *data){
    char opIDs[1024]="";
    int i=0;
    char* modContent;
    Display *display;
    Window root;

    display = XtDisplay(data->widgetData->ifp_modsShell);
    root = DefaultRootWindow(display);


    //get the dhmop operation id list
    while (strlen(data->ModSettings->operation.type[i])){
	strncat(opIDs,data->ModSettings->operation.name[i], NWSRFS_ID_LENGTH);
	strcat (opIDs," ");
	i++;
    }

    modContent=getModContent();
    if (strcmp(modContent,"")){
	if (strncmp(modContent,PRECIP_MOD_KEYWORD,NWSRFS_ID_LENGTH)){
	    save_dhm_mod_in_b2_format(data, modContent);
        }
	else{
	    save_dhm_mod_in_b3_format(data, modContent);
        }
        load_new_mods_edit(data);
        XChangeProperty(
	        display,
	        root,
	        IFPA_number_of_mods_to_write,
	        IFPA_number_of_mods_to_write_type,
	        8,
	        PropModeReplace,
	        (unsigned char *)&data->ModIndex,
	        sizeof(int)
	        );
	}
}

/******************************************************
Routine: void save_dhm_mod_in_b2_format()
Purpose: This routine formats DSACST mod type B2 (e.g. One date: start date)
         from an input string.
Input(s):  Mods_everythingStruct - structure contain data that used in ifp_nwsrfs
           char * modcontent - Mods string entered from the display GUI
******************************************************/
/* Sac state mod */
void save_dhm_mod_in_b2_format(Mods_everythingStruct *data, char *modcontent)
{
    int jul_day_valid;    
    int jul_hour_valid;   
    int dlsdum;           
    int zondum = 1;       
    char *card1;
    char *card2;
    char *modName, *validDate;
    int yr, mn, dy, hr;
    char timeZone[5]="Z   ";
    char modSegmentId[NWSRFS_ID_LENGTH+1]; 
    char tmpMessage[MAX_MSG_LEN]; 
    char opId[NWSRFS_ID_LENGTH+1];
    float modVals[10];
    int numModVals,i;
    char keywords[9][7];

    card1=strtok(modcontent,"\n");
    card2=strtok(NULL,"\n");

    modName=strtok(card1," ");
    validDate=strtok(NULL," ");
    
     
    if(validDate != NULL)
    {
	sscanf(validDate,"%4d%2d%2d%2d%*s",&yr,&mn,&dy,&hr);
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, timeZone);
	JULDA(&jul_day_valid, &jul_hour_valid,&mn,&dy,&yr,&hr,&zondum,&dlsdum,timeZone);
    }
    else{
        jul_hour_valid = 0;
	jul_day_valid = 0;
    }

    parseLineTwoOfModCard(card2, modSegmentId, keywords, modVals,&numModVals,opId, tmpMessage);
    data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
   
    memset(data->ModArray[data->ModIndex], '\0', sizeof(ModInfo));
    data->ModArray[data->ModIndex]->b2.type = Mod_format_B2;
    strcpy(data->ModArray[data->ModIndex]->b2.command, data->ModName);

    data->ModArray[data->ModIndex]->b2.start_date = 0;
   
    data->ModArray[data->ModIndex]->b2.end_date = 0;

     
    if(validDate != NULL){
	   data->ModArray[data->ModIndex]->b2.valid_date = 24*(jul_day_valid - 1) + jul_hour_valid;
    }
    else {
       data->ModArray[data->ModIndex]->b2.valid_date = 0;
    }

    data->ModArray[data->ModIndex]->b2.type_of_id = SEGMENT;

    strcpy(data->ModArray[data->ModIndex]->b2.id, data->SegmentName);

    for (i=0;i<numModVals;i++){
        strcpy(data->ModArray[data->ModIndex]->b2.keyword[i],keywords[i]);
	data->ModArray[data->ModIndex]->b2.values[i] = modVals[i];
    }

    if (strcmp(opId,"")){
        strcpy(data->ModArray[data->ModIndex]->b2.opname[0],opId);
	data->ModArray[data->ModIndex]->b2.number_of_opers=1;
    }else
	data->ModArray[data->ModIndex]->b2.number_of_opers=0;

    data->ModIndex++;
}
/******************************************************
Routine: void save_dhm_mod_in_b3_format()
Purpose: This routine formats DPRECIP mod type B3 (e.g. three dates: start date,
         valid date and end date) from an input string.
Input(s):  Mods_everythingStruct - structure contain data that used in ifp_nwsrfs
           char * modcontent - Mods string entered from the display GUI
******************************************************/
/* Precip mod */
void save_dhm_mod_in_b3_format(Mods_everythingStruct *data, char *modcontent)
{
    int jul_day_start;    
    int jul_hour_start;   
    int jul_day_end;      
    int jul_hour_end;     
    int jul_day_valid;    
    int jul_hour_valid;   
    int dlsdum;           
    int zondum = 1;       
    char *card1;
    char *card2;
    char *modName, *startDate, *endDate, *validDate;
    int yr, mn, dy, hr;
    char timeZone[5]="Z   ";
    char modSegmentId[NWSRFS_ID_LENGTH+1], tmpMessage[MAX_MSG_LEN], opId[NWSRFS_ID_LENGTH+1];
    float modVals[10];
    int numModVals,i;
    char keywords[9][7];

    card1=strtok(modcontent,"\n");
    card2=strtok(NULL,"\n");

    modName=strtok(card1," ");
    startDate=strtok(NULL," ");
    endDate=strtok(NULL," ");
    validDate=strtok(NULL," ");
    sscanf(startDate,"%4d%2d%2d%2d%*s",&yr,&mn,&dy,&hr);

    if(startDate != NULL)
    {
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, timeZone);
        JULDA(&jul_day_start, &jul_hour_start,&mn,&dy,&yr,&hr,&zondum, &dlsdum,timeZone);
    }
    else{
        jul_hour_start = 0;
        jul_day_start = 0;
    }

    if(endDate != NULL)
    {
        sscanf(endDate,"%4d%2d%2d%2d%*s",&yr,&mn,&dy,&hr);
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, timeZone);
        JULDA(&jul_day_end, &jul_hour_end,&mn,&dy,&yr,&hr,&zondum,&dlsdum,timeZone);
    }
    else{
        jul_hour_end = 0;
        jul_day_end = 0;
    }

    if(validDate != NULL)
    {
        sscanf(validDate,"%4d%2d%2d%2d%*s",&yr,&mn,&dy,&hr);
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, timeZone);
	JULDA(&jul_day_valid, &jul_hour_valid,&mn,&dy,&yr,&hr,&zondum,	&dlsdum,timeZone);
    }
    else{
        jul_hour_valid = 0;
	jul_day_valid = 0;
    }

    parseLineTwoOfModCard(card2, modSegmentId, keywords, modVals,&numModVals,opId, tmpMessage);
    data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
   
    memset(data->ModArray[data->ModIndex], '\0', sizeof(ModInfo));
    data->ModArray[data->ModIndex]->b3.type = Mod_format_B3;
    strcpy(data->ModArray[data->ModIndex]->b3.command, data->ModName);

    if(startDate != NULL){
        data->ModArray[data->ModIndex]->b3.start_date = 24*(jul_day_start - 1) + jul_hour_start;
    }
    else {
      data->ModArray[data->ModIndex]->b3.start_date = 0;
    }
   
    if(endDate != NULL){
	   data->ModArray[data->ModIndex]->b3.end_date = 24*(jul_day_end - 1) + jul_hour_end;
    }
    else {
       data->ModArray[data->ModIndex]->b3.end_date = 0;
    }
    if(validDate != NULL){
	   data->ModArray[data->ModIndex]->b3.valid_date = 24*(jul_day_valid - 1) + jul_hour_valid;
    }
    else {
       data->ModArray[data->ModIndex]->b3.valid_date = 0;
    }

    data->ModArray[data->ModIndex]->b3.type_of_id = SEGMENT;

    strcpy(data->ModArray[data->ModIndex]->b3.id, data->SegmentName);

    /* All other Mods allow changing only 1 value at a time                 */
    data->ModArray[data->ModIndex]->b3.num_values = numModVals;

    /* malloc the space so it isn't reused when             */
    /*   the function exits...                           */
    data->ModArray[data->ModIndex]->b3.values =(float *) malloc(sizeof(float) * numModVals);

    for (i=0;i<numModVals;i++) {
        data->ModArray[data->ModIndex]->b3.values[i] = modVals[i];
    }
     
    strcpy(data->ModArray[data->ModIndex]->b3.keyword, "");
	
    if (strcmp(opId,"")){
        strcpy(data->ModArray[data->ModIndex]->b3.opname[0],opId);
        data->ModArray[data->ModIndex]->b3.number_of_opers=1;
    }else
        data->ModArray[data->ModIndex]->b3.number_of_opers=0;
    data->ModIndex++;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
