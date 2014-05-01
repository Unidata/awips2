#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "dhm_mods.h"
extern void popdownModGui();
extern void popupProperties(char *, char *, char *, char *, char *, char *,char *);
int dhmModsSelected = 0;
extern void create_dhm_mod_object();
extern char dsacCurOpname[10]; /*variable save in Mods_callbacks.c */
extern void convertDateToStringInZ(int, int, int , int, char[], char[]);
extern void load_dhm_sac_mod_string (char [], char []);
extern char *getBasinID(char []);
void popup_dhm_mods_gui(Mods_everythingStruct *data, char curOpName[])
{
    char  modsString[MAX_MOD_STRING_LENGTH]="";
    char  startDate[DATE_STRING_LENGTH];
    char  endDate[DATE_STRING_LENGTH];
    char  validDate[DATE_STRING_LENGTH];  
    char  sacmodString[MAX_MOD_STRING_LENGTH] = "";
    char  precipmodString[MAX_MOD_STRING_LENGTH] = "";
    char  curBasinID[NWSRFS_ID_LENGTH+1]="";
    static int firstTime = 1;
        /* jvmTokenNotExists equals 1 if jvm is enable     */
	/* calling java codes only when jvm is enable      */
	
        if(jvmTokenNotExists == 1)
	{
	  
	    if( strcasecmp(data->ModName,"DSACST") == 0 || strcasecmp(data->ModName,"DPRECIP") == 0)
	    {   
	        XtSetSensitive(data->widgetData->modsCreate,FALSE);
	        
               
		if ( dhmModsSelected == 0) {
		     dhmModsSelected = 1;
		}
		
		if(firstTime){   
		    create_dhm_mod_object(); 
		    firstTime = 0;
                }
		
		
		load_dhm_sac_state_mod_string (curOpName, modsString);
		
		strcat(modsString,sacmodString);
		
		load_dhm_precip_mod_string (curOpName, precipmodString);
		
	        strcat(modsString,precipmodString);
							     
	        convertDateToStringInZ(
	           data->ModDates->StartDate->year,
	           data->ModDates->StartDate->month,
	           data->ModDates->StartDate->day,
	           data->ModDates->StartDate->hour,
	           data->ModDates->StartDate->time_zone,startDate);

	        convertDateToStringInZ(
		    data->ModDates->EndDate->year,
		    data->ModDates->EndDate->month,
		    data->ModDates->EndDate->day,
		    data->ModDates->EndDate->hour,
		    data->ModDates->EndDate->time_zone,endDate);

	        convertDateToStringInZ(
		    data->ModDates->ValidDate->year,
		    data->ModDates->ValidDate->month,
		    data->ModDates->ValidDate->day,
		    data->ModDates->ValidDate->hour,
		    data->ModDates->ValidDate->time_zone,validDate);
	      
	       popupProperties(
		    data->ModName,data->SegmentName,
		    curOpName, startDate, validDate, modsString,getBasinID(curOpName));
		   
	    }else
	    {
	
                popdownModGui();
		dhmModsSelected = 0;
	        /*enable Create button from Mod Viewer window */
	        XtSetSensitive(data->widgetData->modsCreate,TRUE);
	    }
       }
       

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

 }
