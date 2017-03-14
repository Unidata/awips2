
#include "dhm_mods.h"

/*.......................................
* File: parse_load_dhm_mods.c
* Author(s): DHM Team
* Date Created: 5/5/07
* Development group: OHD HSEB
* Purpose: This file has routines for parsing and storing a DPRECIP and DSACST
* mod definition; and routines for retrieving the mod definition for passing to java
* Module(s): 
*     parse_dhm_precip_mods
*     parse_dhm_sac_mods
*     load_dhm_sac_state_mod_string
*     load_dhm_precip_mod_string
*     init_dhm_mods
*     parseLineTwoOfModCard
*     checkForValidModDates
*     convertFromJulianHourToMMDDYYYYHHZ
*     convertDateToStringInZ
* Module(s) Called: 
*     FSERCH
*     remove_trailing_space
*     MDYH2
*     FCITZC
*.......................................
* Modified:
* 11/07 - added check to restrict precip mods to observed period
* */


extern void remove_trailing_space(char *inputString);
extern struct basinPropsStruct basinProps;
/******************************************************
Routine: void parse_dhm_precip_mods()
Purpose: This routine decodes dhm PRECIP mods and stores 
         them in a dhmprecipmod structure. Returns error messages (if any).
	 This routine is being called by mods.f using DPRECIP keyword
Input(s):  int*  MP - 
           int   p[] - 	   
	   char modCardContents[][] - Mod string array
	   int* startDate - start IFP date JulianHour
	   int* endDate - end of IFP run date JulianHour
	   int* validDate - format JulianHour
	  
	          
output(s): char returnMessage[] -
           int *returnMessageLength - length of Mod string
	   
input/ouput: int* cardIndex - current mod card index	

  
******************************************************/
void parse_dhm_precip_mods (int* MP,int P[],
                         char modCardContents[][MOD_CARD_LENGTH],int* startDate, int* endDate,
                         int* validDate, int* currentDate,  int* cardIndex, 
                         char returnMessage[MAX_MSG_LEN], int *returnMessageLength) {
    
    char RTNNAM[]="decodedhmprecipmod" ;
    int startIndex=1;
    int operationNumber=64;    
    char operationId[NWSRFS_ID_LENGTH+1];
    char modSegmentId[NWSRFS_ID_LENGTH+1];
    char modValueString[6];
    char tmpMessage[MAX_MSG_LEN];
    char lineOneOfModCard[MOD_CARD_LENGTH+1];
    char lineTwoOfModCard[MOD_CARD_LENGTH+1];
    char startDateString[DATE_STRING_LENGTH];
    char endDateString[DATE_STRING_LENGTH];
    char validDateString[DATE_STRING_LENGTH];
    int numModVals;
    float modValues[9];
    char keywords[9][7];
    
    strcpy(returnMessage,"\n0**READING DHM PRECIP MOD CARD:\n");
    
    convertFromJulianHourToMMDDYYYYHHZ(*startDate, startDateString);
    convertFromJulianHourToMMDDYYYYHHZ(*endDate, endDateString);
    convertFromJulianHourToMMDDYYYYHHZ(*validDate, validDateString); 
    
    sprintf(lineOneOfModCard, PRECIP_MOD_KEYWORD" %s %s %s\n",startDateString,endDateString,validDateString);
    strcat(returnMessage,lineOneOfModCard);
    
    strncpy(lineTwoOfModCard,modCardContents[*cardIndex],MOD_CARD_LENGTH);
    lineTwoOfModCard[80] = '\0';
    strcat(returnMessage,lineTwoOfModCard);
    /*search for dhm operation */
    FSERCH (&operationNumber,operationId,&startIndex,P,MP);
    
    if (startIndex == 0){
        sprintf(tmpMessage, "\n\n0**WARNING** IN DPRECIP MOD: DHM-OP FOR OPERATION "
	   "NAME %s NOT FOUND IN THIS SEGMENT.\n",operationId);
        strcat(returnMessage,tmpMessage);
        (*cardIndex)++;
	    *returnMessageLength = strlen(returnMessage);
        return ;
    }
    
    if (numberOfPrecipMods > MAX_NUMBER_OF_PRECIP_MODS) {
    	sprintf(tmpMessage, "\n\n0**WARNING** IN DPRECIP MOD: MAX NUMBER OF "
	   "MODS %d EXCEEDED. MODS NOT USED\n",MAX_NUMBER_OF_PRECIP_MODS);
        strcat(returnMessage,tmpMessage);
        (*cardIndex)++;
	    *returnMessageLength = strlen(returnMessage);
        return ;
    }

    remove_trailing_space(lineTwoOfModCard);
    if (lineTwoOfModCard[strlen(lineTwoOfModCard)-1]=='&'){ 
        (*cardIndex)++;
        lineTwoOfModCard[strlen(lineTwoOfModCard)-1]='\0';
        strncat(lineTwoOfModCard,modCardContents[*cardIndex],MOD_CARD_LENGTH);
    }

    parseLineTwoOfModCard(lineTwoOfModCard, modSegmentId, keywords, modValues,&numModVals,operationId, tmpMessage);
	
    sprintf(modValueString,"%f",modValues[0]);

    if(strlen(tmpMessage) > 0) {
        strcat(returnMessage,tmpMessage);
        (*cardIndex)++;
	*returnMessageLength = strlen(returnMessage);
        return ;
    }
    
    //see if mod has expired; and make changes accordingly; no need to return
    if (*validDate < *currentDate) {
    	if (*endDate > *validDate) {
            
	     
	    convertFromJulianHourToMMDDYYYYHHZ(*endDate, 
                                               endDateString);
            convertFromJulianHourToMMDDYYYYHHZ(*validDate,
                                                validDateString);
    	    sprintf(tmpMessage, "\n\n0**WARNING** IN DPRECIP MOD: MOD HAS EXPIRED "
    	                        "AND THE END OF MOD WAS CHANGED FROM %s TO %s\n",
    	                        endDateString, validDateString);
            strcat(returnMessage,tmpMessage);
            *endDate = *validDate;
    	}
    }	
    
     
    checkForValidModDates(startDate,endDate,validDate,currentDate,tmpMessage);
    
    // determine end date string again to account 
    //for possilbe changes in checkForValidModDates and expired test
 
    convertFromJulianHourToMMDDYYYYHHZ(*endDate, endDateString);
    
    if(strlen(tmpMessage) > 0) {
	strcat(returnMessage,tmpMessage);
        (*cardIndex)++;
	*returnMessageLength = strlen(returnMessage);
        //return ;
    }
     
    strcpy(dhmprecipmod[numberOfPrecipMods].startDateInMMDDYYYYHHZ,startDateString);
    strcpy(dhmprecipmod[numberOfPrecipMods].endDateInMMDDYYYYHHZ,endDateString);
    strcpy(dhmprecipmod[numberOfPrecipMods].validDateInMMDDYYYYHHZ,validDateString);
    strcpy(dhmprecipmod[numberOfPrecipMods].operationId,operationId);
    strcpy(dhmprecipmod[numberOfPrecipMods].value,modValueString);
    
    numberOfPrecipMods++; 
    *returnMessageLength = strlen(returnMessage);
    (*cardIndex)++;
    return;
}

/******************************************************
Routine: void parse_dhm_sac_mods ()
Purpose: This routine decodes dhm SAC state mods and stores 
         them in a dsacstMods structure. Returns error messages (if any).
	 This routine is being called by mods.f using DSACST keyword
Input(s):  int*  MP - 
           int   p[] - 	   
	   char modCardContents[][] - Mod string array
	   int* startDate - start IFP date JulianHour
	   int* endDate - end of IFP run date JulianHour
	   int* validDate - format JulianHour
	  
	          
output(s): char returnMessage[] -
           int *returnMessageLength - length of Mod string
	   
input/ouput: int* cardIndex - current mod card index	

  
******************************************************/
void parse_dhm_sac_mods (int* MP,int P[],
                         char modCardContents[][MOD_CARD_LENGTH],int* validDate,
                         int* currentDate,  int* cardIndex, 
                         char returnMessage[1024], int *returnMessageLength) {
    
    char RTNNAM[]="decodedhmsacmod" ;
    int startIndex=1;
    int operationNumber=64;    
    char operationId[NWSRFS_ID_LENGTH+1];
    char modSegmentId[NWSRFS_ID_LENGTH+1];
    char modValueString[6];
    char tmpMessage[MAX_MSG_LEN];
    char lineOneOfModCard[MOD_CARD_LENGTH+1];
    char lineTwoOfModCard[161];
    char validDateString[DATE_STRING_LENGTH];
    int numKeywords;
    float modValues[9];
    char keywords[9][7];
    int i;

    strcpy(returnMessage,"\n0**READING DHM SAC MOD CARD:\n");
    
    convertFromJulianHourToMMDDYYYYHHZ(*validDate, validDateString); 
    
    sprintf(lineOneOfModCard,".DSACST %s\n",validDateString);
    strcat(returnMessage,lineOneOfModCard);
    
    strncpy(lineTwoOfModCard,modCardContents[*cardIndex],MOD_CARD_LENGTH);
    lineTwoOfModCard[MOD_CARD_LENGTH] = '\0';
    //need to concat next line too if ends with &
    strcat(returnMessage,lineTwoOfModCard);
    
    FSERCH (&operationNumber,operationId,&startIndex,P,MP);
    
    if (startIndex == 0){
        sprintf(tmpMessage, "\n\n0**WARNING** IN DSACST MOD: DHM-OP FOR OPERATION "
	   "NAME %s NOT FOUND IN THIS SEGMENT.\n",operationId);
        strcat(returnMessage,tmpMessage);
        (*cardIndex)++;
	    *returnMessageLength = strlen(returnMessage);
        return ;
    }
    
    if (numberOfDsacstMods > MAX_NUMBER_OF_SAC_MODS) {
    	sprintf(tmpMessage, "\n\n0**WARNING** IN DSACST MOD: MAX NUMBER OF "
	"MODS %d EXCEEDED. MODS NOT USED\n",MAX_NUMBER_OF_SAC_MODS);
        strcat(returnMessage,tmpMessage);
        (*cardIndex)++;
	*returnMessageLength = strlen(returnMessage);
        return ;
    }


    remove_trailing_space(lineTwoOfModCard);
    if (lineTwoOfModCard[strlen(lineTwoOfModCard)-1]=='&'){ 
        (*cardIndex)++;
        lineTwoOfModCard[strlen(lineTwoOfModCard)-1]='\0';
	strncat(lineTwoOfModCard,modCardContents[*cardIndex],MOD_CARD_LENGTH);
	remove_trailing_space(lineTwoOfModCard);
    }

    parseLineTwoOfModCard(lineTwoOfModCard, modSegmentId, keywords, modValues,&numKeywords,operationId, tmpMessage);
	
    if(strlen(tmpMessage) > 0) {
        strcat(returnMessage,tmpMessage);
        (*cardIndex)++;
	*returnMessageLength = strlen(returnMessage);
        return ;
    }
     
    //add checkForValidModValues to make sure values are >= 0.0;and return a tmpMessage
    
    if(strlen(tmpMessage) > 0) {
	strcat(returnMessage,tmpMessage);
        (*cardIndex)++;
	*returnMessageLength = strlen(returnMessage);
        return ;
    }
    
    if (numKeywords>0){
        strcpy(dsacstMods[numberOfDsacstMods].validDateInMMDDYYYYHHZ,validDateString);
        strcpy(dsacstMods[numberOfDsacstMods].operationId,operationId);
        dsacstMods[numberOfDsacstMods].numberOfKeywords=numKeywords;
		
        for (i=0;i<numKeywords;i++){
            dsacstMods[numberOfDsacstMods].values[i]=modValues[i];
            strcpy(dsacstMods[numberOfDsacstMods].keyword[i],keywords[i]);
        }
    }
    
    numberOfDsacstMods++; 
    *returnMessageLength = strlen(returnMessage);
    (*cardIndex)++;
    return;
}

/******************************************************
Routine: void load_dhm_precip_mod_string()
Purpose: This routine generates a NWSRFS standard mod string format
	 for a DPRECIP MOD. This routine is being called by routine dhm_op.c
Input(s):  	   
	    char operationId[] - Operation ID 
output(s):	   
            char modsString[]- Mod string
	  
return:  number of DPRECIP mods found    
******************************************************/
int load_dhm_precip_mod_string (char operationId[NWSRFS_ID_LENGTH+1], char modsString[MAX_MOD_STRING_LENGTH])
{
    char currentOperationId[NWSRFS_ID_LENGTH+1];
    char modOperationId[NWSRFS_ID_LENGTH+1];
    char oneModString[120];
    char tmpStr[20];
    int counter, index;
    int numberOfPrecipModsFound = 0;
    
    strncpy(currentOperationId,operationId,NWSRFS_ID_LENGTH);
    currentOperationId[NWSRFS_ID_LENGTH]='\0';
    remove_trailing_space(currentOperationId);
    strcpy(modsString,"");
    if(numberOfPrecipMods == 0)return numberOfPrecipModsFound;
    
        strcpy(modsString,"|");
       
	for (counter=0; counter < numberOfPrecipMods; counter++) { 
            strcpy(modOperationId,dhmprecipmod[counter].operationId);
	    if ((strcmp(currentOperationId,modOperationId) == 0) || (strcmp(modOperationId,"") == 0)){
	        sprintf(oneModString, "%s %s %s %s %s\n",PRECIP_MOD_KEYWORD,
	        dhmprecipmod[counter].startDateInMMDDYYYYHHZ,
	        dhmprecipmod[counter].endDateInMMDDYYYYHHZ,
	        dhmprecipmod[counter].validDateInMMDDYYYYHHZ,
	        dhmprecipmod[counter].value);
	        strcat(modsString,oneModString);
		
		// match found
		numberOfPrecipModsFound++;
	    } // end of if
        }//end of for loop 
   
    return numberOfPrecipModsFound;
   
}

/******************************************************
Routine: void load_dhm_sac_state_mod_string()
Purpose: This routine generates a NWSRFS standard mod string format
	 for a DSACST MOD. This routine is being called by routine dhm_op.c
Input(s):  	   
	    char operationId[] - Operation ID 
output(s):	   
            char modsString[]- Mod string
	  
return:  number of DSACST mods found  
******************************************************/

int load_dhm_sac_state_mod_string (char operationId[NWSRFS_ID_LENGTH+1], char modsString[MAX_MOD_STRING_LENGTH])
{
    char currentOperationId[NWSRFS_ID_LENGTH+1];
    char modOperationId[NWSRFS_ID_LENGTH+1];
    char oneModString[120];
    char tmpStr[20];
    int counter, index;
    int  numberOfDsacstModsFound = 0;
    
    strncpy(currentOperationId,operationId,NWSRFS_ID_LENGTH);
    currentOperationId[NWSRFS_ID_LENGTH]='\0';
    remove_trailing_space(currentOperationId);
    strcpy(modsString,"");
    
    if(numberOfDsacstMods == 0)return numberOfDsacstModsFound;
    strcat(modsString,"|");
	
    	for (counter=0; counter < numberOfDsacstMods; counter++) { 
	
            strcpy(modOperationId,dsacstMods[counter].operationId);
            if ((strcmp(currentOperationId,modOperationId) == 0) || 
                (strcmp(modOperationId,"") == 0)){
	            sprintf(oneModString, "%s %s",SAC_MOD_KEYWORD,
	            dsacstMods[counter].validDateInMMDDYYYYHHZ);
		  
                    for (index=0;index<dsacstMods[counter].numberOfKeywords;index++){
		        sprintf(tmpStr," %s %f ",dsacstMods[counter].keyword[index],dsacstMods[counter].values[index]);
			strcat(oneModString,tmpStr);			
		    }
                    strcat(oneModString,"\n");
	            strcat(modsString,oneModString);
		    
		    // match found
		    numberOfDsacstModsFound++;
	    } // end of if
    	}//end of for loop 
	 
    return numberOfDsacstModsFound;
	
   
}

void init_dhm_mods(){
    numberOfPrecipMods = 0;
    totalNumberPrecipModsFound = 0;
    totalNumberSacStateModsFound = 0;
    numberOfDsacstMods = 0;    
    basinProps.count = 0;    
};

/******************************************************
Routine: void parseLineTwoOfModCard()
Purpose: this routine retrieves the operation id and mod value and segmentName from the 2nd
         line of the mod card with format: SegmentName VALUE [/OperationID] 
	 (e.g. ATIT2 2.0 /ATIT2HW) or SegmentName keyword VALUE [keyword value]* [/OperationID]
	 (e.g. ATIT2 UZTWC 2.0 lzfpc 2.3 /ATIT2HW) where items
	  in [] are optional and * means one or more
Input(s):  	   
	   char char lineTwoOfModCard[] - mod card 2 String
	   char modSegmentName[] - segment name
	   char keywords[][] - array of sac or precip key words
	   float modValues [] - array of mod values
	   int *numModVals - number of mod values
	   char opId[] - Operation ID
Output(s):
          char returnMessage[] - return warning string message
  
******************************************************/
void parseLineTwoOfModCard(char lineTwoOfModCard[], char modSegmentName[NWSRFS_ID_LENGTH+1],
                           char keywords[9][7], float modValues[10],int *numModVals, 
			   char opId[NWSRFS_ID_LENGTH+1], char returnMessage[MAX_MSG_LEN])
{
 
    char fields[20][10];
    char *fieldPtr;
    int numFields=0;
    int i, isAFloat;
	
    strcpy(modSegmentName,strtok(lineTwoOfModCard," "));
    fieldPtr=strtok(NULL," ");
    while (fieldPtr!=NULL){
        if (strcmp(fieldPtr,"")){
            strcpy(fields[numFields],fieldPtr);
            fieldPtr=strtok(NULL," ");
            numFields++;
        }
    }
    
    if (numFields<=0) {
        sprintf(returnMessage, "\n\n0**WARNING** IN DPRECIP MOD: INVALID MOD FORMAT " 
    	                       "NOT ENOUGH MOD PARAMETERS DEFINED, MOD NOT USED\n");
    }
    else{
        if (fields[numFields-1][0]=='/'){            
	    strncpy(opId,&fields[numFields-1][1],NWSRFS_ID_LENGTH);	     	     
            numFields--;	     
        }else if (fields[numFields-2][0]=='/'){
            strcpy(opId,fields[numFields-1]);	     
            numFields-=2;
        }
        else{
            strcpy(opId,"");
        }
	
	opId[NWSRFS_ID_LENGTH] = '\0';
	*numModVals=0;
        
	/* precip mod */         
	if (numFields==1){
            *numModVals=1;
            strcpy(keywords[0],"");
            isAFloat = sscanf(fields[0],"%f",&(modValues[0]));
            if (isAFloat != 1 ||  modValues[0]<0) {
                sprintf(returnMessage, "\n\n0**WARNING** IN DPRECIP MOD: INVALID MOD VALUE (%s) "
                                       "MUST BE A NUMBER > 0, MOD NOT USED\n",fields[0]);
                return ;
            }
        }
        else/* Sac state mod */             
	    for (i=0;i<numFields;i++){                 
		if ((i%2)==0)
                    strcpy(keywords[*numModVals],fields[i]);
                else{
                    isAFloat = sscanf(fields[i],"%f",&(modValues[*numModVals]));
    
                    if (isAFloat != 1 ||  modValues[*numModVals]<0) {
                        sprintf(returnMessage, "\n\n0**WARNING** IN DSACST MOD: INVALID MOD VALUE (%s) "
                                               "FOR KEYWORD (%s) MUST BE A NUMBER > 0, "
					       "MOD NOT USED\n",fields[i],keywords[*numModVals]);
                        return ;
                     }else
                         (*numModVals)++;				
                 }
             }

         strcpy(returnMessage,"");
    }	
}

/******************************************************
Routine: void checkModDates()
Purpose: This routine checks for valid mod input dates. Returns string 
         containing warning messages
Input(s):  
	   int* startOfModInJulianHours - start mod date JulianHour
	   int* endOfModInJulianHours - end mod run date JulianHour
	   int* validDateOfModInJulianHours -  JulianHour
	   int* currentDateInJulianHours - current run time JulianHour
	  
	          
output(s): char returnMessage[] - Warning message string
          
  
******************************************************/
void checkForValidModDates(int* startOfModInJulianHours, 
                           int* endOfModInJulianHours, 
                           int* validDateOfModInJulianHours, 
                           int* currentDateInJulianHours, 
                           char returnMessage[MAX_MSG_LEN]) 
{ 
    char tmpMessage[MAX_MSG_LEN];
    char startDateString[DATE_STRING_LENGTH], endDateString[DATE_STRING_LENGTH], validDateString[DATE_STRING_LENGTH];
     
    strcpy(returnMessage,"");
    
    if (*endOfModInJulianHours < *startOfModInJulianHours ) {
    	convertFromJulianHourToMMDDYYYYHHZ(*startOfModInJulianHours, startDateString);
        convertFromJulianHourToMMDDYYYYHHZ(*endOfModInJulianHours, endDateString);
    	sprintf(tmpMessage, "\n\n0**WARNING** IN DPRECIP MOD: START OF MOD (%s) AFTER " 
    	                    "END OF MOD (%s). MOD NOT USED\n",
    	                    startDateString,endDateString);
        strcat(returnMessage,tmpMessage);
	*endOfModInJulianHours = *startOfModInJulianHours;
    }
    
    if (*endOfModInJulianHours > *validDateOfModInJulianHours ) {
    	convertFromJulianHourToMMDDYYYYHHZ(*validDateOfModInJulianHours, validDateString);
        convertFromJulianHourToMMDDYYYYHHZ(*endOfModInJulianHours, endDateString);
    	sprintf(tmpMessage, "\n\n0**WARNING** IN DPRECIP MOD: END OF MOD (%s) CHANGED "
    	                    "TO (%s). \nPRECIP MODS NOT VALID IN FORECAST PERIOD\n",
    	                    endDateString,validDateString);
        strcat(returnMessage,tmpMessage);
	*endOfModInJulianHours = *validDateOfModInJulianHours;
    }
    return;
}

/******************************************************
Routine: void convertFromJulianHourToMMDDYYYYHHZ()
Purpose: this routines converts date in Julian hours to MMDDYYYYHHZ date string format
input(s): int julianHour - hours
output(s): char dateString[] - MMDDYYYYHHZ date string
******************************************************/
void convertFromJulianHourToMMDDYYYYHHZ(int julianHour, char dateString[DATE_STRING_LENGTH])
{  
    int julianDay;
    int julianHourOfDay;
    int month, day, year, hour, daylightSwitch, timeZoneNumber;
    
    julianDay = (julianHour/24) + 1;
    julianHourOfDay = fmod(julianHour,24);
    MDYH2 (&julianDay, &julianHourOfDay, &month, &day, &year, &hour, &timeZoneNumber, &daylightSwitch,"Z   ");
    sprintf(dateString,"%02d%02d%04d%02dZ",month,day,year,hour);
    return;
}


/******************************************************
Routine: void convertDateToStringInZ()
Purpose: this routines converts date to MMDDYYYYHHZ date string format
input(s): 
        int fourDigitYear - year
	int month - Month
	int day - day of moth
	int hour - 
	char timeZone[]
output(s): char dateString[] - MMDDYYYYHHZ date string
******************************************************/

void convertDateToStringInZ(
        int fourDigitYear,
	int month,
	int day,
	int hour,
	char timeZone[],
	char outDateString[DATE_STRING_LENGTH]) {

    int daylightSwitch;           					    
    int timeZoneNumber;         
    int julianDay;          
    int julianHourOfDay;  
    char timeZoneNoNullChar[4];     
    int tzLength = strlen(timeZone);
    int i;
    
    for(i = tzLength; i<4; i++) {
        timeZone[i]= ' ';
    }
    timeZone[4]= '\0'; 
    strncpy(timeZoneNoNullChar,timeZone,4);  
         
    FCITZC(&timeZoneNumber, &daylightSwitch, timeZoneNoNullChar);
    JULDA(
        &julianDay, 
        &julianHourOfDay,
        &month,
        &day,
        &fourDigitYear,
	&hour,
        &timeZoneNumber,
        &daylightSwitch,
        timeZoneNoNullChar);
    
    MDYH2 (&julianDay, &julianHourOfDay, &month, &day, &fourDigitYear, &hour, &timeZoneNumber, &daylightSwitch,"Z   ");    
    sprintf(outDateString,"%02d%02d%02d%02d Z",fourDigitYear%100,month,day,hour);     
    return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

