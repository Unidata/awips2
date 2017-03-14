#ifndef DHM_MODS_H
#define DHM_MODS_H

#include "c_call_f/fcitzc.h"
#include "c_call_f/julda.h"
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

#include "dhm.h"


#define FSERCH fserch_
#define MDYH2 mdyh2_
#define MOD_CARD_LENGTH 80
#define MAX_MOD_STRING_LENGTH 2500
#define MAX_NUMBER_OF_PRECIP_MODS 10
#define MAX_NUMBER_OF_SAC_MODS 10
#define PRECIP_MOD_KEYWORD    ".DPRECIP"
#define SAC_MOD_KEYWORD    ".DSACST"

extern int jvmTokenNotExists;
extern void FSERCH(int*, char[], int*, int[], int*);
extern void MDYH2(int*, int*, int*, int*, int*, int*, int*, int*, char[]);
extern void convertFromJulianHourToMMDDYYYYHHZ(int, char dateString[]);
extern void parseLineTwoOfModCard(char lineTwoOfModCard[], char modSegmentName[NWSRFS_ID_LENGTH+1],char keywords[9][7],
				  float modValues[9], int* numModVals, char opId[NWSRFS_ID_LENGTH+1], char returnMessage[MAX_MSG_LEN]);
extern void checkForValidModDates(int* startOfModInJulianHours, int* endOfModInJulianHours, int* validDateOfModInJulianHours, int* curentDateInJulianHours, char returnMessage[]);


typedef struct dhmprecipmodType{
    char startDateInMMDDYYYYHHZ[DATE_STRING_LENGTH];
    char endDateInMMDDYYYYHHZ[DATE_STRING_LENGTH];
    char validDateInMMDDYYYYHHZ[DATE_STRING_LENGTH];
    char operationId[NWSRFS_ID_LENGTH +1];
    char value[6];
    char lineTwoOfModCard[MOD_CARD_LENGTH];
}dhmprecipmodType;

typedef struct dsacstModType{
    char validDateInMMDDYYYYHHZ[DATE_STRING_LENGTH];
    char operationId[NWSRFS_ID_LENGTH+1];
    int numberOfKeywords;
    char keyword[9][7];     /* keywords, mandatory          */
    float values[9];        /* values assoc with keywords   */
    char lineTwoOfModCard[MOD_CARD_LENGTH];
}dsacstModType;
int dhmModsSelected;

int numberOfPrecipMods, numberOfDsacstMods;
struct dhmprecipmodType dhmprecipmod[MAX_NUMBER_OF_PRECIP_MODS];
struct dsacstModType dsacstMods[MAX_NUMBER_OF_SAC_MODS];

char *getModContent();
int is_dhm_mod_available();
void popdownModGui();
void popupModGui();
void popupProperties(char *, char *, char *, char *,char *,char *,char *);

extern char dhm_model_dataPath[120];


int load_dhm_precip_mod_string(char operationID[NWSRFS_ID_LENGTH+1], char modsString[MAX_MOD_STRING_LENGTH]);
int load_dhm_sac_state_mod_string(char operationID[NWSRFS_ID_LENGTH+1], char modsString[MAX_MOD_STRING_LENGTH]);

typedef struct dhmBasinData{
    char basinID[NWSRFS_ID_LENGTH+1];
    char operationID[NWSRFS_ID_LENGTH+1];
    
}dhmBasinData;

typedef struct basinPropsStruct{

    struct dhmBasinData basinData[10];
    int  count;
}basinPropsStruct;

struct basinPropsStruct basinProps;
void popup_dhm_mods_gui(Mods_everythingStruct  *, char[]);
char *getBasinID(char []);

int totalNumberPrecipModsFound;
int totalNumberSacStateModsFound;

#endif
