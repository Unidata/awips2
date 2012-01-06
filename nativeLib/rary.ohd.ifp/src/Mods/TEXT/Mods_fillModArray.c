/********************************************************************************/
/*										*/
/*	Mods_fillModArray.c							*/
/*										*/
/*	Coded by:	Tom Adams - NWS/Hydrologic Research Laboratory		*/
/*	Date:		10/30/90						*/
/*	Modified:	05/02/91						*/
/*	Last modified:	11/23/94                                                */
/*                      05/19/95 - D. Page      				*/
/*			11/06/95 - D. Page					*/
/*		        03/09/01 - J. Gofus					*/
/*		        04/04/01 - A. Vo					*/
/*										*/
/*	This file contains functions, callbacks & otherwise, to			*/
/*	get data from the mods_popup window (Run-time Modifications)		*/
/*	and put the data into the appropriate mods data structure, used		*/
/*	to generate ASCII data files in FORTRAN card image format for		*/
/*	the coupled NWSRFS ver. 5 FORTRAN program.				*/
/*										*/
/*	See: "Mods_info.h" - formats A1, A2, B1, B2, or B3		      	*/
/*                                                                              */
/*      add code to handle UHGCDATE --AV 4/15/04                                */
/*                  								*/
/********************************************************************************/


#include <stdlib.h>
#include <math.h>

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"
#include "Mods_defStruct.h" /* jgg 2/21/01 */
#include "c_call_f/fcitzc.h" /*--Added by AV --*/
#include "c_call_f/julda.h"


int compare_A1_modArrays(ModInfo *, ModInfo *);
int compare_A2_modArrays(ModInfo *, ModInfo *);
int compare_B1_modArrays(ModInfo *, ModInfo *);
int compare_B2_modArrays(ModInfo *, ModInfo *);
int compare_B3_modArrays(ModInfo *, ModInfo *);
void save_A1(Mods_everythingStruct *, char *,date *, date *, date *);
void save_A2(Mods_everythingStruct *, char *,date *, date *, date *);

void save_B1(Mods_everythingStruct *, char *, date *, date *, date *, int );
void save_B2(Mods_everythingStruct *, char *, date *, date *, date *, int );
void save_B3(Mods_everythingStruct *, char *, date *, date *, date *, int );

int getModTSdates(); /*Modified by gzhou 02/2004 */
/*static int test_last_two_mods(Mods_everythingStruct *);*/
extern char *get_fgroup_name();


/*extern int find_in_ts (char *, char *, int, float[], char[][]);*/
extern int find_in_ts ();
/*extern void  mp_create_mods(Mods_everythingStruct *, int , char *,int *, int *);*/
extern void  mp_create_mods(Mods_everythingStruct *, int , char *,int *, int *);
extern void PrintWarning();   
/* *******************************************************************************************

	The following functions malloc space for the ModInfo array and fill the appropriate
	structure for the specific Run-time Mod...

		( see: "mods_info.h" - formats A1, A2, B1, B2, or B3)

   ******************************************************************************************* */


void save_A1(Mods_everythingStruct *data, char *currentSegment,
		date *startDate, date *endDate, date *validDate)
{
/*      Format type:    A1                      */

	int             jul_day_start;   /* Julian start day */
	int             jul_hour_start;  /* Julian start hour */
	int             jul_day_end;     /* Julian end day */
	int             jul_hour_end;    /* Julian end hour */
	int             jul_day_valid;   /* Julian valid day */
	int             jul_hour_valid;  /* Julian valid hour */
	int             dlsdum;          /* Daylight savings time flag,
					    1 for  daylight savings time,
					    0 for standard time */
	int             zondum = 1;      /* time zone value */
	int             j;               /* counter */
	int             deltaT;          /* time series time interval */
	int             locTS;           /* location of time series array */
	int             numberOfSelectedOperations = 0;
	int             selectedWidgetIndex;


	Display         *display;
	Window          root;



 display = XtDisplay(data->widgetData->ifp_modsShell);
 root = DefaultRootWindow(display);


deltaT = data->ModSettings->time_series.delta_t[0];
if(deltaT <= 0) deltaT = 6;

if(startDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, startDate->time_zone);
	JULDA(&jul_day_start, &jul_hour_start,
		&startDate->month,
		&startDate->day,
		&startDate->year,
		&startDate->hour,
		&zondum, &dlsdum, startDate->time_zone);
}
else    {
	jul_hour_start = 0;
	jul_day_start = 0;
	}


if(endDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, endDate->time_zone);
	JULDA(&jul_day_end, &jul_hour_end,
		&endDate->month,
		&endDate->day,
		&endDate->year,
		&endDate->hour,
		&zondum, &dlsdum, endDate->time_zone);
}
else    {
	jul_hour_end = 0;
	jul_day_end = 0;
	}


if(validDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, validDate->time_zone);
	JULDA(&jul_day_valid, &jul_hour_valid,
		&validDate->month,
		&validDate->day,
		&validDate->year,
		&validDate->hour,
		&zondum, &dlsdum, validDate->time_zone);
}
else    {
	jul_hour_valid = 0;
	jul_day_valid = 0;
	}


for(j = 0; j <= data->opTSdata->num; j++)
	{
	if(operations_number[j] == 1) numberOfSelectedOperations++;
	}

data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
memset(data->ModArray[data->ModIndex], '\0', sizeof(ModInfo));
data->ModArray[data->ModIndex]->a1.type = Mod_format_A1;



strcpy(data->ModArray[data->ModIndex]->a1.command, data->ModName);
strcpy(data->ModArray[data->ModIndex]->a1.segment_id, currentSegment);

if(startDate != NULL)
	data->ModArray[data->ModIndex]->a1.start_date = 24*(jul_day_start - 1) + jul_hour_start;
else    data->ModArray[data->ModIndex]->a1.start_date = 0;
if(endDate != NULL)
	data->ModArray[data->ModIndex]->a1.end_date = 24*(jul_day_end - 1) + jul_hour_end;
else    data->ModArray[data->ModIndex]->a1.end_date = 0;
if(validDate != NULL)
	data->ModArray[data->ModIndex]->a1.valid_date = 24*(jul_day_valid - 1) + jul_hour_valid;
else    data->ModArray[data->ModIndex]->a1.valid_date = 0;

for(j = 0; j < 20; j++)
	data->ModArray[data->ModIndex]->a1.dates[j].date = 0;

getModTSdates(data, deltaT, data->ModArray[data->ModIndex]->a1.dates);

data->ModArray[data->ModIndex]->a1.number_of_ts = numberOfSelectedOperations;

for(j = 0; j < 10; j++)
	data->ModArray[data->ModIndex]->a1.info[j] = NULL;

selectedWidgetIndex = 0;
for(j = 0; j < data->opTSdata->num; j++)
	{
	if(operations_number[j] == 1)
		{
		deltaT = data->ModSettings->time_series.delta_t[j];
		locTS = find_in_ts(
				data->ModSettings->time_series.id[j],
				data->ModSettings->time_series.datatype[j],
				deltaT,
				data->ofsData->ts_float_array,
				data->ofsData->ts_char_array
				);

		data->ModArray[data->ModIndex]->a1.info[selectedWidgetIndex] =
						(TS_INFO *)malloc(sizeof(TS_INFO));
		data->ModArray[data->ModIndex]->a1.info[selectedWidgetIndex]->ts_type =
						(int)data->ofsData->ts_float_array[locTS - 1];
		data->ModArray[data->ModIndex]->a1.info[selectedWidgetIndex]->delta_t = deltaT;

		memset(data->ModArray[data->ModIndex]->a1.info[selectedWidgetIndex]->ts_id, '\0', 9);
		memset(data->ModArray[data->ModIndex]->a1.info[selectedWidgetIndex]->data_type, '\0', 5);

		strncpy(data->ModArray[data->ModIndex]->a1.info[selectedWidgetIndex]->ts_id,
				data->ModSettings->time_series.id[j], 8);
		strncpy(data->ModArray[data->ModIndex]->a1.info[selectedWidgetIndex]->data_type,
				data->ModSettings->time_series.datatype[j], 4);
		selectedWidgetIndex++;
		}
	}

j = 0;
while(data->ModArray[data->ModIndex]->a1.dates[j].date != 0)
	{
	j++;
	if( j == MAX_NUMBER_OF_SELECTABLE_DATES) break;
	}

data->ModIndex++;
}



void save_A2(Mods_everythingStruct *data, char *currentSegment,
		date *startDate, date *endDate, date *validDate)
{
/*      Format type:    A2                      */






}


void save_B1(Mods_everythingStruct *data, char *currentSegment,
		date *startDate, date *endDate, date *validDate, int modsgroup)
{
/*      Format type:    B1                      */


	int             jul_day_start, jul_hour_start;
	int             jul_day_end, jul_hour_end;
	int             jul_day_valid;   /* Julian valid day */
	int             jul_hour_valid;  /* Julian valid hour */
	int             dlsdum, zondum = 1;
	int             j, deltaT;
	int             numberOfSelectedOperations = 0;
	int             selectedWidgetIndex;
	short 		position;
	Widget		selection;


	Display         *display;
	Window          root;


 display = XtDisplay(data->widgetData->ifp_modsShell);
 root = DefaultRootWindow(display);



deltaT = data->ModSettings->time_series.delta_t[0];
if(deltaT <= 0) deltaT = 6;

if(startDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, startDate->time_zone);
	JULDA(&jul_day_start, &jul_hour_start,
		&startDate->month,
		&startDate->day,
		&startDate->year,
		&startDate->hour,
		&zondum, &dlsdum, startDate->time_zone);
}
else    {
	jul_hour_start = 0;
	jul_day_start = 0;
	}


if(endDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, endDate->time_zone);
	JULDA(&jul_day_end, &jul_hour_end,
		&endDate->month,
		&endDate->day,
		&endDate->year,
		&endDate->hour,
		&zondum, &dlsdum, endDate->time_zone);
}
else    {
	jul_hour_end = 0;
	jul_day_end = 0;
	}


if(validDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, validDate->time_zone);
	JULDA(&jul_day_valid, &jul_hour_valid,
		&validDate->month,
		&validDate->day,
		&validDate->year,
		&validDate->hour,
		&zondum, &dlsdum, validDate->time_zone);
}
else    {
	jul_hour_valid = 0;
	jul_day_valid = 0;
	}



for(j = 0; j < data->opTSdata->num; j++)
	if(operations_number[j] == 1) numberOfSelectedOperations++;

data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
memset(data->ModArray[data->ModIndex], '\0', sizeof(ModInfo));
data->ModArray[data->ModIndex]->b1.type = Mod_format_B1;
strcpy(data->ModArray[data->ModIndex]->b1.command, data->ModName);

if(startDate != NULL)
	data->ModArray[data->ModIndex]->b1.start_date = 24*(jul_day_start - 1) + jul_hour_start;
else    data->ModArray[data->ModIndex]->b1.start_date = 0;
if(endDate != NULL)
	data->ModArray[data->ModIndex]->b1.end_date = 24*(jul_day_end - 1) + jul_hour_end;
else    data->ModArray[data->ModIndex]->b1.end_date = 0;
if(validDate != NULL)
	data->ModArray[data->ModIndex]->b1.valid_date = 24*(jul_day_valid - 1) + jul_hour_valid;
else    data->ModArray[data->ModIndex]->b1.valid_date = 0;

  switch (modsgroup){
	case FGROUP:
  		data->ModArray[data->ModIndex]->b1.type_of_id = FGROUP;
  		currentSegment = (char *)get_fgroup_name();
		break;
	case RANGE:
  		/*data->ModArray[data->ModIndex]->b1.type_of_id = FGROUP;*/
		strcpy(currentSegment,range_selected.seg_str[range_selected.first]);
		strcat(currentSegment,"-");
		strcat(currentSegment,range_selected.seg_str[range_selected.last]);
		break;
	case SEGMENT:
  		data->ModArray[data->ModIndex]->b1.type_of_id = SEGMENT;
		break;
	default:
		break;
	}
strcpy(data->ModArray[data->ModIndex]->b1.id, currentSegment);

if(data->selectedModDef->modOption)
	{
	XtVaGetValues(data->widgetData->optionsMenu, XmNmenuHistory, &selection, NULL);
	XtVaGetValues(selection, XmNpositionIndex, &position, NULL);
	strcpy(data->ModArray[data->ModIndex]->b1.keyword, 
	       data->Options->keyword[position]);
	}

for(j = 0; j < 20; j++)
	data->ModArray[data->ModIndex]->b1.dates[j].date = 0;

/* modified by gzhou for bug r21-45
getModTSdates(data, deltaT, data->ModArray[data->ModIndex]->b1.dates);
*/
int result = getModTSdates(data, deltaT, data->ModArray[data->ModIndex]->b1.dates);
if(result == 0) return;


selectedWidgetIndex = 0;
if(data->opTSdata->num == numberOfSelectedOperations)            /* If all the operations are selected   */
	data->ModArray[data->ModIndex]->b1.number_of_opers = 0;     /* set '.number_of_opers == 0,          */
else    {                                                       /* otherwise, fill the name array       */
	data->ModArray[data->ModIndex]->b1.number_of_opers = numberOfSelectedOperations;

	for(j = 0; j < data->opTSdata->num; j++)
		{
		if(operations_number[j] == 1)
			{
			memset(data->ModArray[data->ModIndex]->b1.opname[selectedWidgetIndex], '\0', 9);
			strncpy(data->ModArray[data->ModIndex]->b1.opname[selectedWidgetIndex],
				data->ModSettings->operation.name[j], 8);
			selectedWidgetIndex++;
			}
		}
	}



data->ModIndex++;
}


void save_B2(Mods_everythingStruct *data, char *currentSegment,
		date *startDate, date *endDate, date *validDate, int modsgroup)
{
/*      Format type:    B2                      */

	int             jul_day_start, jul_hour_start;
	int             jul_day_end, jul_hour_end;
	int             jul_day_valid;   /* Julian valid day */
	int             jul_hour_valid;  /* Julian valid hour */
	int             dlsdum, zondum = 1;
	int             j, deltaT;
	int             numberOfSelectedOperations = 0;
	int             selectedWidgetIndex;
	short           position;
	Widget		selection;

	Display         *display;
	Window          root;
	int             MaxIndx;/*AV added 4/4/01 */
        int             tcurval;
/* Following symbols and variables added by jgg - some for testing  */



    int index;
    int i;
    float testVal;

/* Should the following be defined in a .h?  */

    char saccoKeyword[7][6] = {
         {"UZTWC\0"},
         {"UZFWC\0"},
         {"LZTWC\0"},
         {"LZFSC\0"},
         {"LZFPC\0"},
         {"ADIMC\0"},
         {"FGIX\0"}
    };

    WaterLevelType WL;
    

    

/*
dateChange = data->ModArray[data->ModIndex]->b2.start_date - PrevStartdate;
printf("in Mods_fillModArray datechange= %d\n",dateChange);
 */ 
 
if(data->selectedModDef->DataWinType == SACCO_TYPE)
{
/* Initialize SACCO struct (WL)  */
   
   for( i=0; i<7; i++)  
   {  
       WL.MaxWaterLevels[i] = data->WaterLvl->MaxWaterLevels[i];   
       WL.CurrentWaterLevels[i] = data->WaterLvl->CurrentWaterLevels[i];  
       WL.NewWaterLevels[i] = data->WaterLvl->NewWaterLevels[i];  
     
   }   
}
else
{
   for( i=0; i<7; i++)  
   {  
       WL.MaxWaterLevels[i] = -999.0;   
       WL.CurrentWaterLevels[i] = -999.0;  
       WL.NewWaterLevels[i] = -999.0;  
   }   


}

 display = XtDisplay(data->widgetData->ifp_modsShell);
 root = DefaultRootWindow(display);

deltaT = data->ModSettings->time_series.delta_t[0];
if(deltaT <= 0) deltaT = 6;

if(startDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, startDate->time_zone);        
	JULDA(&jul_day_start, &jul_hour_start,
		&startDate->month,
		&startDate->day,
		&startDate->year,
		&startDate->hour,
		&zondum, &dlsdum, startDate->time_zone);
}
else    {
	jul_hour_start = 0;
	jul_day_start = 0;
	}


if(endDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, endDate->time_zone);        
	JULDA(&jul_day_end, &jul_hour_end,
		&endDate->month,
		&endDate->day,
		&endDate->year,
		&endDate->hour,
		&zondum, &dlsdum, endDate->time_zone);
}
else    {
	jul_hour_end = 0;
	jul_day_end = 0;
	}


if(validDate != NULL)
{
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, validDate->time_zone);        
	JULDA(&jul_day_valid, &jul_hour_valid,
		&validDate->month,
		&validDate->day,
		&validDate->year,
		&validDate->hour,
		&zondum, &dlsdum, validDate->time_zone);
}
else    {
	jul_hour_valid = 0;
	jul_day_valid = 0;
	}


    for(j = 0; j < data->opTSdata->num; j++)
        {
        if(operations_number[j] == 1) numberOfSelectedOperations++;
        }

    data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
    memset(data->ModArray[data->ModIndex], '\0', sizeof(ModInfo));
    data->ModArray[data->ModIndex]->b2.type = Mod_format_B2;
    strcpy(data->ModArray[data->ModIndex]->b2.command, data->ModName);  

    if(startDate != NULL)
    
        data->ModArray[data->ModIndex]->b2.start_date = 24*(jul_day_start - 1) + jul_hour_start;
        
       
    else    data->ModArray[data->ModIndex]->b2.start_date = 0;
  
    if(endDate != NULL)
        data->ModArray[data->ModIndex]->b2.end_date = 24*(jul_day_end - 1) + jul_hour_end;
    else    data->ModArray[data->ModIndex]->b2.end_date = 0;
    if(validDate != NULL)
        data->ModArray[data->ModIndex]->b2.valid_date = 24*(jul_day_valid - 1) + jul_hour_valid;
    else    data->ModArray[data->ModIndex]->b2.valid_date = 0;

    switch (modsgroup){
        case FGROUP:
                data->ModArray[data->ModIndex]->b2.type_of_id = FGROUP;
                currentSegment = (char *)get_fgroup_name();
                break;
        case RANGE:
                /*data->ModArray[data->ModIndex]->b2.type_of_id = FGROUP;*/
                strcpy(currentSegment,range_selected.seg_str[range_selected.first]);
                strcat(currentSegment,"-");
                strcat(currentSegment,range_selected.seg_str[range_selected.last]);
                break;
        case SEGMENT:
                data->ModArray[data->ModIndex]->b2.type_of_id = SEGMENT;
                break;
        default:
                break;
        }
    strcpy(data->ModArray[data->ModIndex]->b2.id, currentSegment);

    for(j = 0; j < 9; j++)  /* Initialize values & keywords...                      */
        {
        data->ModArray[data->ModIndex]->b2.values[j] = -999;
        memset(data->ModArray[data->ModIndex]->b2.keyword[j], '\0', 7);
        }

    if(data->selectedModDef->DataWinType == SACCO_TYPE)    /* This is the new branch for the sacco processing - 3/9/01 */
    {


/* The following section handles /opname in the mod */

        selectedWidgetIndex = 0;
        if(data->opTSdata->num == numberOfSelectedOperations)            /* If all the operations are selected   */
        {                                                                /* set '.number_of_opers == 0,          */
           data->ModArray[data->ModIndex]->b2.number_of_opers = 0;
        } 
    else
    {

        data->ModArray[data->ModIndex]->b2.number_of_opers = numberOfSelectedOperations;

        for(j = 0; j < data->opTSdata->num; j++)
        {
            if(operations_number[j] == 1)
            {
                memset(data->ModArray[data->ModIndex]->b2.opname[selectedWidgetIndex], '\0', 9);
                strncpy(data->ModArray[data->ModIndex]->b2.opname[selectedWidgetIndex],
                data->ModSettings->operation.name[j], 8);

                selectedWidgetIndex++;
            }  /* end of if(operations...)  */
        }  /*  end of for(j...)  */
    }  /*  end of else "fill the name array"  */


/* Here is where we read the sacco struct and determine what to write out to the ModArray */
    index = 0;  
    if(WL.CurrentWaterLevels[6] == -999) MaxIndx = 6;
        else MaxIndx = 7;
    
    for(j = 0; j < MaxIndx; j++)  /*  read the appropriate struct array  */
    {
   
     /*printf("S_B2: for %i, sacNewVal = %f, sacCOVal = %f \n", j, WL.NewWaterLevels[j], WL.CurrentWaterLevels[j]); *//* jggDbug */

         
          if(fabs(WL.NewWaterLevels[j] - WL.CurrentWaterLevels[j])>= 0.009){
              strcpy(data->ModArray[data->ModIndex]->b2.keyword[index], saccoKeyword[j]);
              data->ModArray[data->ModIndex]->b2.values[index] = WL.NewWaterLevels[j];
          }
       
         index++;
       /* }*/

    } 
    data->ModIndex++;  




} /* end of SACCO type if */
else
{


    selectedWidgetIndex = 0;
    if(data->opTSdata->num == numberOfSelectedOperations)            /* If all the operations are selected   */
	{                                                       /* set '.number_of_opers == 0,          */
	data->ModArray[data->ModIndex]->b2.number_of_opers = 0;

	if(data->selectedModDef->modOption) {
		XtVaGetValues(data->widgetData->optionsMenu, XmNmenuHistory, &selection, NULL);
	        XtVaGetValues(selection, XmNpositionIndex, &position, NULL);
	        strcpy(data->ModArray[data->ModIndex]->b1.keyword, 
	               data->Options->keyword[position]);
		}
	}
    else    {       /* otherwise, fill the name array       */

	data->ModArray[data->ModIndex]->b2.number_of_opers = numberOfSelectedOperations;

	for(j = 0; j < data->opTSdata->num; j++)
		{
		if(operations_number[j] == 1)
			{
			memset(data->ModArray[data->ModIndex]->b2.opname[selectedWidgetIndex], '\0', 9);
			strncpy(data->ModArray[data->ModIndex]->b2.opname[selectedWidgetIndex],
				data->ModSettings->operation.name[j], 8);

			if(data->selectedModDef->modOption) {
				XtVaGetValues(data->widgetData->optionsMenu, XmNmenuHistory, &selection, NULL);
	                        XtVaGetValues(selection, XmNpositionIndex, &position, NULL);
	                        strcpy(data->ModArray[data->ModIndex]->b1.keyword, 
	                               data->Options->keyword[position]);
				} /* end of if(data...) */

			selectedWidgetIndex++;
			}  /* end of if(operations...)  */
		}  /*  end of for(j...)  */
	}  /*  end of else "fill the name array"  */

    data->ModIndex++;

    }  /*  end of non-SACCO save_B2 processing  */

}  

void save_B3(Mods_everythingStruct *data, char *currentSegment,
		date *startDate, date *endDate, date *validDate, int modsgroup)
{
/*      Format type:    B3                      */

	int             i, j;
	int             numberOfSelectedOperations = 0;
	int             dlsdum, zondum = 1;
	int             jul_day_start, jul_hour_start;
	int             jul_day_end, jul_hour_end;
	int             jul_day_valid;   /* Julian valid day */
	int             jul_hour_valid;  /* Julian valid hour */
	int             selectedWidgetIndex;
	Arg             wargs[2];
	char            *string;
	XmString        *xmValues_array;
	int		xmItemCount;

	int             scale_value;
	int             decimal_conversion = 1;
	short           decimal_points;



        
        for(j = 0; j < data->opTSdata->num; j++)if(operations_number[j] == 1) numberOfSelectedOperations++;
	/* calulate startdate, enddate and valid date  UHGCDATE -- AV 4/15/04*/
        if(startDate != NULL){
	        /* replace call to julda_2 with fcitzc_ and julda_ for y2k */
                FCITZC(&zondum, &dlsdum, startDate->time_zone);        
		JULDA(&jul_day_start, &jul_hour_start,
			&startDate->month,
			&startDate->day,
			&startDate->year,
			&startDate->hour,
			&zondum, &dlsdum, startDate->time_zone);
	} 
	else {
		jul_hour_start = 0;
		jul_day_start = 0;
	}


	if(endDate != NULL){
	        /* replace call to julda_2 with fcitzc_ and julda_ for y2k */
                FCITZC(&zondum, &dlsdum, endDate->time_zone);        
		JULDA(&jul_day_end, &jul_hour_end,
			&endDate->month,
			&endDate->day,
			&endDate->year,
			&endDate->hour,
			&zondum, &dlsdum, endDate->time_zone);
	}
	else    {
		jul_hour_end = 0;
		jul_day_end = 0;
		}


	
        if(validDate != NULL)
        {
	        /* replace call to julda_2 with fcitzc_ and julda_ for y2k */
                FCITZC(&zondum, &dlsdum, validDate->time_zone);        
	        JULDA(&jul_day_valid, &jul_hour_valid,
		       &validDate->month,
		       &validDate->day,
		       &validDate->year,
		       &validDate->hour,
		       &zondum, &dlsdum, validDate->time_zone);
	}
        else    {
	        jul_hour_valid = 0;
	        jul_day_valid = 0;
	        }
	
  

   data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
   memset(data->ModArray[data->ModIndex], '\0', sizeof(ModInfo));
   data->ModArray[data->ModIndex]->b3.type = Mod_format_B3;
   strcpy(data->ModArray[data->ModIndex]->b3.command, data->ModName);

   if(startDate != NULL)
	   data->ModArray[data->ModIndex]->b3.start_date =
			   24*(jul_day_start - 1) + jul_hour_start;
   else    data->ModArray[data->ModIndex]->b3.start_date = 0;

   if(endDate != NULL)
	   data->ModArray[data->ModIndex]->b3.end_date =
			   24*(jul_day_end - 1) + jul_hour_end;
   else    data->ModArray[data->ModIndex]->b3.end_date = 0;
   if(validDate != NULL)
	   data->ModArray[data->ModIndex]->b3.valid_date = 
	                   24*(jul_day_valid - 1) + jul_hour_valid;
   else    data->ModArray[data->ModIndex]->b3.valid_date = 0;
   
if(data->selectedModDef->DataWinType == TIME_SERIES_TYPE)
	{
	switch (modsgroup)
		{
		case FGROUP:
  			currentSegment = (char *)get_fgroup_name();
			break;
		case RANGE:
			strcpy(currentSegment,range_selected.seg_str[range_selected.first]);
			strcat(currentSegment,"-");
			strcat(currentSegment,range_selected.seg_str[range_selected.last]);
			break;
		default:
			break;
		}
	/* now pass in all of data to get the ModIndex correct - dp 6 May 95 */
	/* passing modsgroup and current Segment to fix Mr 1114 4/12/200 */
/*	mp_create_mods(data, modsgroup, currentSegment);*/
/*  Changed void  mp_create_mods(Mods_everythingStruct *,int , char *)           */
/*  to void  mp_create_mods(Mods_everythingStruct *,int , char *, int * , int *) */
/*  to pass startdate and enddate to routine 	                     12/06/02  AV*/
     
	mp_create_mods(data, modsgroup, currentSegment, (int *)data->ModArray[data->ModIndex]->b3.start_date,
                           (int *)data->ModArray[data->ModIndex]->b3.end_date);
             
	}
else    {
	if(startDate != NULL)
	{
	        /* replace call to julda_2 with fcitzc_ and julda_ for y2k */
                FCITZC(&zondum, &dlsdum, startDate->time_zone);        
		JULDA(&jul_day_start, &jul_hour_start,
			&startDate->month,
			&startDate->day,
			&startDate->year,
			&startDate->hour,
			&zondum, &dlsdum, startDate->time_zone);
	} 
	else    {
		jul_hour_start = 0;
		jul_day_start = 0;
		}


	
	if(endDate != NULL)
	{
	        /* replace call to julda_2 with fcitzc_ and julda_ for y2k */
                FCITZC(&zondum, &dlsdum, endDate->time_zone);        
		JULDA(&jul_day_end, &jul_hour_end,
			&endDate->month,
			&endDate->day,
			&endDate->year,
			&endDate->hour,
			&zondum, &dlsdum, endDate->time_zone);
	}
	else    {
		jul_hour_end = 0;
		jul_day_end = 0;
		}


	
        if(validDate != NULL)
        {
	        /* replace call to julda_2 with fcitzc_ and julda_ for y2k */
                FCITZC(&zondum, &dlsdum, validDate->time_zone);        
	        JULDA(&jul_day_valid, &jul_hour_valid,
		       &validDate->month,
		       &validDate->day,
		       &validDate->year,
		       &validDate->hour,
		       &zondum, &dlsdum, validDate->time_zone);
	}
        else    {
	        jul_hour_valid = 0;
	        jul_day_valid = 0;
	        }

   data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
   memset(data->ModArray[data->ModIndex], '\0', sizeof(ModInfo));
   data->ModArray[data->ModIndex]->b3.type = Mod_format_B3;
   strcpy(data->ModArray[data->ModIndex]->b3.command, data->ModName);

   if(startDate != NULL)
	   data->ModArray[data->ModIndex]->b3.start_date =
			   24*(jul_day_start - 1) + jul_hour_start;
   else    data->ModArray[data->ModIndex]->b3.start_date = 0;

   if(endDate != NULL)
	   data->ModArray[data->ModIndex]->b3.end_date =
			   24*(jul_day_end - 1) + jul_hour_end;
   else    data->ModArray[data->ModIndex]->b3.end_date = 0;
   if(validDate != NULL)
	   data->ModArray[data->ModIndex]->b3.valid_date = 
	                   24*(jul_day_valid - 1) + jul_hour_valid;
   else    data->ModArray[data->ModIndex]->b3.valid_date = 0;
  switch (modsgroup){
	case FGROUP:
  		data->ModArray[data->ModIndex]->b3.type_of_id = FGROUP;
  		currentSegment = (char * )get_fgroup_name();
		break;
	case RANGE:
  		data->ModArray[data->ModIndex]->b3.type_of_id = RANGE;
		strcpy(currentSegment,range_selected.seg_str[range_selected.first]);
		strcat(currentSegment,"-");
		strcat(currentSegment,range_selected.seg_str[range_selected.last]);
		break;
	case SEGMENT:
  		data->ModArray[data->ModIndex]->b3.type_of_id = SEGMENT;
		break;
	default:
		break;
	}

   strcpy(data->ModArray[data->ModIndex]->b3.id, currentSegment);


   if(data->selectedModDef->DataWinType == SETQMEAN_TYPE)
	   {
	   XtVaGetValues(data->setQMeanWidgets->setQMeanValuesList, 
			 XmNitems, &xmValues_array,
			 XmNitemCount, &xmItemCount,
			 NULL);
	   
	   data->ModArray[data->ModIndex]->b3.num_values = xmItemCount;
	   
	   /* malloc the space so it isn't reused when             */
	   /*      the function exits...                           */
	   data->ModArray[data->ModIndex]->b3.values =
		   (float *) malloc(sizeof(float) * data->ModArray[data->ModIndex]->b3.num_values);
	   
	   for(i = 1; i <= data->ModArray[data->ModIndex]->b3.num_values; i++)
		   {
		   XmStringGetLtoR(xmValues_array[i - 1], XmSTRING_DEFAULT_CHARSET, &string);
		   data->ModArray[data->ModIndex]->b3.values[i - 1] = atof(string);
		   }
	   }
   else    {
   
  
	   /* All other Mods allow changing only 1 value at a time                 */
	   data->ModArray[data->ModIndex]->b3.num_values = 1;

	   /* malloc the space so it isn't reused when             */
	   /*      the function exits...                           */
	   data->ModArray[data->ModIndex]->b3.values =
		   (float *) malloc(sizeof(float) * data->ModArray[data->ModIndex]->b3.num_values);

	   if(data->selectedModDef->value)
		   {
		   XmScaleGetValue(data->widgetData->modsScale, &scale_value);

		   XtVaGetValues(data->widgetData->modsScale, XmNdecimalPoints, &decimal_points, NULL);

		   for(i = 1; i <= decimal_points; i++) decimal_conversion *= 10;
		   data->ModArray[data->ModIndex]->b3.values[0] = (float) scale_value/decimal_conversion;
		   }
	   }

   strcpy(data->ModArray[data->ModIndex]->b3.keyword, "");
   if(data->opTSdata->num == numberOfSelectedOperations)			/* If all the operations are selected   */
	   data->ModArray[data->ModIndex]->b3.number_of_opers = 0;	/* set '.number_of_opers == 0,          */
   else    {								/* otherwise, fill the name array       */
	   data->ModArray[data->ModIndex]->b3.number_of_opers = numberOfSelectedOperations;

	   selectedWidgetIndex = -1;
	   for(j = 0; j < data->opTSdata->num; j++)
		   {
		   if(operations_number[j] == 1)
			   {
			   selectedWidgetIndex++;
			   strncpy(data->ModArray[data->ModIndex]->b3.opname[selectedWidgetIndex],
				   data->ModSettings->operation.name[j], 8);
			   }
		   }
	   }
 data->ModIndex++;
 }  /* end else not ROCHNG, RRICHNG, UHGCHNG */

}





/* **************************************************************************

	convert_string_to_date()
		converts a string, in the format MMM_DD_YYYY_TT_ZZZZ,
		into a 'date' structure, where:
		MMM:    is a 3-day abbreviation for a month,
		DD:     is the day of the month (no antecedent 0's, eg., "07"),
		YYYY:   is the year,
		TT:     is the time, 0 to 24,
		ZZZZ:   is the time zone code, left justified.

   ************************************************************************** */

void convert_string_to_date(Widget w, date *dateStructure, char *dateString)
{
	char            *month, *day, *year, *time;
	int             j, intMonth, length;
	/* AV changed unsigned char to char due to linux port error 7/16/01 */
	char   *first_blank;


month = (char *)malloc(4);
day = (char *)malloc(3);
year = (char *)malloc(5);
time = (char *)malloc(3);

memset(month, '\0', 4);
memset(day, '\0', 3);
memset(year, '\0', 5);
memset(time, '\0', 3);



first_blank = (unsigned char *)strchr(dateString, ' ');  /* Get the month, then advance the string      */
length = first_blank - dateString;      /*      pointer past the next blank(s)          */
strncpy(month, dateString, length);
for(j = 1; j <= length; j++) dateString++;
dateString++;                           /* Increment past the blank...                  */
if(*dateString == ' ') dateString++;    /* Increment again if need be, because there    */
					/*      may be another blank...                 */

first_blank = (unsigned char *)strchr(dateString, ' ');  /* Get the day...                               */
length = first_blank - dateString;
strncpy(day, dateString, length);
for(j = 1; j <= length; j++) dateString++;
dateString++;           /* Increment past the blank...                  */
if(*dateString == ' ') dateString++;    /* Increment again if need be, because there    */
					/*      may be another blank...                 */

first_blank = (unsigned char *)strchr(dateString, ' ');  /* Get the year...                              */
length = first_blank - dateString;
strncpy(year, dateString, length);
for(j = 1; j <= length; j++) dateString++;
dateString++;           /* Increment past the blank...                  */
if(*dateString == ' ') dateString++;    /* Increment again if need be, because there    */
					/*      may be another blank...                 */

first_blank = (unsigned char *)strchr(dateString, ' ');  /* Get the time...                              */
length = first_blank - dateString;
strncpy(time, dateString, length);
for(j = 1; j <= length; j++) dateString++;
dateString++;                           /* Increment past the blank...                  */

if(strcmp("Jan", month) == 0)      intMonth = 1;
else if(strcmp("Feb", month) == 0) intMonth = 2;
else if(strcmp("Mar", month) == 0) intMonth = 3;
else if(strcmp("Apr", month) == 0) intMonth = 4;
else if(strcmp("May", month) == 0) intMonth = 5;
else if(strcmp("Jun", month) == 0) intMonth = 6;
else if(strcmp("Jul", month) == 0) intMonth = 7;
else if(strcmp("Aug", month) == 0) intMonth = 8;
else if(strcmp("Sep", month) == 0) intMonth = 9;
else if(strcmp("Oct", month) == 0) intMonth = 10;
else if(strcmp("Nov", month) == 0) intMonth = 11;
else if(strcmp("Dec", month) == 0) intMonth = 12;

/*      Fill the date structure                                 */
dateStructure->month = intMonth;
dateStructure->day = atoi(day);
dateStructure->year = atoi(year);
dateStructure->hour = atoi(time);
strcpy(dateStructure->time_zone, dateString);   /* 'dateString' at this point is simply the time-zone   */
						/*      string...                                       */


}



/* **************************************************************************

	getModTSdates()
		converts the text resources of the widgets in a 'display_widgets'
		structure into a 'date' structure...

	modified by gzhou 02/2004
	if not pick up at lease one date then stop Mods create.

   ************************************************************************** */
/*
void getModTSdates(data, deltaT, modDates_array)
*/
int getModTSdates(data, deltaT, modDates_array)
   Mods_everythingStruct    *data;
	int                     deltaT;
	ModDates                modDates_array[];
{
	int             currentJulianTime = 0;
	int             previousJulianTime = 0;
	int             beforePreviousJulianTime = 0;
	int             j, dlsdum, zondum = 1;
	int             numDatesSelected;
	int             jul_day, jul_hour;
	int             modDates_counter, last_j_selected;
	char            *dateString;
	XmString        *xmStringDates;

	Arg             wargs[2];
	Widget          w;
	date            *currentDate;




w = data->widgetData->ifp_modsShell;
currentDate = (date *)malloc(sizeof(date));

XtVaGetValues(data->widgetData->datesValueSList,
	XmNselectedItems, &xmStringDates,
	XmNselectedItemCount, &numDatesSelected,
	NULL);

	
    /*Added by gzhou for bug r21-45 -- 02/24/2004*/
	if(numDatesSelected <= 0)
	{
			PrintWarning ( 1,"Mods_createMod", "Pick up a date before create Mods!\n\tThe Mod create is incomplete!");
			XtManageChild(data->dialogStruct->modDateErrorMessageBox);
			return 0;
	
	}
    /*End of modification by gzhou*/

last_j_selected = SKIP;
modDates_counter = 0;

for(j = 0; j < numDatesSelected; j++)           /* Handle only the selected dates...            */
	{
	XmStringGetLtoR(xmStringDates[j], XmSTRING_DEFAULT_CHARSET, &dateString);


	convert_string_to_date(w, currentDate, dateString);
	
	/* replace call to julda_2 with fcitzc_ and julda_ for y2k */
        FCITZC(&zondum, &dlsdum, currentDate->time_zone);        
	JULDA(&jul_day, &jul_hour,
			&currentDate->month,
			&currentDate->day,
			&currentDate->year,
			&currentDate->hour,
			&zondum, &dlsdum, currentDate->time_zone);

	currentJulianTime = 24*(jul_day - 1) + jul_hour;


	if(last_j_selected != SKIP)     /* Skip past the first selected widget since we don't   */
			{               /* know if it's a SINGLE_DATE or a START_RANGE type     */
			if((currentJulianTime - previousJulianTime) == deltaT)
				{
				if(j == 1)
					{
					modDates_array[modDates_counter].date = previousJulianTime;
					modDates_array[modDates_counter].type_of_date = START_RANGE;
					modDates_counter++;
					}
				else if((previousJulianTime - beforePreviousJulianTime) > deltaT)
					{
					modDates_array[modDates_counter].date = previousJulianTime;
					modDates_array[modDates_counter].type_of_date = START_RANGE;
					modDates_counter++;
					}
				}
			else if((previousJulianTime - beforePreviousJulianTime) == deltaT)
				{
				modDates_array[modDates_counter].date = previousJulianTime;
				modDates_array[modDates_counter].type_of_date = END_RANGE;
				modDates_counter++;
				}
			else    {
				modDates_array[modDates_counter].date = previousJulianTime;
				modDates_array[modDates_counter].type_of_date = SINGLE_DATE;
				modDates_counter++;
				}
		}               /*      End of test to skip past the first selected widget...   */

	last_j_selected = j;
	beforePreviousJulianTime = previousJulianTime;
	previousJulianTime = currentJulianTime;


	}                       /*      End of 'for' loop...                                    */


if((previousJulianTime - beforePreviousJulianTime) == deltaT)
	{
	modDates_array[modDates_counter].date = currentJulianTime;
	modDates_array[modDates_counter].type_of_date = END_RANGE;
	}
else    {
	modDates_array[modDates_counter].date = currentJulianTime;
	modDates_array[modDates_counter].type_of_date = SINGLE_DATE;
	}
	
	return 1;

}



/* *************************************************************************************

	check_date_list()
			checks to see if at least one date is selected.

   ************************************************************************************* */

int check_date_list(Mods_everythingStruct *data)
{

	int             dateCount;



	XtVaGetValues(data->widgetData->datesValueSList, XmNselectedItemCount, dateCount, NULL);
	
/* if(data->selectedModDef->TSDates)	*/

	if(dateCount <= 0) return(NO);
	else return(YES);

}




/* *************************************************************************************

	test_last_two_mods()
		checks to see if the last two Mods_array structures are identical;
		returns:
				TRUE    if they are identical,
				FALSE   otherwise.

   ************************************************************************************* */

static int test_last_two_mods(Mods_everythingStruct *data)
{

	int             last;           
	int             previous;       
	int             theSame = FALSE;
	int             j;
	ModInfo         *lastModArray, *previousModArray;


 
 if(data->ModIndex < 2)
    return(FALSE);

 last = data->ModIndex - 1;
 previous = data->ModIndex - 2;

 lastModArray = (ModInfo *)malloc(sizeof(ModInfo));
 *lastModArray = *data->ModArray[last];


 previousModArray = (ModInfo *)malloc(sizeof(ModInfo));


 for(j = previous; j >= 0; j--)
	{
	*previousModArray = *data->ModArray[j];

	if(lastModArray->type == previousModArray->type)
		{
		switch(lastModArray->type)
			{
			case Mod_format_A1:
				theSame = compare_A1_modArrays(lastModArray, previousModArray);
				break;

			case Mod_format_A2:
				theSame = compare_A2_modArrays(lastModArray, previousModArray);
				break;

			case Mod_format_B1:
				theSame = compare_B1_modArrays(lastModArray, previousModArray);
				break;

			case Mod_format_B2:
				theSame = compare_B2_modArrays(lastModArray, previousModArray);
				break;

			case Mod_format_B3:
				theSame = compare_B3_modArrays(lastModArray, previousModArray);
				break;

			}
		}
	if(theSame) return(theSame);   
	}


return(FALSE);

}



/* *************************************************************************************

	compare_A1_modArrays()
		Compares two Mods_array structures of format type A1, item by item, to
		check whether they are identical or not; returns:

				TRUE    if they are identical,
				FALSE   otherwise.

   ************************************************************************************* */

int compare_A1_modArrays(ModInfo *lastModArray, ModInfo *previousModArray)
{

	int             k;   /* counter */



if(strcmp(lastModArray->a1.command, previousModArray->a1.command) == 0)
	{
	if(lastModArray->a1.start_date != previousModArray->a1.start_date) return(FALSE);
	if(lastModArray->a1.end_date != previousModArray->a1.end_date) return(FALSE);
	if(lastModArray->a1.valid_date != previousModArray->a1.valid_date) return(FALSE);
	if(strcmp(lastModArray->a1.segment_id, previousModArray->a1.segment_id) != 0) return(FALSE);
	if(lastModArray->a1.number_of_ts != previousModArray->a1.number_of_ts) return(FALSE);

	for(k = 0; k < lastModArray->a1.number_of_ts; k++)
		{
		if(lastModArray->a1.info[k]->ts_type != previousModArray->a1.info[k]->ts_type) return(FALSE);
		if(lastModArray->a1.info[k]->delta_t != previousModArray->a1.info[k]->delta_t) return(FALSE);
		if(strcmp(lastModArray->a1.info[k]->ts_id, previousModArray->a1.info[k]->ts_id) != 0) return(FALSE);
		if(strcmp(lastModArray->a1.info[k]->data_type, previousModArray->a1.info[k]->data_type) != 0)
			return(FALSE);

		}

	k = 0;
	while(lastModArray->a1.dates[k].date != 0)
		{
		if(memcmp(&(lastModArray->a1.dates[k]), &(previousModArray->a1.dates[k]), sizeof(ModDates)) != 0)
				return(FALSE);
		else k++;
		if( k == MAX_NUMBER_OF_SELECTABLE_DATES) break;
		}
	}
else return(FALSE);


return(TRUE);

}


/* *************************************************************************************

	compare_A2_modArrays()
		Compares two Mods_array structures of format type A2, item by item, to
		check whether they are identical or not; returns:

				TRUE    if they are identical,
				FALSE   otherwise.

   ************************************************************************************* */

int compare_A2_modArrays(ModInfo *lastModArray, ModInfo *previousModArray)
{

	int             k;



if(strcmp(lastModArray->a2.command, previousModArray->a2.command) == 0)
	{
	if(lastModArray->a2.start_date != previousModArray->a2.start_date) return(FALSE);
	if(lastModArray->a2.end_date != previousModArray->a2.end_date) return(FALSE);
	if(lastModArray->a2.valid_date != previousModArray->a2.valid_date) return(FALSE);
	if(strcmp(lastModArray->a2.segment_id, previousModArray->a2.segment_id) != 0) return(FALSE);
	if(memcmp(lastModArray->a2.info, previousModArray->a2.info, sizeof(TS_INFO)) != 0) return(FALSE);
	if(lastModArray->a2.num_values != previousModArray->a2.num_values) return(FALSE);

	for(k = 0; k < lastModArray->a2.num_values; k++)
		{
		if(lastModArray->a2.values[k] != previousModArray->a2.values[k]) return(FALSE);
		}

	if(strcmp(lastModArray->a2.keyword, previousModArray->a2.keyword) != 0) return(FALSE);
	if(strcmp(lastModArray->a2.optype, previousModArray->a2.optype) != 0) return(FALSE);
	if(strcmp(lastModArray->a2.opname, previousModArray->a2.opname) != 0) return(FALSE);
	}
else return(FALSE);

return(TRUE);

}


/* *************************************************************************************

	compare_B1_modArrays()
		Compares two Mods_array structures of format type B1, item by item, to
		check whether they are identical or not; returns:

				TRUE    if they are identical,
				FALSE   otherwise.

   ************************************************************************************* */

int compare_B1_modArrays(ModInfo *lastModArray, ModInfo *previousModArray)
{

	int             k;



if(strcmp(lastModArray->b1.command, previousModArray->b1.command) == 0)
	{
	if(lastModArray->b1.start_date != previousModArray->b1.start_date) return(FALSE);
	if(lastModArray->b1.end_date != previousModArray->b1.end_date) return(FALSE);
	if(lastModArray->b1.valid_date != previousModArray->b1.valid_date) return(FALSE);
	if(lastModArray->b1.type_of_id != previousModArray->b1.type_of_id) return(FALSE);
	if(strcmp(lastModArray->b1.id, previousModArray->b1.id) != 0) return(FALSE);
	if(strcmp(lastModArray->b1.keyword, previousModArray->b1.keyword) != 0) return(FALSE);

	k = 0;
	while(lastModArray->b1.dates[k].date != 0)
		{
		if(memcmp(&(lastModArray->b1.dates[k]), &(previousModArray->b1.dates[k]), sizeof(ModDates)) != 0)
				return(FALSE);
		else k++;
		if( k == MAX_NUMBER_OF_SELECTABLE_DATES) break;
		}

	if(lastModArray->b1.number_of_opers != previousModArray->b1.number_of_opers) return(FALSE);

	for(k = 0; k < lastModArray->b1.number_of_opers; k++)
		{
		if(strcmp((char *)lastModArray->b1.opname,(char *) previousModArray->b1.opname) != 0) return(FALSE);
		}

	}
else return(FALSE);

return(TRUE);

}




/* *************************************************************************************

	compare_B2_modArrays()
		Compares two Mods_array structures of format type B2, item by item, to
		check whether they are identical or not; returns:

				TRUE    if they are identical,
				FALSE   otherwise.

   ************************************************************************************* */

int compare_B2_modArrays(ModInfo *lastModArray, ModInfo *previousModArray)
{

	int             k;



if(strcmp(lastModArray->b2.command, previousModArray->b2.command) == 0)
	{
	if(lastModArray->b2.start_date != previousModArray->b2.start_date) return(FALSE);
	if(lastModArray->b2.end_date != previousModArray->b2.end_date) return(FALSE);
	if(lastModArray->b2.valid_date != previousModArray->b2.valid_date) return(FALSE);
	if(lastModArray->b2.type_of_id != previousModArray->b2.type_of_id) return(FALSE);
	if(strcmp(lastModArray->b2.id, previousModArray->b2.id) != 0) return(FALSE);

	/*k = 0;*/
	/*while(strcmp(lastModArray->b2.keyword[k], NULL) != 0)--AV */
        /*keyword[9][7] defined in mods_info.h */
        for (k=0;k<8;k++){  /*loop thru max of keyword */
          /*while(strcmp(lastModArray->b2.keyword[k], "") != 0)*/
            if(strcmp(lastModArray->b2.keyword[k], "") == 0) continue;
		/*{*/
            if(strcmp(lastModArray->b2.keyword[k], previousModArray->b2.keyword[k]) != 0) return(FALSE);
            if(lastModArray->b2.values[k] != previousModArray->b2.values[k]) return(FALSE);
		/*k++;
		}*/
        }
	if(lastModArray->b2.number_of_opers != previousModArray->b2.number_of_opers) return(FALSE);

	for(k = 0; k < lastModArray->b2.number_of_opers; k++)
        {
	    if(strcmp((char *)lastModArray->b2.opname, (char *)previousModArray->b2.opname) != 0) return(FALSE);
	}

	}
else return(FALSE);

return(TRUE);

}




/* *************************************************************************************

	compare_B3_modArrays()
		Compares two Mods_array structures of format type B3, item by item, to
		check whether they are identical or not; returns:

				TRUE    if they are identical,
				FALSE   otherwise.

   ************************************************************************************* */

int compare_B3_modArrays(ModInfo *lastModArray, ModInfo *previousModArray)
{

	int             k;



if(strcmp(lastModArray->b3.command, previousModArray->b3.command) == 0)
	{
	if(lastModArray->b3.start_date != previousModArray->b3.start_date) return(FALSE);
	if(lastModArray->b3.end_date != previousModArray->b3.end_date) return(FALSE);
	if(lastModArray->b3.valid_date != previousModArray->b3.valid_date) return(FALSE);
	if(lastModArray->b3.type_of_id != previousModArray->b3.type_of_id) return(FALSE);
	if(strcmp(lastModArray->b3.id, previousModArray->b3.id) != 0) return(FALSE);

	if(lastModArray->b3.num_values != previousModArray->b3.num_values) return(FALSE);

	for(k = 0; k < lastModArray->b3.num_values; k++)
		{
		if(lastModArray->b3.values[k] != previousModArray->b3.values[k]) return(FALSE);
		}

	if(strcmp(lastModArray->b3.keyword, previousModArray->b3.keyword) != 0) return(FALSE);

	if(lastModArray->b3.number_of_opers != previousModArray->b3.number_of_opers) return(FALSE);

	for(k = 0; k < lastModArray->b3.number_of_opers; k++)
		{
		if(strcmp((char *)lastModArray->b3.opname, (char *)previousModArray->b3.opname) != 0) return(FALSE);
		}

	}
else return(FALSE);

return(TRUE);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_fillModArray.c,v $";
 static char rcs_id2[] = "$Id: Mods_fillModArray.c,v 1.14 2005/01/31 17:55:23 aivo Exp $";}
/*  ===================================================  */

}




