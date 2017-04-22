/****************************************************************/
/*                                                              */
/*	FILE:		Mods_initValidinterfaceElements.c	*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		10/19/94                                */
/*                                                              */
/*      Modified by:    D. Page                                 */
/*                      28 Oct. 1995                            */
/*      NOTE:							*/
/*								*/
/*                                                              */
/****************************************************************/


#include <time.h>
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "Mods_opTSDataStruct.h"
#include "ifp_atoms.h"
#include "c_call_f/fconvt.h" /*--Added by AV --*/
#include "mods_plot.h"
#include "c_call_f/mdyh2.h" /*kwz.UHGCDATE*/
#include "c_call_f/julda.h" /*kwz.UHGCDATE*/
#include "ifp_inc/Date.h" /*kwz.UHGCDATE*/
#include "c_call_f/fcitzc.h"

static char *initializeValueChangeWidget(Mods_everythingStruct *);
static void initializeTSDateList(Mods_everythingStruct *, mod_data *);
static void initializeOpTSList(Mods_everythingStruct *, mod_data *);
static void opTSListDestroyCB(Widget, OpTSTypeStruct_p, XtPointer);
static void initializeOpTSLabel(Widget, Mod_defStruct *);
static void initializeScaleLabel(Widget, char *);
void UhgCDateClickCB(Widget, Mods_everythingStruct *, XmListCallbackStruct*);
extern char		*get_timeZone_code(Display *);
extern void		change_the_hour(int, date *);
extern char		*makeDateString(date *);
extern OpTSTypeStruct_p	makeOpTSList(mod_data *, int);
extern void		initializeOptionsOpMenu(char *, Mods_everythingStruct *);
extern void		updateValueChangeDialogs(modLimitsDef *, dialogWidgetStruct_p);
mod_data		*get_the_data(char *, ofsData_struct *, operMod_struct *, Display *);
extern void handleShowDataCB(Widget, Mods_everythingStruct *, XtPointer);
extern void addValidInterfaceCallbacks(Mods_everythingStruct *);
extern int getNumTimeSteps(date *, date*, int, char*);
extern void get_month_day_year_hour_tzc(int*,int*,int*,int*,int*,int*,int*,int*,int*,char*);
extern void Initdates4Base(int*, int*, int*);
extern char *ConvertMonthToString(int);
extern void TextFieldDisplayDate(date*, char*,
Widget,Widget,Widget,Widget,Widget);
void initModsForUhgcdate(Mods_everythingStruct *data, mod_data *modsData);
void set_currentModSavedFalseCB(Widget, Mods_everythingStruct *,  
                                  XmAnyCallbackStruct *); 
void OpnameSelectionCB(Widget, Mods_everythingStruct *, XmListCallbackStruct *);
void updateUTSCB(Widget w, Mods_everythingStruct *data, 
                  XmListCallbackStruct *call_data);
void    updateOplabel( Mods_everythingStruct *data, char *buttonName);
void    loadDefaultUHG(int *StartDate, int *EndDate, int *ValidDate);



extern int	numUhgMod[20]; /*20 opmax declared in setUhgMod.c*/
extern union    _ModInfo UhgList[20][100],uhgchngMod;/*100 uhg ts,declared in setUhgMod.c*/
extern int      gopidx, UhgFlag;
extern char     baseopBuf[20][9];

int             selopname_indx; /* Global index selection for operation name change. Called in setUhgMod*/
int             uhgOpInitFlag = 0; /*flag to udpate ts plot when operation name change */
XmListCallbackStruct *ListData;


int            uhgFlagToggle = 0,CURIDX;/*used in Mod_showTSplot */
/****************************************************************/
/*                                                              */
/*	void initializeValidInterfaceElements()			*/
/*                                                              */
/*	RETURNS:	NONE					*/
/*                                                              */
/****************************************************************/

void initializeValidInterfaceElements(char *modName, Mods_everythingStruct *data)
{

	char		*unitsString;
	Display		*display;
	int             position;
        
        selopname_indx = 0;
	/* printf("Inside 'initializeValidInterfaceElements()'...\n"); */

	display = XtDisplay(data->widgetData->ifp_modsShell);
	
	data->ModSettings = get_the_data(modName, data->ofsData, data->operModData, display);

	/*-------------	Scale Widget: value change --------------------------*/		/*  1	*/
	
        if(data->selectedModDef->value) {
		unitsString = initializeValueChangeWidget(data);
		initializeScaleLabel(data->widgetData->modsScale, unitsString);
		updateValueChangeDialogs(data->scaleValueLimits, data->dialogStruct);
				
		XtFree(unitsString);
		}
	/*-------------	List Widget: Operation/Time-series List -------------*/		/*  6	*/
	if(data->selectedModDef->OpTimeSeries) {
               
		initializeOpTSLabel(data->widgetData->typeIDLabel, data->selectedModDef);
                
		initializeOpTSList(data, data->ModSettings);
		}

	/*-------------	List Widget: TS Date List ---------------------------*/   /*  7	*/ 
        
        
        if(data->create_flag == 1){
          
           XtDestroyWidget(data->modsPlotData->main_plot_shell);
           
           data->create_flag = 0;
           uhgOpInitFlag     = 0;
           	
        }
	if (!strcmp(data->selectedModDef->name,"UHGCDATE"))
        {	
             uhgOpInitFlag = 0;
             
                      
XtRemoveCallback(data->widgetData->timeSeriesList, XmNsingleSelectionCallback,
  (XtCallbackProc)OpnameSelectionCB, data);
        
        
             initModsForUhgcdate(data, data->ModSettings);
             
/*             XtSetSensitive(data->widgetData->dataEntryToggle, TRUE);*/
        }
	else if(data->selectedModDef->TSDates) 
               initializeTSDateList(data, data->ModSettings);
        
	/*-------------	OptionMenu Widget - Mod Option ----------------------*/		/*  9   */
	if(data->selectedModDef->modOption) initializeOptionsOpMenu(modName, data);
			
	/*-------------	DataEntryToggle Widget - DateWinType ----------------*/         /*      */
        if(data->selectedModDef->DataWinType){          
          if ( data->create_flag == 0) {              
              data->create_flag = 1;
	      handleShowDataCB(data->widgetData->dataEntryToggle, data, NULL);                   
           }
         
        }
	addValidInterfaceCallbacks(data);   

        ListData = (XmListCallbackStruct *)malloc(sizeof(XmListCallbackStruct));
       
	/***For UHGCDATE mod plot, plot 1st UHGCDATE data or default***/
	if (!strcmp(data->selectedModDef->name,"UHGCDATE"))
	{
            
            position = 2;           
            if(numUhgMod[0]>=3)position=3;        
            XmListSelectPos(data->widgetData->datesValueSList, position, TRUE);  
  	    ListData->item_position = position;
	    UhgCDateClickCB(data->widgetData->datesValueSList,data,ListData);
            uhgFlagToggle = 1;        
	}
  }



/****************************************************************************************/
/*											*/
/*	static void initializeValueChangeWidget()					*/
/*											*/
/*	When we move the scale, we don't want to allow an error message to be generated	*/
/*      if either the upper or lower bound of the range is not included; so, subtract	*/
/*      or add an amount equal to the smallest the scale widget will increment from	*/
/*      'the scaleMaximum' & 'scaleMinimum'...						*/
/*                                                                                      */
/*	'upper_error_inclusive' == 1 (YES) if the upper bound is included, & == 0,	*/
/*		otherwise								*/
/*	'lower_error_inclusive' == 1 (YES) if the lower bound is included, & == 0,	*/
/*		otherwise								*/
/*											*/
/****************************************************************************************/

static char *initializeValueChangeWidget(Mods_everythingStruct *data)
{

	char            english_units[4];
	char            *units_string;		/* For displaying the units			*/
	char		string[50];

	int             i;
	int             errorFlag;
	int             initialScaleValue;
	int             scaleMinimum;
	int             scaleMaximum;
	int             scaleDecimalPoints = 1;
	int             scaleFactor = 1;
	int             units_test;		/* Tests units for Metric System or US System	*/
						/* 	1 => Metric, 0 => US System		*/
	float           multFactor;     	/* Multiplication factor for conversion		*/
	float           addConstant;
	float		displayValue;
	Display		*display;


        
	display = XtDisplay(data->widgetData->ifp_modsShell);
	units_string = (char *)XtMalloc(sizeof(char)*20);

	switch(data->modValueLimits->units){
	
		case DIMENSIONLESS:
		
			initialScaleValue = 100;
			strcpy(units_string, "Dimensionless");
			scaleDecimalPoints = 2;
			multFactor = 1;
			addConstant = 0;
			break;

		case LENGTH_UNITS:
		
			if(data->units->General_Units)
				{       /* Units are METRIC...  */
				initialScaleValue = 254;                 /* For MILLIMETERS...	*/
				strcpy(units_string, "Millimeters");
				multFactor = 1;
				addConstant = 0;
				}
			else    {       /* Units are ENGLISH... */
				initialScaleValue = 10;                 /* For INCHES...	*/
				strcpy(units_string, "Inches");
				FCONVT("MM  ", "L   ", english_units, &multFactor, &addConstant, &errorFlag);
				}
			break;

		case FLOW_UNITS:
		
			if(data->units->General_Units)
				{       /* Units are METRIC...	*/
				strcpy(units_string, "CMS");
				multFactor = 1;
				addConstant = 0;
				}
			else    {       /* Units are ENGLISH...	*/
				strcpy(units_string, "CFS");
				FCONVT("CMS ", "L3/T", english_units, &multFactor, &addConstant, &errorFlag);
				}

			initialScaleValue = 10;
			break;
		
		case API_UNITS:
		
			if(data->units->API_Units)
				{       /* Units are METRIC...  */
				initialScaleValue = 254;                 /* For MILLIMETERS...	*/
				strcpy(units_string, "Millimeters");
				multFactor = 1;
				addConstant = 0;
				}
			else    {       /* Units are ENGLISH... */
				initialScaleValue = 10;                 /* For INCHES...	*/
				strcpy(units_string, "Inches");
				FCONVT("MM  ", "L   ", english_units, &multFactor, &addConstant, &errorFlag);
				}
			break;

		case TIME_PERIODS:
		
			initialScaleValue = 1;
			strcpy(units_string, "Time Periods");
			scaleDecimalPoints = 0;
			multFactor = 1;
			addConstant = 0;
			break;
			
		default:
		
			initialScaleValue = 100;
			strcpy(units_string, "Dimensionless");
			scaleDecimalPoints = 2;
			multFactor = 1;
			addConstant = 0;
			break;
		}


	for(i = 0; i < scaleDecimalPoints; i++) scaleFactor *= 10;

	scaleMaximum = data->modValueLimits->upper_error_limit * scaleFactor
			- (1 - data->modValueLimits->upper_error_inclusive);
	scaleMaximum = scaleMaximum * multFactor + addConstant;
        if(initialScaleValue > scaleMaximum) initialScaleValue = scaleMaximum;

	scaleMinimum = data->modValueLimits->lower_error_limit * scaleFactor
			+ (1 - data->modValueLimits->lower_error_inclusive);
	scaleMinimum = scaleMinimum * multFactor + addConstant;
	
	if(initialScaleValue < scaleMinimum) initialScaleValue = scaleMinimum;
	if(initialScaleValue > scaleMaximum) initialScaleValue = scaleMaximum;
       

	data->scaleValueLimits->lower_warning_limit =
			data->modValueLimits->lower_warning_limit * multFactor + addConstant;
	data->scaleValueLimits->upper_warning_limit =
			data->modValueLimits->upper_warning_limit * multFactor + addConstant;
	data->scaleValueLimits->lower_error_limit =
			data->modValueLimits->lower_error_limit   * multFactor + addConstant;
	data->scaleValueLimits->upper_error_limit =
			data->modValueLimits->upper_error_limit   * multFactor + addConstant;

	data->scaleValueLimits->lower_warning_inclusive = data->modValueLimits->lower_warning_inclusive;
	data->scaleValueLimits->upper_warning_inclusive = data->modValueLimits->upper_warning_inclusive;
	data->scaleValueLimits->lower_error_inclusive   = data->modValueLimits->lower_error_inclusive;
	data->scaleValueLimits->upper_error_inclusive   = data->modValueLimits->upper_error_inclusive;



	XtVaSetValues(data->widgetData->modsScale,
			XmNvalue,		initialScaleValue,
			XmNmaximum,		scaleMaximum,
			XmNminimum,		scaleMinimum,
			XmNprocessingDirection,	XmMAX_ON_RIGHT,
			XmNdecimalPoints,	scaleDecimalPoints,
			XmNshowValue,		FALSE,
			NULL);
			
			
	/*--------------------------------------------------------------*/
	/* We might as well initialize the TextField Widget here too!	*/
	/*--------------------------------------------------------------*/
	displayValue = (float)initialScaleValue/(float)scaleFactor;
	if(data->modValueLimits->units != TIME_PERIODS)	
	   sprintf(string, "%f", displayValue);
	else
	   sprintf(string, "%d", (int)displayValue);
	XmTextFieldSetString(data->widgetData->valueDisplayEntry, string);
	
	/* Reset the warning and error message flags */
	data->flags->errorMessageDisplayed = NO;
	data->flags->warningPoppedUp = NO;
	data->flags->haveBeenWarned = NO;
			
	return(units_string);			
}




/* static */ 
void initializeTSDateList(Mods_everythingStruct *data, mod_data *modsData)
{

	int		deltaT;
	int		k;
	int		steps;
	char		DateString[25];
	char		*time_zone_code;
	char		*string;
	Display		*display;
	XmString	xmDateString[MAX_RUN_PERIOD];
	date		nextDate;
	
	

	display = XtDisplay(data->widgetData->ifp_modsShell);
	
	/*------------------------------------------------------*/
	/* Clear all the items in the list; right now this is	*/
	/* unnecessary but maybe desired in the future, so	*/
	/* we'll do it now...					*/
	/*------------------------------------------------------*/
	XmListDeleteAllItems(data->widgetData->datesValueSList); 

	deltaT = modsData->time_series.delta_t[data->flags->OperationSelected];
	if(deltaT <= 0) deltaT = 6;

	nextDate = *data->ModDates->StartDate;
	
 	time_zone_code = get_timeZone_code(display);

        change_the_hour(deltaT, &nextDate);
	steps = getNumTimeSteps(&nextDate, data->ModDates->EndDate,
				deltaT, time_zone_code);
    
	free(time_zone_code);
	

	for(k = 0; k <= steps; k++){
		string = makeDateString(&nextDate);
		xmDateString[k] = (XmString) XmStringCreateSimple(string);
		free(string);
                change_the_hour(deltaT, &nextDate);


		if(k >= MAX_RUN_PERIOD) break;          /* MAX_RUN_PERIOD was set to 744	*/
		}


	XtVaSetValues(data->widgetData->datesValueSList, 
			XmNitems,		xmDateString,
			XmNitemCount,		k,
			XmNvisibleItemCount,	10,
			NULL);

	if(steps < MAX_RUN_PERIOD) for(k = 1; k <= steps; k++) XmStringFree(xmDateString[k]);
	else for(k = 1; k < MAX_RUN_PERIOD; k++) XmStringFree(xmDateString[k]);

}
/*****Procedures for UHGCDATE begin*************/
void UhgCDateClickCB(Widget w, Mods_everythingStruct *data, 
                               XmListCallbackStruct *call_data)
                              
{

	int        selectedIndex;
	date       dateStruct;
	char       *monthString;
	int        inDate,inHr,itz,idsav,i,val,j;
	char	   *time_zone_code;
	Display	   *display;
	float      max_y,y_axis_max,mp_y_increment;
        char       buttonName[10];
        
	if (strcmp(data->selectedModDef->name,"UHGCDATE"))return;
             
        selectedIndex = call_data->item_position-1; /*because Position start at 1.*/
                         
	display = XtDisplay(data->widgetData->ifp_modsShell);
 	time_zone_code = get_timeZone_code(display);
	strcpy (dateStruct.time_zone,time_zone_code);
	
        /***Now take care the mod date gui***/
	/*reset the dates*/
	get_month_day_year_hour_tzc((int*)UhgList[selopname_indx][selectedIndex].b3.start_date,&inDate,
		&inHr,&dateStruct.month,&dateStruct.day,&dateStruct.year,
		&dateStruct.hour,&itz,&idsav,time_zone_code);
 	monthString=(char *)ConvertMonthToString(dateStruct.month);

       /*------------------------------------------------------*/
       /* Put the date & time in the Widgets			*/
       /*------------------------------------------------------*/
        TextFieldDisplayDate(&dateStruct, monthString,
                        data->widgetData->startMonthTextF,
                        data->widgetData->startDayTextF,
                        data->widgetData->startYearTextF,
                        data->widgetData->startTimeTextF,
                        data->widgetData->startTZoneLabel);

	/*av deb printf("selopname_indx=%d  ==%s\n",selopname_indx,baseopBuf[selopname_indx]);*/
	get_month_day_year_hour_tzc((int*)UhgList[selopname_indx][selectedIndex].b3.end_date,&inDate,
		&inHr,&dateStruct.month,&dateStruct.day,&dateStruct.year,
		&dateStruct.hour,&itz,&idsav,time_zone_code);
	monthString = (char *)ConvertMonthToString(dateStruct.month);
       /*------------------------------------------------------*/
       /* Put the date & time in the Widgets	           */
       /*------------------------------------------------------*/
       TextFieldDisplayDate(&dateStruct, monthString,
                        data->widgetData->endMonthTextF,
                        data->widgetData->endDayTextF,
                        data->widgetData->endYearTextF,
                        data->widgetData->endTimeTextF,
                        data->widgetData->endTZoneLabel);

      
	get_month_day_year_hour_tzc((int*)UhgList[selopname_indx][selectedIndex].b3.valid_date,&inDate,
		&inHr,&dateStruct.month,&dateStruct.day,&dateStruct.year,
		&dateStruct.hour,&itz,&idsav,time_zone_code);
	monthString = (char *)ConvertMonthToString(dateStruct.month);
       /*------------------------------------------------------*/
       /* Put the date & time in the Widgets		   */
       /*------------------------------------------------------*/
        TextFieldDisplayDate(&dateStruct, monthString,
                        data->widgetData->validMonthTextF,
                        data->widgetData->validDayTextF,
                        data->widgetData->validYearTextF,
                        data->widgetData->validTimeTextF,
                        data->widgetData->validTZoneLabel);
       
         strncpy(UhgList[selopname_indx][selectedIndex].b3.opname[selopname_indx],baseopBuf[selopname_indx],8);
       
         
	/***Now take care the mod plot gui***/
        
   
        max_y = 0.0;
        
           
	for (i=0;i<UhgList[selopname_indx][selectedIndex].b3.num_values;i++)
	{
             
             data->modsPlotData->ts_array[0][i]=UhgList[selopname_indx][selectedIndex].b3.values[i] ; 
             data->modsPlotData->orig_ts_array[0][i]=UhgList[selopname_indx][selectedIndex].b3.values[i] ;
 
	     if (max_y<UhgList[selopname_indx][selectedIndex].b3.values[i])
			max_y = UhgList[selopname_indx][selectedIndex].b3.values[i] ;
                
	}
                
        /* Handle UHGDATE display when when main_plot_shell is destroyed */ 
        /* ------------------------------------------------------------- */ 
             
        
        if (data->create_flag == 0)
        {          
            if(data->selectedModDef->DataWinType){
              data->create_flag = 1; 
              handleShowDataCB(data->widgetData->dataEntryToggle, data, NULL);
            }
            for (i=0;i<UhgList[selopname_indx][selectedIndex].b3.num_values;i++)
	    {
		data->modsPlotData->ts_array[0][i]=UhgList[selopname_indx][selectedIndex].b3.values[i] ; 
                data->modsPlotData->orig_ts_array[0][i]=UhgList[selopname_indx][selectedIndex].b3.values[i] ;
	    }
            
        }   
     
       if(max_y!=data->modsPlotData->default_y_axis_max)
        {
           XmScaleGetValue(data->modsPlotData->change_y_axis_max_widget,&val);
           
           scale_max_min(data->modsPlotData->min_y, max_y, 0.0,
		 &data->modsPlotData->min_y, &y_axis_max, &mp_y_increment);
          
           data->modsPlotData->max_y = max_y;
           data->modsPlotData->y_axis_max =  y_axis_max*val;
           data->modsPlotData->default_y_axis_max = y_axis_max;
           data->modsPlotData->mp_y_increment = mp_y_increment;

           resize_mp_y_axis(data->modsPlotData->drawing_area_widget[1], 
                  data->modsPlotData, NULL);
        
        }
       
        /*refresh data for UHGDATE display*/   
        resize_mp_graph(data->modsPlotData->drawing_area_widget[2],
                                                   data->modsPlotData, NULL);
                                                   
        strcpy (buttonName, UhgList[selopname_indx][selectedIndex].b3.opname[selopname_indx]);                                           
        updateOplabel(data, buttonName); 
       
}       
/*********/
void UhgCDateDestroyCB(Widget w, Mods_everythingStruct *data, 
                               XmListCallbackStruct *call_data)
{
/*destroy all call backs on data->widgetData->datesValueSList*/
	XtRemoveAllCallbacks(data->widgetData->datesValueSList, XmNdefaultActionCallback);
}
/********/


void initModsForUhgcdate(Mods_everythingStruct *data, mod_data *modsData)
{
        
	int		i,j,k,l,curPos;
	char		DateString[35],startTime[20],endTime[20],validTime[20];
	char		*string;
	XmString	xmDateString[MAX_RUN_PERIOD];
	
	int inDate,inHr,month,day,year,hour,itz,idsav;
	int StartDate,EndDate,ValidDate;
	date *tmpDatePtr;

	char		*time_zone_code;
	Display		*display;
        static int      first = 1;
             
        
	display = XtDisplay(data->widgetData->ifp_modsShell);
 	time_zone_code = get_timeZone_code(display);
	FCITZC(&itz, &idsav, time_zone_code);
        
	/*------------------------------------------------------*/
	/* Clear all the items in the list; right now this is	*/
	/* unnecessary but maybe desired in the future, so	*/
	/* we'll do it now...					*/
	/*------------------------------------------------------*/
	XmListDeleteAllItems(data->widgetData->datesValueSList); 
  
	tmpDatePtr=data->ModDates->StartDate;
	JULDA(&inDate,&inHr,&tmpDatePtr->month,&tmpDatePtr->day,&tmpDatePtr->year,
		&tmpDatePtr->hour,&itz,&idsav,tmpDatePtr->time_zone);
	StartDate=(inDate-1)*24+inHr;/*-1? see get_MDYH.c*/

	tmpDatePtr=data->ModDates->EndDate;
	JULDA(&inDate,&inHr,&tmpDatePtr->month,&tmpDatePtr->day,&tmpDatePtr->year,
		&tmpDatePtr->hour,&itz,&idsav,tmpDatePtr->time_zone);
	EndDate=(inDate-1)*24+inHr;

	tmpDatePtr=data->ModDates->ValidDate;
	JULDA(&inDate,&inHr,&tmpDatePtr->month,&tmpDatePtr->day,&tmpDatePtr->year,
		&tmpDatePtr->hour,&itz,&idsav,tmpDatePtr->time_zone);
	ValidDate=(inDate-1)*24+inHr;

	/* Initialize dates for base UHG */
        Initdates4Base(&StartDate,&EndDate,&ValidDate);
        /* Initialize dates for default UHG */
        loadDefaultUHG(&StartDate,&EndDate,&ValidDate);
	k=0;
	l= selopname_indx;
         
	for (i=0;i<numUhgMod[l];i++)
	{
            
		if (!strcmp(UhgList[l][i].b3.command,"BASE"))
		{	xmDateString[i] = (XmString) XmStringCreateSimple("BASE");
			k++;
		}
		else if (!strcmp(UhgList[l][i].b3.command,"DEFAULT"))
		{	xmDateString[i] = (XmString) XmStringCreateSimple("DEFAULT");
			k++;
		}
		else if (!strcmp(UhgList[l][i].b3.command,"UHGCDATE"))
		{
			get_month_day_year_hour_tzc((int*)UhgList[l][i].b3.start_date,&inDate,&inHr,
				&month,&day,&year,&hour,&itz,&idsav,tmpDatePtr->time_zone);
			year=year%100;/*Only need the year, no century*/
			sprintf(startTime,"%2d%2d%2d%2dZ ",month,day,year,hour);

			get_month_day_year_hour_tzc((int*)UhgList[l][i].b3.end_date,&inDate,&inHr,
				&month,&day,&year,&hour,&itz,&idsav,tmpDatePtr->time_zone);
                                
			year=year%100;/*Only need the year, no century*/
			sprintf(endTime,"%2d%2d%2d%2dZ ",month,day,year,hour);

			get_month_day_year_hour_tzc((int*)UhgList[l][i].b3.valid_date,&inDate,&inHr,
				&month,&day,&year,&hour,&itz,&idsav,tmpDatePtr->time_zone);
			year=year%100;/*Only need the year, no century*/
			sprintf(validTime,"%2d%2d%2d%2dZ",month,day,year,hour);
			strcpy (DateString,startTime);
			strcat (DateString,endTime);
			strcat (DateString,validTime);
			for (j=0;j<29;j++)
				if (DateString[j]==' ')	DateString[j]='0' ;

			DateString[9]=' ';/*Because they replaced by '0' in last loop.*/
			DateString[19]=' ';

			xmDateString[k] = (XmString) XmStringCreateSimple(DateString) ;
                       
			k++;
		}
	}
        
	XtVaSetValues(data->widgetData->datesValueSList, 
		XmNitems,		xmDateString,
		XmNitemCount,		k,
		XmNvisibleItemCount,	10,
		XmNselectionPolicy, XmSINGLE_SELECT,
		NULL);
                
        /*XmListSelectPos(data->widgetData->datesValueSList,2,TRUE);*/
        
	for(i = 0; i < k; i++)
		XmStringFree(xmDateString[i]);
        
        curPos = k; 
        if(k>=3)curPos=3; 
        
          
        /* This line may cause problems - Don't know why */
        XmListSelectPos(data->widgetData->datesValueSList, curPos, TRUE);
 
        XtAddCallback(data->widgetData->timeSeriesList, (String)XmNsingleSelectionCallback,
		       (XtCallbackProc)OpnameSelectionCB, data);
                       
       
        XtAddCallback(data->widgetData->datesValueSList,
		(String)XmNsingleSelectionCallback, (XtCallbackProc)updateUTSCB, data);                 

         if ( uhgOpInitFlag ) {             
            if (data->create_flag == 1) {            
            
                XtDestroyWidget(data->modsPlotData->main_plot_shell);
               
            }
            handleShowDataCB(data->widgetData->dataEntryToggle, data, NULL);
                  data->create_flag = 1;
           
            addValidInterfaceCallbacks(data);  
             
            ListData->item_position = curPos;      
            UhgCDateClickCB(data->widgetData->datesValueSList, data, ListData);
           
         }uhgOpInitFlag = 1;

         
}

/*****Procedures for UHGCDATE end*************/

static void initializeOpTSLabel(Widget w, Mod_defStruct *data)
{

	char	*OpString = "Type / Name";
	char	*TSString = "ID / Data Type / Delta T";
	char	string[30];
	
	
	if(data->OpTSType) strcpy(string, TSString);
	else strcpy(string, OpString);


	XtVaSetValues(w, XtVaTypedArg,
			XmNlabelString, XmRString, string,
			strlen(string)+1, NULL);

}


static void initializeOpTSList(Mods_everythingStruct *data, mod_data *modsData)
{

	int		i;
	XmString	*xmStringList;
	

	/*----------------------------------------------------------------------*/
	/* Reset the arrays to garauntee that we correctly set the flags	*/
	/*----------------------------------------------------------------------*/
	
	for(i = 0; i < MAX_OPERATIONS; i++) {
		operations_number[i] = FALSE;
		data->flags->opNumber[i] = FALSE;
		}
		
		
	data->opTSdata = makeOpTSList(modsData, data->selectedModDef->OpTSType);

        
	XtVaSetValues(data->widgetData->timeSeriesList, 
			XmNitems,		data->opTSdata->xmStringList,
			XmNitemCount,		data->opTSdata->num,
			XmNvisibleItemCount,	5,
			NULL);
                        
	/*--------------------------------------------------------------*/
	/* Clean-up and select all the items, which is the default...	*/
	/* we have to Deselect all the items before we can set them all	*/
	/* selected!							*/
	/*--------------------------------------------------------------*/
	if(data->selectedModDef->OpTSSelectType) {	/* MULTIPLE_SELECT_TYPE...	*/

		XtVaSetValues(data->widgetData->timeSeriesList, 
				XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
	
		XmListDeselectAllItems(data->widgetData->timeSeriesList);
		
		for(i = 0; i < data->opTSdata->num; i++) {
			XmListSelectPos(data->widgetData->timeSeriesList, i+1, FALSE);
			XmStringFree(data->opTSdata->xmStringList[i]);
			
			operations_number[i] = TRUE;
			data->flags->opNumber[i] = TRUE;
		}
	}
        else	{					/* SINGLE_SELECT_TYPE...	*/
	
		XtVaSetValues(data->widgetData->timeSeriesList, 
				XmNselectionPolicy, XmSINGLE_SELECT, NULL);

		/* AI This line may cause problem */
                XmListSelectPos(data->widgetData->timeSeriesList, 1, TRUE);
		
		operations_number[0] = TRUE;
		data->flags->opNumber[0] = TRUE;
               
		}
	
       
	XtAddCallback(data->widgetData->timeSeriesList, (String)XmNdestroyCallback, 
	              (XtCallbackProc)opTSListDestroyCB, data->opTSdata);
}



static void opTSListDestroyCB(Widget w, OpTSTypeStruct_p opTSdata, XtPointer callData)
{

	XtFree((char *)opTSdata);
	
}


static void initializeScaleLabel(Widget w, char *string)
{

	XtVaSetValues(w, XtVaTypedArg,
			XmNtitleString, XmRString, string,
			strlen(string)+1, NULL);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_initValidInterfaceElements.c,v $";
 static char rcs_id2[] = "$Id: Mods_initValidInterfaceElements.c,v 1.15 2006/04/18 15:27:42 aivo Exp $";}
/*  ===================================================  */

}

void OpnameSelectionCB(Widget w, Mods_everythingStruct *data, 
                  XmListCallbackStruct *call_data)
{
     int position;
  
    /* global variable to return the opname selection index */
    /* This selopname_indx is for current selected operation name */
    /* e.g. ROSSE or ROSSEB..  */
     selopname_indx = call_data->item_position-1; 
                      
     XtRemoveCallback(data->widgetData->timeSeriesList, XmNsingleSelectionCallback,
		       (XtCallbackProc)OpnameSelectionCB, data);
     
     
     XtRemoveCallback(data->widgetData->datesValueSList,
		XmNsingleSelectionCallback, (XtCallbackProc)updateUTSCB, data);
                
     
     /*Show new list */   
     initModsForUhgcdate(data, data->ModSettings);
     
}
/*****************************************************/
/*update UHG timeseries based on the                 */
/*operation name                                     */            
/*****************************************************/
void updateUTSCB(Widget w, Mods_everythingStruct *data, 
                  XmListCallbackStruct *call_data)
{
   
   if (strcmp(data->selectedModDef->name,"UHGCDATE") !=0)return;
    /* Current TimeSeries Selected Index 1-N - must be offset by 1*/
    ListData->item_position=call_data->item_position;
    CURIDX = ListData->item_position;    
    UhgCDateClickCB(data->widgetData->datesValueSList,data,ListData);
    printf("Call UHGDATE gui \n");
   
}

/*********/

void    updateOplabel( Mods_everythingStruct *data, char *buttonName)
{

   Arg       wargs[10];  
   char      bname[20];
   XmString  xmstr;            
                  
                      
   int       n=0;
   
    strcpy(data->modsPlotData->op_name[0], buttonName);
    strcpy(bname,buttonName);
    xmstr=XmStringCreateLtoR(bname,XmSTRING_DEFAULT_CHARSET);

    XtVaSetValues(data->modsPlotData->legend[0],XmNlabelString,xmstr,NULL);  
    XmStringFree (xmstr);
                                         
}
/*********/


/*Load latest UHGCHNG mod data to default index*/
/* Set mod start, end and valid dates as dates of the run */
void loadDefaultUHG(int *StartDate, int *EndDate, int *ValidDate)
{
int i,j;
      
   j = selopname_indx; /*selopname_indx defines in Mods_initInterfaceElements */
   if (strcmp(UhgList[j][1].b3.command,"DEFAULT")!=0)
   {
        /* find index from opname */
        UhgList[j][1].b3.num_values=UhgList[j][0].b3.num_values;
        UhgList[j][1].b3.values=(float*) malloc (sizeof(float) *UhgList[j][0].b3.num_values);
	
		for (i=0;i<UhgList[j][1].b3.num_values;i++)
			UhgList[j][1].b3.values[i]=UhgList[j][0].b3.values[i];
            
        strcpy(UhgList[j][1].b3.command,"DEFAULT");
        strcpy(UhgList[j][1].b3.id,"DEFAULT");
    }
        
    /*set start dates for default index (this is the latest UHGCHNGMOD)*/
	UhgList[j][1].b3.start_date = *StartDate;
	UhgList[j][1].b3.end_date   = *EndDate;
	UhgList[j][1].b3.valid_date = *ValidDate;
}
