/****************************************************************/
/*                                                              */
/*	FILE:		Mods_initModDates.c			*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		10/31/94                                */
/*                                                              */
/*      Modified by:    Donna Page                              */
/*                      27 Oct. 1995                            */
/*                                      			*/
/*								*/
/*                                                              */
/****************************************************************/



#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"


static void initializeStartDateControls(Mods_everythingStruct *);
static void initializeEndDateControls(Mods_everythingStruct *);
static void initializeValidDateControls(Mods_everythingStruct *);

extern date *GetDate(Display *, Atom, Atom);
extern void TextFieldDisplayDate(date *, char *, Widget, Widget, Widget, Widget, Widget);
extern char *ConvertMonthToString(int);

extern void incrementMonth(Widget, dateFieldStruct_p, XmAnyCallbackStruct *);
extern void incrementDay(Widget, dateFieldStruct_p, XmAnyCallbackStruct *);
extern void incrementYear(Widget, dateFieldStruct_p, XmAnyCallbackStruct *);
extern void incrementTime(Widget, dateFieldStruct_p, XmAnyCallbackStruct *);

extern void TextFieldDatesAddCB(Widget, Mods_everythingStruct *, dateFieldStruct_p);

extern void decrementMonth(Widget, dateFieldStruct_p, XmAnyCallbackStruct *);
extern void decrementDay(Widget, dateFieldStruct_p, XmAnyCallbackStruct *);
extern void decrementYear(Widget, dateFieldStruct_p, XmAnyCallbackStruct *);
extern void decrementTime(Widget, dateFieldStruct_p, XmAnyCallbackStruct *);

extern void change_the_hour(int, date *);

/****************************************************************/
/*                                                              */
/*	void initializeModDates()				*/
/*                                                              */
/*	RETURNS:	NONE					*/
/*                                                              */
/****************************************************************/

void initializeModDates(Mods_everythingStruct *data)
{

	data->ModDates = (datesStruct_p)XtMalloc(sizeof(datesStruct_t));
	
	initializeStartDateControls(data);
	initializeEndDateControls(data);
	initializeValidDateControls(data);

}


static void initializeStartDateControls(Mods_everythingStruct *data)
{

	char		*monthString;
	date		*StartDate;
	Display		*display;
	
	dateFieldStruct_t	**dateControl;
	dateWidgetStruct_p	dateWidgets;
	
	int             delta_t;
	
	
	display = XtDisplay(data->widgetData->ifp_modsShell);
	
	StartDate = GetDate(display, IFPA_run_start_date, IFPA_run_start_date_type);
	
	data->ModDates->StartDate = StartDate;
	   
	monthString = ConvertMonthToString(StartDate->month);
	
	/*------------------------------------------------------*/
	/* Put the date & time in the Widgets			*/
	/*------------------------------------------------------*/
	TextFieldDisplayDate(StartDate, monthString,
				data->widgetData->startMonthTextF,
				data->widgetData->startDayTextF,
				data->widgetData->startYearTextF,
				data->widgetData->startTimeTextF,
				data->widgetData->startTZoneLabel);

	
	/*------------------------------------------------------*/
	/* Make space for the Date Widgets structure		*/
	/*------------------------------------------------------*/
	dateWidgets = (dateWidgetStruct_p)XtMalloc(sizeof(dateWidgetStruct_t));
	dateWidgets->month = data->widgetData->startMonthTextF;
	dateWidgets->day   = data->widgetData->startDayTextF;
	dateWidgets->year  = data->widgetData->startYearTextF;
	dateWidgets->time  = data->widgetData->startTimeTextF;
	
	dateControl = (dateFieldStruct_t **)XtMalloc(sizeof(dateFieldStruct_t)*4);
									
	/*--------------------- START MONTH --------------------*/
	dateControl[0] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[0]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[0]->dateStruct		= StartDate;
	dateControl[0]->selected		= data->widgetData->startMonthTextF;
	dateControl[0]->CBFuncs->increment	= incrementMonth;
	dateControl[0]->CBFuncs->decrement	= decrementMonth;
	dateControl[0]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->startMonthTextF, data, dateControl[0]);
	
	
	/*---------------------- START DAY ---------------------*/
	dateControl[1] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[1]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[1]->dateStruct		= StartDate;
	dateControl[1]->selected		= data->widgetData->startDayTextF;
	dateControl[1]->CBFuncs->increment	= incrementDay;
	dateControl[1]->CBFuncs->decrement	= decrementDay;
	dateControl[1]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->startDayTextF, data, dateControl[1]);
	
	
	/*--------------------- START YEAR ---------------------*/
	dateControl[2] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[2]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[2]->dateStruct		= StartDate;
	dateControl[2]->selected		= data->widgetData->startYearTextF;
	dateControl[2]->CBFuncs->increment	= incrementYear;
	dateControl[2]->CBFuncs->decrement	= decrementYear;
	dateControl[2]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->startYearTextF, data, dateControl[2]);
	
	
	/*--------------------- START TIME ---------------------*/
	dateControl[3] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[3]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[3]->dateStruct		= StartDate;
	dateControl[3]->selected		= data->widgetData->startTimeTextF;
	dateControl[3]->CBFuncs->increment	= incrementTime;
	dateControl[3]->CBFuncs->decrement	= decrementTime;
	dateControl[3]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->startTimeTextF, data, dateControl[3]);
	
}


static void initializeEndDateControls(Mods_everythingStruct *data)
{

	char		*monthString;
	date		*EndDate;
	Display		*display;
	
	dateFieldStruct_t	**dateControl;
	dateWidgetStruct_p	dateWidgets;
	
	
	
	display = XtDisplay(data->widgetData->ifp_modsShell);
	
	EndDate = GetDate(display, IFPA_run_end_date, IFPA_run_end_date_type);
	data->ModDates->EndDate = EndDate;
	monthString = ConvertMonthToString(EndDate->month);
	
	/*------------------------------------------------------*/
	/* Put the date & time in the Widgets			*/
	/*------------------------------------------------------*/
	TextFieldDisplayDate(EndDate, monthString,
				data->widgetData->endMonthTextF,
				data->widgetData->endDayTextF,
				data->widgetData->endYearTextF,
				data->widgetData->endTimeTextF,
				data->widgetData->endTZoneLabel);

	
	/*------------------------------------------------------*/
	/* Make space for the Date Widgets structure		*/
	/*------------------------------------------------------*/
	dateWidgets = (dateWidgetStruct_p)XtMalloc(sizeof(dateWidgetStruct_t));
	dateWidgets->month = data->widgetData->endMonthTextF;
	dateWidgets->day   = data->widgetData->endDayTextF;
	dateWidgets->year  = data->widgetData->endYearTextF;
	dateWidgets->time  = data->widgetData->endTimeTextF;
	
	dateControl = (dateFieldStruct_t **)XtMalloc(sizeof(dateFieldStruct_t *)*4);
									
	/*---------------------- END MONTH ---------------------*/
	dateControl[0] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[0]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[0]->dateStruct		= EndDate;
	dateControl[0]->selected		= data->widgetData->endMonthTextF;
	dateControl[0]->CBFuncs->increment	= incrementMonth;
	dateControl[0]->CBFuncs->decrement	= decrementMonth;
	dateControl[0]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->endMonthTextF, data, dateControl[0]);
	
	
	/*----------------------- END DAY ----------------------*/
	dateControl[1] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[1]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[1]->dateStruct		= EndDate;
	dateControl[1]->selected		= data->widgetData->endDayTextF;
	dateControl[1]->CBFuncs->increment	= incrementDay;
	dateControl[1]->CBFuncs->decrement	= decrementDay;
	dateControl[1]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->endDayTextF, data, dateControl[1]);
	
	
	/*---------------------- END YEAR ----------------------*/
	dateControl[2] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[2]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[2]->dateStruct		= EndDate;
	dateControl[2]->selected		= data->widgetData->endYearTextF;
	dateControl[2]->CBFuncs->increment	= incrementYear;
	dateControl[2]->CBFuncs->decrement	= decrementYear;
	dateControl[2]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->endYearTextF, data, dateControl[2]);
	
	
	/*---------------------- END TIME ----------------------*/
	dateControl[3] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[3]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[3]->dateStruct		= EndDate;
	dateControl[3]->selected		= data->widgetData->endTimeTextF;
	dateControl[3]->CBFuncs->increment	= incrementTime;
	dateControl[3]->CBFuncs->decrement	= decrementTime;
	dateControl[3]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->endTimeTextF, data, dateControl[3]);





}


static void initializeValidDateControls(Mods_everythingStruct *data)
{

	char		*monthString;
	date		*ValidDate;
	Display		*display;
		
	dateFieldStruct_t	**dateControl;
	dateWidgetStruct_p	dateWidgets;
			
	/* printf("Inside 'initializeValidDateControls()'.../n"); */
		
	display = XtDisplay(data->widgetData->ifp_modsShell);
	
	ValidDate = GetDate(display, IFPA_run_end_obs_date, IFPA_run_end_obs_date_type);
	data->ModDates->ValidDate = ValidDate;
	monthString = ConvertMonthToString(ValidDate->month);

	/*------------------------------------------------------*/
	/* Put the date & time in the Widgets			*/
	/*------------------------------------------------------*/
	TextFieldDisplayDate(ValidDate, monthString,
				data->widgetData->validMonthTextF,
				data->widgetData->validDayTextF,
				data->widgetData->validYearTextF,
				data->widgetData->validTimeTextF,
				data->widgetData->validTZoneLabel);

	
	/*------------------------------------------------------*/
	/* Make space for the Date Widgets structure		*/
	/*------------------------------------------------------*/
	dateWidgets = (dateWidgetStruct_p)XtMalloc(sizeof(dateWidgetStruct_t));
	dateWidgets->month = data->widgetData->validMonthTextF;
	dateWidgets->day   = data->widgetData->validDayTextF;
	dateWidgets->year  = data->widgetData->validYearTextF;
	dateWidgets->time  = data->widgetData->validTimeTextF;
	
	dateControl = (dateFieldStruct_t **)XtMalloc(sizeof(dateFieldStruct_t *)*4);
									
	/*---------------------- END MONTH ---------------------*/
	dateControl[0] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[0]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[0]->dateStruct		= ValidDate;
	dateControl[0]->selected		= data->widgetData->validMonthTextF;
	dateControl[0]->CBFuncs->increment	= incrementMonth;
	dateControl[0]->CBFuncs->decrement	= decrementMonth;
	dateControl[0]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->validMonthTextF, data, dateControl[0]);
	
	
	/*----------------------- END DAY ----------------------*/
	dateControl[1] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[1]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[1]->dateStruct		= ValidDate;
	dateControl[1]->selected		= data->widgetData->validDayTextF;
	dateControl[1]->CBFuncs->increment	= incrementDay;
	dateControl[1]->CBFuncs->decrement	= decrementDay;
	dateControl[1]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->validDayTextF, data, dateControl[1]);
	
	
	/*---------------------- END YEAR ----------------------*/
	dateControl[2] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[2]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[2]->dateStruct		= ValidDate;
	dateControl[2]->selected		= data->widgetData->validYearTextF;
	dateControl[2]->CBFuncs->increment	= incrementYear;
	dateControl[2]->CBFuncs->decrement	= decrementYear;
	dateControl[2]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->validYearTextF, data, dateControl[2]);
	
	
	/*---------------------- END TIME ----------------------*/
	dateControl[3] = (dateFieldStruct_t *)XtMalloc(sizeof(dateFieldStruct_t));
	dateControl[3]->CBFuncs = (dateCBStruct_p)XtMalloc(sizeof(dateCBStruct_t));
	dateControl[3]->dateStruct		= ValidDate;
	dateControl[3]->selected		= data->widgetData->validTimeTextF;
	dateControl[3]->CBFuncs->increment	= incrementTime;
	dateControl[3]->CBFuncs->decrement	= decrementTime;
	dateControl[3]->dateWidgets	= dateWidgets;
	TextFieldDatesAddCB(data->widgetData->validTimeTextF, data, dateControl[3]);
	


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_initModDates.c,v $";
 static char rcs_id2[] = "$Id: Mods_initModDates.c,v 1.2 2006/04/18 15:27:31 aivo Exp $";}
/*  ===================================================  */

}

