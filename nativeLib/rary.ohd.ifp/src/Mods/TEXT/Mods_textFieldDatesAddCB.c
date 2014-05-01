/************************************************************************/
/*									*/
/*	FILE:		Mods_textFieldDatesAddCB.c			*/
/*									*/
/*									*/
/*	Coded by:	Tom Adams 					*/
/*			NWS * Office of Hydrology * HRL			*/
/*	Date:		11/01/94					*/
/*									*/
/*      Modified by:    D. Page - 1 Nov. 1995				*/
/*			Added new callback for the incrementDate	*/
/*			and decrementDate widgets			*/
/*									*/
/*      NOTE:		Incrementing and Decrementing the date		*/
/*			& time fields using the ArrowButton		*/
/*			widgets depends on which TextField		*/
/*			widget is selected; so, we must add or		*/
/*			remove Callback functions accordingly,		*/
/*			so that the correct date fields get		*/
/*			updated						*/
/*									*/
/************************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern int GetMonthNumbyStr ( char *);
extern int day_in_month ( int , int);
static void freeDateFieldDataCB(Widget, XtPointer, XtPointer);
static void dateSelectionCB(Widget, Mods_everythingStruct *, XtPointer);

extern void set_currentModSavedFalseCB(Widget, Mods_everythingStruct *,  
                                       XmAnyCallbackStruct *); 
				       
				       
extern int days_in_month(int,int);
void adjustDateCB(Widget, XtPointer, XtPointer);
void AdjustADateWidget(int *, Widget, int, int,XtPointer , XtPointer);


/************************************************************************/
/*                                                              	*/
/*	void TextFieldDatesAddCB()					*/
/*                                                              	*/
/*	RETURNS:	NONE						*/
/*                                                              	*/
/************************************************************************/

void TextFieldDatesAddCB(Widget w, Mods_everythingStruct *data, dateFieldStruct_p controlData)
{

	XtVaSetValues(w, XmNuserData, controlData, NULL);
	
	XtAddCallback(w, XmNfocusCallback, (XtCallbackProc)dateSelectionCB, data);
	XtAddCallback(w, XmNdestroyCallback, freeDateFieldDataCB, controlData);

	XtAddCallback(w, XmNlosingFocusCallback, adjustDateCB, data);
}


/************************************************************************/
/*                                                              	*/
/*	void dateSelectionCB()						*/
/*									*/
/*		This callback is called whenever the user selects a	*/
/*		date field (TextField) widget; A lot is going on here,	*/
/*		some fairly subtle:					*/
/*									*/
/*		(1) The function 'TextFieldDatesAddCB()' was used to	*/
/*		    set the XmNuserData resource to pass additional	*/
/*		    data into this Callback function, which is neces-	*/
/*		    sary because					*/
/*		(2) We're setting-up Callback functions for the Arrow	*/
/*		    Button Widgets for incrementing & decrementing the	*/
/*		    selected date field Widget which implies that diff-	*/
/*		    erent incrementing functions are called depending	*/
/*		    on whether the Month, Day, Year, or Time field were	*/
/*		    selected z7 need to be updated, also it's best to	*/
/*		    keep the Widget data separate from the data passed	*/
/*		    to the Callback function for the ArrowButton	*/
/*		    Widgets						*/
/*		(3) We need to remove all Callbacks for the ArrowButton	*/
/*		    Widgets before adding any so we don't confound	*/
/*		    what's to be updated				*/
/*									*/
/*	RETURNS:	NONE						*/
/*									*/
/************************************************************************/
void dateSelectionCB(Widget w, Mods_everythingStruct *data, XtPointer callData)
{

	dateFieldStruct_p	controlData;
	
		
	XtVaGetValues(w, XmNuserData, &controlData, NULL);

	XtRemoveAllCallbacks(data->widgetData->incrementDate, XmNactivateCallback);
	XtRemoveAllCallbacks(data->widgetData->decrementDate, XmNactivateCallback);

	XtAddCallback(data->widgetData->incrementDate,	XmNactivateCallback,
			controlData->CBFuncs->increment, controlData);
	XtAddCallback(data->widgetData->decrementDate,	XmNactivateCallback,
			controlData->CBFuncs->decrement, controlData);
			
        /* Add callback for set_currentModSavedFalseCB if either button activated */
	XtAddCallback(data->widgetData->incrementDate,	XmNactivateCallback,
			(XtCallbackProc)set_currentModSavedFalseCB, data);
	XtAddCallback(data->widgetData->decrementDate,	XmNactivateCallback,
			(XtCallbackProc)set_currentModSavedFalseCB, data);

}



static void freeDateFieldDataCB(Widget w, XtPointer clientdata, XtPointer callData)
{
	dateFieldStruct_p controlData = (dateFieldStruct_p) clientdata ;

	XtFree((char *)controlData->dateStruct);	/* Free space held by the date structure		*/
	XtFree((char *)controlData->CBFuncs);		/* Free space held by the Callback function structure	*/
	XtFree((char *)controlData);			/* Free space held by the date Control structure	*/

}

/***********3-5-03,kwz*************/
/*If w is widget month, and if its value is in Jan to Dec then change the
*data->dateStruct->month value, else do not do anything.
*If w is not widget month, then call AdjustADateWidget to adjust its value.
*/
void adjustDateCB(Widget w, XtPointer clientdata, XtPointer callData)
{
	int NumDay,month;
	dateFieldStruct_p controlData;
	Mods_everythingStruct *Data = (Mods_everythingStruct *)clientdata ;
	
	XtVaGetValues(w, XmNuserData, &controlData, NULL);
	
	if (w==controlData->dateWidgets->month)
	{
		month=GetMonthNumbyStr((char*)XmTextFieldGetString(w));
		
		/*if Date value changed, set the flags*/
		if(month!=controlData->dateStruct->month)
			set_currentModSavedFalseCB(w,clientdata,callData);
			
		/**/
		if(month>0 && month < 13)
			controlData->dateStruct->month= month;
	}
	else if(w==controlData->dateWidgets->day)
	{
		NumDay=days_in_month(controlData->dateStruct->month, controlData->dateStruct->year);
		AdjustADateWidget(&(controlData->dateStruct->day),w,1,NumDay,clientdata,callData);
	}
	else if(w==controlData->dateWidgets->year)
		AdjustADateWidget(&(controlData->dateStruct->year),w,1900,2100,clientdata,callData);
	else /*the HourTextFs*/
		AdjustADateWidget(&(controlData->dateStruct->hour),w,1,24,clientdata,callData);

}
/*************3-5-03,kwz*******************/
/*If the text value in TimeBox is in [lb,ub] then update Time
else do not do anything.
*/
void AdjustADateWidget(int *Time, Widget TimeBox,int lb, int ub, XtPointer clientdata, XtPointer callData)
{
	int stat,newTime;
	char TempStr[10];

	stat=sscanf(XmTextFieldGetString(TimeBox),"%d",&newTime);
	/*if Date value changed, set the flags*/
	if(stat==1 && *Time!=newTime)
		set_currentModSavedFalseCB(TimeBox,clientdata,callData);

	if(stat==1 && newTime >= lb  && newTime <= ub)
		*Time=newTime;/*TimeBox has an valid value, so update Time.*/

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_textFieldDatesAddCB.c,v $";
 static char rcs_id2[] = "$Id: Mods_textFieldDatesAddCB.c,v 1.3 2006/04/18 15:28:40 aivo Exp $";}
/*  ===================================================  */

}
