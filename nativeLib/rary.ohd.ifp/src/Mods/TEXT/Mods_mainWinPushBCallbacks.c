/****************************************************************/
/*								*/
/*	FILE:		Mods_mainWinPushBCallbacks.c		*/
/*								*/
/*	Callback functions for the Mods Main Window Dialog	*/
/*	PushButtons						*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/22/94				*/
/*								*/
/*	Modified by:    D. Page - 4 May 1995, 29 Oct. 1995	*/
/*			7, 10 Nov. 1995 			*/
/*								*/
/****************************************************************/


#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "Mods_info.h"
#include "ifp_atoms.h"
#include "c_call_f/julda.h"
#include "c_call_f/fcitzc.h"


int	 selected_Mods_type;
/*----------------------------------------------*/
/* Externs for 'createSelectionCB()'		*/
/*----------------------------------------------*/
extern int  modIsUnique(Mods_everythingStruct *);
extern void createMod(Mods_everythingStruct *);
extern void update_modTheSameDialog(Mods_everythingStruct *);


/*----------------------------------------------*/
/* Externs for 'closeSelectionCB()'		*/
/*----------------------------------------------*/
extern void update_modNotSavedDialogForClose(Mods_everythingStruct *);
extern void update_modNotSavedDialogAfterClose(Mods_everythingStruct *);
extern void load_new_mods_edit(Mods_everythingStruct *);
extern void load_new_fgroupmods_edit(Mods_everythingStruct *);

extern	int	GetMenuPos(Widget w);
extern  SEL_RANGE range_selected;
extern void ShowErrorDialog(Widget,char *);
extern char *get_timeZone_code(Display *);
extern int days_in_month(int, int);
int isModDatesValid(Mods_everythingStruct *);
extern int GetMonthNumbyStr(char*);
int getMonthbyWidget(Widget);
int getNumbyWidget (Widget, int, int);

void showInvalidDateErrorMsg(Widget,int);

void createSelectionCB(Widget w, Mods_everythingStruct *data, XmPushButtonCallbackStruct *call_data)
{
	Display  *display;
	Window   root;
	int      FGmods_save;
	char      *first_range_mods_str;

	int             type, format, nitems, left;
	long		offset = 0;
	int 	currentRangeModSaved;
	int isValidDate ;

/**do nothing if current mod is dprecip, dsstate**/
	/*if (!(strcasecmp(data->ModName,"DPRECIP") && strcasecmp(data->ModName,"DSACST")))
	{
	    return;
        }*/
	
	
       
	/********************actions**************/
       if (!strcmp(data->selectedModDef->name,"UHGCDATE"))
       {	
            isValidDate=isModDatesValid(data) ;

	    if (isValidDate < 0) /*mod date is invalid.  Stop create mod*/
	    {
	        showInvalidDateErrorMsg(data->widgetData->modsClose,isValidDate);/*pop up the error message if any error*/
	        return ;
	    }
        }

	/*  Cannot give XtDisplay w because the modsCreate widget (w) is not guaranteed
	 *  to be of any particular class.  XtDisplay needs a widget derived from Core -
	 *  it crashes in the next line if you pass it w.  dp - 5 May 95
	 */
        display = XtDisplay(data->widgetData->modsClose);
        root = DefaultRootWindow(display);	

	first_range_mods_str = (char *)malloc(9);
	memset(first_range_mods_str,'\0', 9);
           
	if(modIsUnique(data))
	{
          /* If the mods viewer window is managed - update the list */
          if(XtIsManaged(data->viewerWidgets->viewerForm))
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
	else	 
	{
	   update_modTheSameDialog(data);
	   XtManageChild(data->dialogStruct->modTheSameMessageBox);
        }
        
        /* Whether mod is unique or not, post IFPA_current_mod_saved as TRUE so 
         * user can continue and change currentModChanged to FALSE 
         * - dp - 29, 31 Oct. 1995
         */
        data->currentModChanged = FALSE;
        data->currentModSaved = TRUE;
        
        XChangeProperty(
	      display,
	      DefaultRootWindow(display),
	      IFPA_current_mod_saved,
	      IFPA_current_mod_saved_type,
	      8,
	      PropModeReplace,
	      (unsigned char *)&data->currentModSaved,
	      sizeof(int)
	      );

	selected_Mods_type = 	GetMenuPos(data->widgetData->optionsApplyMenu);
        data->fgroupModsselected = selected_Mods_type;
        
        display = XtDisplay(data->widgetData->optionsApplyMenu);

	FGmods_save = selected_Mods_type;
	if(FGmods_save == 1 || FGmods_save == 2)
	{
	XChangeProperty(
      			display,
      			DefaultRootWindow(display),
      			IFPA_FGmods_save,
      			IFPA_FGmods_save_type,
      			8,
      			PropModeReplace,
      			(unsigned char *)&FGmods_save,
      			sizeof(int)
      			);
        }
	/* copy the first range selected segment */
        strcpy(first_range_mods_str,range_selected.seg_str[range_selected.min_range]);

	if(selected_Mods_type == 2)
        	{
			/* set flag for rangemod if rangemod data had been saved */
			data->rangeModsSaved = 2;
			XChangeProperty(
	      			display,
	      			DefaultRootWindow(display),
	      			IFPA_rangemods_files_updated,
	      			IFPA_rangemods_files_updated_type,
	      			8,
	      			PropModeReplace,
	      			(unsigned char *)first_range_mods_str,
	      			strlen(first_range_mods_str)
	      			);
		}
	/* reset min_range to (Max)last Range index in cex25.c*/

	data->rangeModsSaved = 0;
	currentRangeModSaved = data->rangeModsSaved;
	XChangeProperty(
	display,
	root,
	IFPA_rangemods_saved,
	IFPA_rangemods_saved_type,
	8,
	PropModeReplace,
	(unsigned char *)&currentRangeModSaved,
	sizeof(int)
	);

}


void closeSelectionCB(Widget w, Mods_everythingStruct *data, XmPushButtonCallbackStruct *call_data)
{

	int		quit_Mods;
	Display         *display;
	Window          root;



if(w==data->dialogStruct->modNotSavedMessageBox && 
   isModDatesValid(data) < 0 && data->currentModChanged == TRUE)
{
	ShowErrorDialog (data->widgetData->modsClose,"***invalid mod date***");

	return ;
}
	display = XtDisplay(w);
	root = DefaultRootWindow(display);
	
	if (data->rangeModsSaved == TRUE && data->currentModChanged == TRUE)
        {
	   data->rangeModsSaved = 0;
	   update_modNotSavedDialogForClose(data);
           XtManageChild(data->dialogStruct->modNotSavedMessageBox);
        }
	else if(data->currentModSaved == FALSE && data->currentModChanged == TRUE )
        {
	   update_modNotSavedDialogForClose(data);
           XtManageChild(data->dialogStruct->modNotSavedMessageBox);
        }
        else
        {
           /* if the calling widget was not the Close button it must have
              been a modNotSavedDialog button so add the callbacks
              removed earlier back in
            */
           if(w != data->widgetData->modsClose)
              update_modNotSavedDialogAfterClose(data);
        
	   XtUnmanageChild(data->widgetData->ifp_modsMessageBox);
		
	   quit_Mods = TRUE;

	   XChangeProperty(
	          display,
	          root,
	          IFPA_done_making_mods,
	          IFPA_done_making_mods_type,
	          8,
	          PropModeReplace,
	          (unsigned char *)&quit_Mods,
	          sizeof(int)
	          );
        }
/*----------------------------------------
	XChangeProperty(
	       display,
	       root,
	       IFPA_mods_quit_ok,
	       IFPA_mods_quit_ok_type,
	       8,
	       PropModeReplace,
	       &quit_Mods,
	       sizeof(int)
	       );
------------------------------------------*/
}


void undoSelectionCB(Widget w, Mods_everythingStruct *data, XmPushButtonCallbackStruct *call_data)
{



}


void preferencesSelectionCB(Widget w, Mods_everythingStruct *data, XmPushButtonCallbackStruct *call_data)
{



}

void helpSelectionCB(Widget w, Mods_everythingStruct *data, XmPushButtonCallbackStruct *call_data)
{




}

/************add for uhgchng editable date********/
/*Info: data->widgetData holds all gui information
* and data->ModDates hold start, end and valid dates
*/
int isModDatesValid(Mods_everythingStruct *data)
/*return value: 0=valid, above 0 = warning, below 0 = error
*-1=invalid startMonth, -2=invalid startYear, -3=invalid startDay, -4=invalid startHour,
*-5=invalid endMonth, -6=invalid endYear, -7=invalid endDay, -8=invalid endHour,
*-9=invalid validMonth, -10=invalid validYear, -11=invalid validDay, -12=invalid validHour,
*/
{
	int numDay, isValid;
	int startMonth, startYear, startDay, startTime ;
	int endMonth, endYear, endDay, endTime ;
	int validMonth, validYear, validDay, validTime ;
    date *datePtr;
    int totalSDay,totalEDay,totalRunDay,tmpHr,itz,idsav;
    char		*time_zone_code;
    Display		*display;

	/********end of declaration section**************/
	isValid = 0;/*assume all mod dates are valid*/
    display = XtDisplay(data->widgetData->ifp_modsShell);
    time_zone_code = (char *)get_timeZone_code(display);
    FCITZC(&itz, &idsav, time_zone_code);
    datePtr=data->ModDates->StartDate;
    JULDA(&totalRunDay,&tmpHr,&datePtr->month,&datePtr->day,&datePtr->year,&datePtr->hour,&itz,&idsav,time_zone_code);

	startMonth=getMonthbyWidget(data->widgetData->startMonthTextF);
	startYear=getNumbyWidget(data->widgetData->startYearTextF,1900,2099);
	numDay=days_in_month(startMonth, startYear);
	startDay=getNumbyWidget(data->widgetData->startDayTextF,1,numDay);
	startTime=getNumbyWidget(data->widgetData->startTimeTextF,1,24);
    JULDA(&totalSDay,&tmpHr,&startMonth,&startDay,&startYear,&startTime,&itz,&idsav,time_zone_code);
/*kwz.comment for ncrfc requirement.8/3/04
if(abs(totalSDay-totalRunDay)>10) isValid=-13;*/
	
	endMonth=getMonthbyWidget(data->widgetData->endMonthTextF);
	endYear=getNumbyWidget(data->widgetData->endYearTextF,1900,2099);
	numDay=days_in_month(endMonth, endYear);
	endDay=getNumbyWidget(data->widgetData->endDayTextF,1,numDay);
	endTime=getNumbyWidget(data->widgetData->endTimeTextF,1,24);
    datePtr=data->ModDates->EndDate;
    JULDA(&totalRunDay,&tmpHr,&datePtr->month,&datePtr->day,&datePtr->year,&datePtr->hour,&itz,&idsav,time_zone_code);

    JULDA(&totalEDay,&tmpHr,&endMonth,&endDay,&endYear,&endTime,&itz,&idsav,time_zone_code);
/*kwz.comment for ncrfc requirement.8/3/04
if(abs(totalEDay-totalRunDay)>10) isValid=-14;
else */
    if(totalSDay>totalEDay) isValid=-15;
/*kwz.comment for ncrfc requirement.8/3/04
else if(abs (totalSDay-totalEDay)>30) isValid=-16;*/

	validMonth=getMonthbyWidget(data->widgetData->validMonthTextF);
	validYear=getNumbyWidget(data->widgetData->validYearTextF,1900,2099);
	numDay=days_in_month(validMonth, validYear);
	validDay=getNumbyWidget(data->widgetData->validDayTextF,1,numDay);
	validTime=getNumbyWidget(data->widgetData->validTimeTextF,1,24);

	if (startMonth == -1) isValid = -1;
	else if (startYear== -1) isValid = -2;
	else if (startDay== -1) isValid = -3;
	else if (startTime== -1) isValid = -4;
	
	else if (endMonth == -1) isValid = -5;
	else if (endYear== -1) isValid = -6;
	else if (endDay== -1) isValid = -7;
	else if (endTime== -1) isValid = -8;
	
	else if (validMonth == -1) isValid = -9;
	else if (validYear== -1) isValid = -10;
	else if (validDay== -1) isValid = -11;
	else if (validTime== -1) isValid = -12;

	return isValid;
}/*end of modDatesValid*/

int getMonthbyWidget(Widget monthWidget)
{
	int month ;

	month=GetMonthNumbyStr((char*)XmTextFieldGetString(monthWidget));
	if(month<=0 || month >= 13)
		month=-1 ;

	return month ;
}

int getNumbyWidget (Widget numWidget, int lb, int ub)
{
	int stat, num ;

	stat=sscanf(XmTextFieldGetString(numWidget),"%d",&num);
	
	if (stat!=1 || num < lb  || num > ub )
		num = -1 ;
	
	return num ;
}

void showInvalidDateErrorMsg(Widget w, int errorNum)
/*This subroutine to show a date error message.
*errorNum < 0 is error, errorNum==0 is ok, errorNum > 0 is warning.
*/
{
	switch (errorNum){/*pop up the error message*/
		case -1:
			ShowErrorDialog (w,"***start month is invalid***");
			break ;
		case -2:
			ShowErrorDialog (w,"***start year is invalid***");
			break ;
		case -3:
			ShowErrorDialog (w,"***start day is invalid***");
			break ;
		case -4:
			ShowErrorDialog (w,"***start hour is invalid***");
			break ;
		case -5:
			ShowErrorDialog (w,"***end month is invalid***");
			break ;
		case -6:
			ShowErrorDialog (w,"***end year is invalid***");
			break ;
		case -7:
			ShowErrorDialog (w,"***end day is invalid***");
			break ;
		case -8:
			ShowErrorDialog (w,"***end hour is invalid***");
			break ;
		case -9:
			ShowErrorDialog (w,"***valid month is invalid***");
			break ;
		case -10:
			ShowErrorDialog (w,"***valid year is invalid***");
			break ;
		case -11:
			ShowErrorDialog (w,"***valid day is invalid***");
			break ;
		case -12:
			ShowErrorDialog (w,"***valid hour is invalid***");
			break ;
/*kwz.comment for ncrfc requirement.8/3/04
		case -13:
			ShowErrorDialog (w,"***start date is not in +-10 days of start run date***");
			break ;
		case -14:
			ShowErrorDialog (w,"***end date is not in +-10 days of end run date***");
			break ;*/
		case -15:
			ShowErrorDialog (w,"***start date is later than end date***");
			break ;
/*kwz.comment for ncrfc requirement.8/3/04
		case -16:
			ShowErrorDialog (w,"***start date to end date is more than 30 days***");
			break ;*/
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_mainWinPushBCallbacks.c,v $";
 static char rcs_id2[] = "$Id: Mods_mainWinPushBCallbacks.c,v 1.5 2006/04/18 15:27:51 aivo Exp $";}
/*  ===================================================  */

}
