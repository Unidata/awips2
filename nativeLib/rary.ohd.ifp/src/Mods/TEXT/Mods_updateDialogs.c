/********************************************************************************/
/*										                                        */
/*	Mods_updateDialogs.c							                            */
/*										                                        */
/*										                                        */
/*------------------------------------------------------------------------------*/
/*										                                        */
/*	Coded by:       Tom Adams						                            */
/*	Affiliation:    NOAA/NWS/Office of Hydrology/HRL			                */
/*	Date:           01/31/91						                            */
/*	Revision:	11/15/94	from mods_dialog.c			                        */
/*										                                        */
/*      Modified by:    D. Page - 30 Oct. 1995, 4 Nov. 1995    			        */
/*			7 Nov. 1995						                                    */
/*										                                        */
/********************************************************************************/


#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern void call_setModInterfaceElementsCB(Widget, Mods_everythingStruct *,
                                           XmAnyCallbackStruct);
extern void closeSelectionCB(Widget, Mods_everythingStruct *, 
                             XmPushButtonCallbackStruct *);
extern void call_createSelectionCB(Widget w, Mods_everythingStruct *, 
                                   XmPushButtonCallbackStruct *);
extern void createSelectionCB(Widget w, Mods_everythingStruct *,
                              XmPushButtonCallbackStruct *);                            
                                                         
void updateValueChangeDialogs(modLimitsDef *, dialogWidgetStruct_p);
void update_modNotSavedDialog(Mods_everythingStruct *);
void update_modNotSavedDialogForClose(Mods_everythingStruct *);
void update_modNotSavedDialogAfterClose(Mods_everythingStruct *);

/************************************************************************/
/*									*/
/*	updateValueChangeDialogs()					*/
/*									*/
/*									*/
/************************************************************************/

void updateValueChangeDialogs(modLimitsDef *Limits, dialogWidgetStruct_p dialogStruct)
{

	int		n;
	XmString        xmMessageString;

	char            str_lowerLimit[21], str_upperLimit[21], *string;
	char            *lower_inequality, *upper_inequality;
	char            str_lowerErrorLimit[21];
	char            str_upperErrorLimit[21];




	string = (char *)malloc(100);
	memset(string, '\0', 100);

	lower_inequality = (char *)malloc(100);
	memset(lower_inequality, '\0', 3);

	upper_inequality = (char *)malloc(100);
	memset(upper_inequality, '\0', 3);

	sprintf(str_lowerLimit, "%.1f", Limits->lower_warning_limit);
	sprintf(str_upperLimit, "%.1f", Limits->upper_warning_limit);

	sprintf(str_lowerErrorLimit, "%.1f", Limits->lower_error_limit);
	sprintf(str_upperErrorLimit, "%.1f", Limits->upper_error_limit);

	if(Limits->lower_warning_inclusive == YES) strcpy(lower_inequality, "<=");
	else strcpy(lower_inequality, "<");

	if(Limits->upper_warning_inclusive == YES) strcpy(upper_inequality, "<=");
	else strcpy(upper_inequality, "<");

	strcpy(string, "WARNING; the recommended range for this value is: \n\n");
	strcat(string, str_lowerLimit);
	strcat(string, " ");
	strcat(string, lower_inequality);
	strcat(string, " ");
	strcat(string, "value");
	strcat(string, " ");
	strcat(string, upper_inequality);
	strcat(string, " ");
	strcat(string, str_upperLimit);
	xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

	XtVaSetValues(dialogStruct->valueWarningMessageBox,
			XmNmessageString, xmMessageString, NULL);
	
	XmStringFree(xmMessageString);


	if(Limits->lower_error_inclusive == YES) strcpy(lower_inequality, "<=");
	else strcpy(lower_inequality, "<");

	if(Limits->upper_error_inclusive == YES) strcpy(upper_inequality, "<=");
	else strcpy(upper_inequality, "<");

	strcpy(string, "ERROR; this value's range is limited to: \n\n");
	strcat(string, str_lowerErrorLimit);
	strcat(string, " ");
	strcat(string, lower_inequality);
	strcat(string, " ");
	strcat(string, "value");
	strcat(string, " ");
	strcat(string, upper_inequality);
	strcat(string, " ");
	strcat(string, str_upperErrorLimit);
	xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);


	XtVaSetValues(dialogStruct->valueErrorMessageBox,
			XmNmessageString, xmMessageString, NULL);

	XmStringFree(xmMessageString);

}

/************************************************************************/
/*									*/
/*	update_modNotSavedDialog()					*/
/*									*/
/*									*/
/************************************************************************/

void update_modNotSavedDialog(Mods_everythingStruct *data)
{
	char            string[150];              /* Array holding characters of text */
	XmString        xmMessageString;

   strcpy(string, "None of the data have been saved for the current ");
   strcat(string, data->ModName);
   strcat(string, " run-time mod.\n\n");
   strcat(string, "Do you wish to save data for this mod?");

   xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

   XtVaSetValues(data->dialogStruct->modNotSavedMessageBox,
		 XmNmessageString, xmMessageString, NULL);
}

/************************************************************************/
/*									*/
/*	update_modNotSavedDialogForClose()	                	*/
/*	                                                                */
/*  update the modNotSavedDialog for when called by the Close button    */
/*									*/
/************************************************************************/

void update_modNotSavedDialogForClose(Mods_everythingStruct *data)
{
   XtRemoveCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		    (XtCallbackProc)call_createSelectionCB, data);
   XtRemoveCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		    (XtCallbackProc)call_setModInterfaceElementsCB, data);
   XtRemoveCallback(data->dialogStruct->modNotSavedMessageBox, XmNcancelCallback,
		    (XtCallbackProc)call_setModInterfaceElementsCB, data);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		 (XtCallbackProc)createSelectionCB, data);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		 (XtCallbackProc)closeSelectionCB, data);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNcancelCallback,
		 (XtCallbackProc)closeSelectionCB, data);
}		    
		    
/************************************************************************/
/*									*/
/*	update_modNotSavedDialogAfterClose()	                	*/
/*	                                                                */
/*  update the modNotSavedDialog for when called by either the OK or    */
/*	Cancel buttons in the modNotSavedMessageBox			*/
/************************************************************************/

void update_modNotSavedDialogAfterClose(Mods_everythingStruct *data)
{
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		    (XtCallbackProc)call_createSelectionCB, data);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		    (XtCallbackProc)call_setModInterfaceElementsCB, data);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNcancelCallback,
		    (XtCallbackProc)call_setModInterfaceElementsCB, data);
   XtRemoveCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		    (XtCallbackProc)createSelectionCB, data);
   XtRemoveCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		    (XtCallbackProc)closeSelectionCB, data);
   XtRemoveCallback(data->dialogStruct->modNotSavedMessageBox, XmNcancelCallback,
		    (XtCallbackProc)closeSelectionCB, data);
}		    
 
/************************************************************************/
/*									*/
/*	update_modTheSameDialog()					*/
/*									*/
/*									*/
/************************************************************************/

void update_modTheSameDialog(Mods_everythingStruct *data)
{
	char            string[150];       /* Array holding characters of text */
	XmString        xmMessageString;
	
   strcpy(string, "This ");
   strcat(string, data->ModName);
   strcat(string, " mod and a mod previously saved are identical.\n");
   strcat(string, "The data will not be saved again.");

   xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

   XtVaSetValues(data->dialogStruct->modTheSameMessageBox,
		 XmNmessageString, xmMessageString, NULL);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_updateDialogs.c,v $";
 static char rcs_id2[] = "$Id: Mods_updateDialogs.c,v 1.2 2006/04/18 15:28:45 aivo Exp $";}
/*  ===================================================  */

}
  
