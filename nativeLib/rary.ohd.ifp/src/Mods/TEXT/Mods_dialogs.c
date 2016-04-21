/********************************************************************************/
/*										                                        */
/*	Mods_dialogs.c								                                */
/*										                                        */
/*	This file contains all the functions that create:			                */
/*										                                        */
/*			Information,						*/
/*			Warning, and						*/
/*			Error							*/
/*										*/
/*	popup dialog windows...							*/
/*										*/
/*------------------------------------------------------------------------------*/
/*										*/
/*	Coded by:       Tom Adams						*/
/*	Affiliation:    NOAA/NWS/Office of Hydrology/HRL			*/
/*	Date:           01/31/91						*/
/*	Revision:	11/15/94	from mods_dialog.c			*/
/*										*/
/*	Modified by:    D. Page - 29 Oct. 1995, 7 Nov. 1995, 11 Nov. 1995	*/
/*										*/
/********************************************************************************/


#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"


extern void unmanageDialogCB(Widget, Widget, XtPointer);
extern void resetInputFocusToDefaultWindowCB(Widget, Widget, XmAnyCallbackStruct *);
extern void set_currentModSavedTrueCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);
extern void call_setModInterfaceElementsCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);
extern void call_createSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void saveViewerChangesCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void undoViewerChangesCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void viewerCloseSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);

void createModTheSameDialog(Mods_everythingStruct *);

/************************************************************************/
/*									*/
/*	createValueChangeDialogs()					*/
/*									*/
/*									*/
/************************************************************************/


void createValueChangeDialogs(Mods_everythingStruct *data)
{

	int		n;
	Widget          warning_cancel_button;
	Widget          error_cancel_button;
	Arg		wargs[3];



	n = 0;
	XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
	data->dialogStruct->valueWarningMessageBox
			= XmCreateWarningDialog
				(
				data->widgetData->ifp_modsShell,
				"valueWarningMessageBox",
				wargs,
				n
				);


	n = 0;
	XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
	data->dialogStruct->valueErrorMessageBox
			= XmCreateErrorDialog
				(
				data->widgetData->ifp_modsShell,
				"valueErrorMessageBox",
				wargs,
				n
				);


	warning_cancel_button = XmMessageBoxGetChild(data->dialogStruct->valueWarningMessageBox,
					XmDIALOG_CANCEL_BUTTON);
	XtUnmanageChild(warning_cancel_button);

	error_cancel_button = XmMessageBoxGetChild(data->dialogStruct->valueErrorMessageBox,
					XmDIALOG_CANCEL_BUTTON);
	XtUnmanageChild(error_cancel_button);
}

/************************************************************************/
/*									*/
/*	createModNotSavedDialog()					*/
/*	   taken from mods_dialogs.c					*/
/*									*/
/************************************************************************/


void createModNotSavedDialog(Mods_everythingStruct *data)
{
	Widget          cancel_button;                  /* Cancel button widget data structure          */
	Widget          ok_button;                      /* Ok button widget data structure              */
	Widget          help_button;                    /* Help button widget data structure            */
	Arg             wargs[8];                       /* Window resource data structure array         */
        int             n;
	char            string[150];                    /* Array holding characters of text             */
	XmString        xmMessageString;


   strcpy(string, "None of the data have been saved for the current ");
   strcat(string, " ");
   strcat(string, " run-time mod.\n\n");
   strcat(string, "Do you wish to save data for this mod?");

   xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

   n=0;
   XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
   XtSetArg(wargs[n], XmNokLabelString, XmStringCreate("Yes", XmSTRING_DEFAULT_CHARSET)); n++;
   XtSetArg(wargs[n], XmNcancelLabelString, XmStringCreate("No", XmSTRING_DEFAULT_CHARSET)); n++;
   XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
   data->dialogStruct->modNotSavedMessageBox = XmCreateWarningDialog
			                       (
			                       data->widgetData->ifp_modsShell,
			                       "modNotSavedMessageBox",
			                       wargs,
			                       n
			                       );

   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
	         (XtCallbackProc)resetInputFocusToDefaultWindowCB, NULL);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		 (XtCallbackProc)call_createSelectionCB, data);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNokCallback,
		 (XtCallbackProc)call_setModInterfaceElementsCB, data);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNcancelCallback,
		 (XtCallbackProc)set_currentModSavedTrueCB, data);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNcancelCallback,
		 (XtCallbackProc)resetInputFocusToDefaultWindowCB, NULL);
   XtAddCallback(data->dialogStruct->modNotSavedMessageBox, XmNcancelCallback,
		 (XtCallbackProc)call_setModInterfaceElementsCB, data);

   n=0;
   ok_button = XmMessageBoxGetChild(data->dialogStruct->modNotSavedMessageBox,
                                    XmDIALOG_OK_BUTTON);
   XtSetArg(wargs[n], XtNwidth, 100); n++;
   XtSetValues(ok_button, wargs, n);

   n=0;
   cancel_button = XmMessageBoxGetChild(data->dialogStruct->modNotSavedMessageBox, 
                                        XmDIALOG_CANCEL_BUTTON);
   XtSetArg(wargs[0], XtNwidth, 100); n++;
   XtSetValues(cancel_button, wargs, n);

}

/* *****************************************************************************

	createModTheSameDialog()
           taken from mods_dialogs.c
   ***************************************************************************** */

void createModTheSameDialog(Mods_everythingStruct *data)
{

	Widget          cancel_button;  /* Cancel button widget data structure          */
	Widget          ok_button;      /* Ok button widget data structure              */
	Widget          help_button;    /* Help button widget data structure            */
	Arg             wargs[4];       /* Window resource data structure array         */

	char            string[150];    /* Array holding characters of text             */
	XmString        xmMessageString;
        int             n;

strcpy(string, "This ");
strcat(string, " mod and a mod previously saved are identical.\n");
strcat(string, "The data will not be saved again.");

xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

n=0;
XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
data->dialogStruct->modTheSameMessageBox = XmCreateInformationDialog
			                   (
			                    data->widgetData->ifp_modsShell,
			                    "mod_the_same_Dialog",
			                    wargs,
			                    n
			                   );

n=0;
ok_button = XmMessageBoxGetChild(data->dialogStruct->modTheSameMessageBox, 
                                 XmDIALOG_OK_BUTTON);
XtSetArg(wargs[n], XtNwidth, 100); n++;
XtSetValues(ok_button, wargs, n);

n=0;
help_button = XmMessageBoxGetChild(data->dialogStruct->modTheSameMessageBox,
                                   XmDIALOG_HELP_BUTTON);
XtSetArg(wargs[n], XtNwidth, 100); n++;
XtSetValues(help_button, wargs, n);


cancel_button = XmMessageBoxGetChild(data->dialogStruct->modTheSameMessageBox, 
                                     XmDIALOG_CANCEL_BUTTON);
XtUnmanageChild(cancel_button);

}


/* *****************************************************************************

	createModDateErrorDialog()
           taken from mods_dialogs.c
	added by gzhou 11/26/2003
   ***************************************************************************** */

void createModDateErrorDialog(Mods_everythingStruct *data)
{

	int		n;
	Widget          cancel_button;      /* cancel button widget data structure              */
	Arg		wargs[3];
	char            string[150];    /* Array holding characters of text             */
	XmString        xmMessageString;

	strcpy(string, "Pick up a date before create Mods!\n\tThe Mod create incomplete!");

	xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

	n=0;
	XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
	XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
	data->dialogStruct->modDateErrorMessageBox = XmCreateErrorDialog
			                	   (
			                    	data->widgetData->ifp_modsShell,
			                    	"Mods_date_error_Dialog",
			                    	wargs,
			                    	n
			                	   );

	cancel_button = XmMessageBoxGetChild(data->dialogStruct->modDateErrorMessageBox,
					XmDIALOG_CANCEL_BUTTON);
	XtUnmanageChild(cancel_button);

}




/************************************************************************/
/*									*/
/*	createModViewerNotSavedDialog()					*/
/*  Function to create a popup to warn user that something was changed	*/
/*   in a mod viewer window but not saved.				*/
/************************************************************************/


void createModViewerNotSavedDialog(Mods_everythingStruct *data)
{
	Widget          cancel_button;                  /* Cancel button widget data structure  */
	Widget          ok_button;                      /* Ok button widget data structure      */
	Widget          help_button;                    /* Help button widget data structure    */
	Arg             wargs[8];                       /* Window resource data structure array */
        int             n;
	char            string[150];                    /* Array holding characters of text     */
	XmString        xmMessageString;
	char            window_name[15];
	
   memset(window_name, '\0', 15);
   strcpy(window_name, "Mods from file");
   
   strcpy(string, "Data has been changed in the ");
   strcat(string, window_name);
   strcat(string, " window.\n\n");
   strcat(string, "Do you wish to save data for this window?");

   xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

   n=0;
   XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
   XtSetArg(wargs[n], XmNokLabelString, XmStringCreate("Yes", XmSTRING_DEFAULT_CHARSET)); n++;
   XtSetArg(wargs[n], XmNcancelLabelString, XmStringCreate("No", XmSTRING_DEFAULT_CHARSET)); n++;
   XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
   data->dialogStruct->modViewerNotSavedMB = XmCreateWarningDialog
			                       (
			                       data->widgetData->ifp_modsShell,
			                       "modViewerNotSavedMB",
			                       wargs,
			                       n
			                       );

   XtAddCallback(data->dialogStruct->modViewerNotSavedMB , XmNokCallback,
	         (XtCallbackProc)resetInputFocusToDefaultWindowCB, NULL);
   XtAddCallback(data->dialogStruct->modViewerNotSavedMB , XmNokCallback,
		 (XtCallbackProc)saveViewerChangesCB, data);
   XtAddCallback(data->dialogStruct->modViewerNotSavedMB , XmNokCallback,
		 (XtCallbackProc)viewerCloseSelectionCB, data);		 
   XtAddCallback(data->dialogStruct->modViewerNotSavedMB , XmNcancelCallback,
		 (XtCallbackProc)resetInputFocusToDefaultWindowCB, NULL);
   XtAddCallback(data->dialogStruct->modViewerNotSavedMB , XmNcancelCallback,
		 (XtCallbackProc)undoViewerChangesCB, data);
   XtAddCallback(data->dialogStruct->modViewerNotSavedMB , XmNcancelCallback,
		 (XtCallbackProc)viewerCloseSelectionCB, data);

   n=0;
   ok_button = XmMessageBoxGetChild(data->dialogStruct->modViewerNotSavedMB,
                                    XmDIALOG_OK_BUTTON);
   XtSetArg(wargs[n], XtNwidth, 100); n++;
   XtSetValues(ok_button, wargs, n);

   n=0;
   cancel_button = XmMessageBoxGetChild(data->dialogStruct->modViewerNotSavedMB, 
                                        XmDIALOG_CANCEL_BUTTON);
   XtSetArg(wargs[0], XtNwidth, 100); n++;
   XtSetValues(cancel_button, wargs, n);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_dialogs.c,v $";
 static char rcs_id2[] = "$Id: Mods_dialogs.c,v 1.5 2006/04/18 13:34:04 aivo Exp $";}
/*  ===================================================  */

}

