
/* *************************************************************************************

	run_dialogs.c

	This file contains all the functions that create:

			Information,
			Warning, and
			Error

	popup dialog windows...

   -------------------------------------------------------------------------------------

	Coded by:       Tom Adams
	Affiliation:    NOAA/NWS/Office of Hydrology/Hydrologic Research Laboratory
	Date:           05/03/91
	Revision:       05/03/91, 06/07/91, 12/29/92, 1/29/93


   ************************************************************************************* */




#include "libXifp.h"
#include "run_dates.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"
#include "ifp_help.h"
#include "help.h"






/* *****************************************************************************

	popup_quitting_NWSRFS_warningDialog()

   ***************************************************************************** */

void popup_quitting_NWSRFS_warningDialog(someWidgets)
	the_widget_struct       *someWidgets;
{

	Widget          shell;
	Widget          cancel_button, ok_button, help_button;
	Arg             wargs[8];

	char            string[101];
	XmString        xmMessageString;
	int             n;

	help_struct     *help_data;




 strcpy(string, "Are you sure you want to terminate NWSRFS execution?");

 xmMessageString = XmStringCreateSimple(string);

 n = 0;
 XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
 XtSetArg(wargs[n], XmNokLabelString, XmStringCreateSimple("Yes")); n++;
 XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
 shell = XmCreateWarningDialog
			(
			someWidgets->tree_shell,
			"cancelAll_warningShell",
			wargs,
			n
			);
 XtAddCallback(shell, XmNokCallback, set_leave_NWSRFS_to_Yes, someWidgets);
 XmStringFree(xmMessageString);

 help_data = (help_struct *) malloc(sizeof(help_struct));
 help_data->parent = someWidgets->tree_shell;
 help_data->message_widget_name = "cancelAll_warning_dialog";

 XtAddCallback(shell, XmNhelpCallback, popup_help_window, "QUIT_RUN_DIALOG");


 ok_button = XmMessageBoxGetChild(shell, XmDIALOG_OK_BUTTON);
 XtSetArg(wargs[0], XtNwidth, 100);
 XtSetValues(ok_button, wargs, 1);


 help_button = XmMessageBoxGetChild(shell, XmDIALOG_HELP_BUTTON);
 XtSetArg(wargs[0], XtNwidth, 100);
 XtSetValues(help_button, wargs, 1);

 cancel_button = XmMessageBoxGetChild(shell, XmDIALOG_CANCEL_BUTTON);
 XtSetArg(wargs[0], XtNwidth, 100);
 XtSetValues(cancel_button, wargs, 1);

 XtManageChild(shell);

}


/* *****************************************************************************

	popup_mods_not_written_to_file_warningDialog()

		creates the PopupShell widget and children for the warning
		message that is displayed when the user has created mods and,
		instead of re-running the current segment, has selected 'Next'
		or 'Go upstream' from the menu in 'run_NWSRFS'... any run-time
		mods that were created (and saved in RAM) will be lost if 'Next'
		or 'Go upstream' are executed.

   ***************************************************************************** */

void popup_mods_not_written_to_file_warningDialog(someWidgets)
	the_widget_struct       *someWidgets;
{

	Widget          shellWidget;
	Widget          cancel_button, ok_button, help_button;
	Arg             wargs[4];

	int             *number_of_mods_to_write;
	int             n;
	char            string[201];
	char            num_modsString[3];
	XmString        xmMessageString;

	Display         *display;
	Window          root;

	long            offset = 0;
	Atom            type;
	int             format, nitems, left;
	int             i, last_item, number_of_items;

	help_struct     *help_data;




display = XtDisplay(someWidgets->tree_shell);
root = DefaultRootWindow(display);

help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = someWidgets->tree_shell;
help_data->message_widget_name = "mods_not_written_warning";


if(XGetWindowProperty
	(
	display,
	root,
	IFPA_number_of_mods_to_write,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_number_of_mods_to_write_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&number_of_mods_to_write) == Success && type == IFPA_number_of_mods_to_write_type){
	memset(num_modsString, '\0', 3);
	sprintf(num_modsString, "%d", *number_of_mods_to_write);
	}

strcpy(string, "There are ");
strcat(string, num_modsString);
strcat(string, " remaining Run-time mods that have not been written\n");
strcat(string, "to a file in mods card image format and they will be lost unless\n");
strcat(string, "the segment is re-run.");

xmMessageString = XmStringCreateSimple(string);

n = 0;
XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
XtSetArg(wargs[n], XmNokLabelString, XmStringCreateSimple("Continue")); n++;
shellWidget = XmCreateWarningDialog
			(
			someWidgets->tree_shell,
			"mods_not_written_warningShell",
			wargs,
			n
			);
XtAddCallback(shellWidget, XmNhelpCallback, popup_help_window, "MODS_NOT_WRITTEN_DIALOG");
XtAddCallback(shellWidget, XmNokCallback, continue_with_next_segment, someWidgets);
XmStringFree(xmMessageString);

XtManageChild(shellWidget);

ok_button = XmMessageBoxGetChild(shellWidget, XmDIALOG_OK_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(ok_button, wargs, 1);

help_button = XmMessageBoxGetChild(shellWidget, XmDIALOG_HELP_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(help_button, wargs, 1);


cancel_button = XmMessageBoxGetChild(shellWidget, XmDIALOG_CANCEL_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(cancel_button, wargs, 1);


}




/* *****************************************************************************

	popup_pending_mods_not_deleted_warningDialog()

		creates the PopupShell widget and children for the warning
		message that is displayed when the user has deleted mods and,
		not saved them before trying to rerun, next, continue, goto,
		or quit
		instead of re-running the current segment
		... any run-time
		mods that were created will be lost if other than the rerun
		command is executed.

   ***************************************************************************** */

void popup_pending_mods_not_deleted_warningDialog(someWidgets)
	the_widget_struct       *someWidgets;
{

	Widget          shellWidget;
	Widget          cancel_button, ok_button, help_button;
	int             n;
	Arg             wargs[8];

	int             *numberOfModsToDeleteFromFile;
	char            string[501], string_temp[101];
	XmString        xmMessageString;

	Display         *display;
	Window          root;

	long            offset = 0;
	Atom            type;
	int             format, nitems, left;
	int             i, last_item, number_of_items;

	help_struct     *help_data;




display = XtDisplay(someWidgets->tree_shell);
root = DefaultRootWindow(display);

help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = someWidgets->tree_shell;
help_data->message_widget_name = "mods_pending_not_deleted_warning";

memset(string, '\0', 501);
memset(string_temp, '\0', 101);

 if(XGetWindowProperty
    (
    display,
    root,
    IFPA_num_mods_to_delete_fromFile,
    offset,
    (long) sizeof(int),
    FALSE,
    IFPA_num_mods_to_delete_fromFile_type,
    (Atom *)&type,
    (int *)&format,
    (unsigned long *)&nitems,
    (unsigned long *)&left,
    (unsigned char **)&numberOfModsToDeleteFromFile) == Success &&
    type == IFPA_num_mods_to_delete_fromFile_type
   )
    {
     if(*numberOfModsToDeleteFromFile > 0)
       {
	sprintf(string, "WARNING: %d Run-time mod%s ha%s been marked ",
			*numberOfModsToDeleteFromFile,
			*numberOfModsToDeleteFromFile == 1 ? "" : "s",
			*numberOfModsToDeleteFromFile == 1 ? "s" : "ve");
	strcat(string, "to delete, but not actually deleted.\n");

xmMessageString = XmStringCreateSimple(string);

n = 0;
XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
XtSetArg(wargs[n], XmNokLabelString, XmStringCreateSimple(someWidgets->current_command)); n++;
XtSetArg(wargs[n], XmNcancelLabelString, XmStringCreateSimple("Cancel")); n++;
XtSetArg(wargs[n], XmNdefaultButtonType, XmDIALOG_OK_BUTTON); n++;
shellWidget = XmCreateWarningDialog
			(
			someWidgets->tree_shell,
			"mods_pending_not_deleted_warningShell",
			wargs,
			n
			);
XtAddCallback(shellWidget, XmNhelpCallback, popup_help_window, "MODS_DELETED_NOT_SAVED");
XtAddCallback(shellWidget, XmNokCallback, continue_with_requested_command_deleted, someWidgets);
XmStringFree(xmMessageString);

XtManageChild(shellWidget);
       }
    }

}

/* *****************************************************************************

	popup_pending_mods_not_saved_warningDialog()

		creates the PopupShell widget and children for the warning
		message that is displayed when the user has created mods and,
		not saved them before trying to rerun, next, continue, goto,
		or quit
		instead of re-running the current segment
		... any run-time
		mods that were created will be lost if other than the rerun
		command is executed.

   ***************************************************************************** */

void popup_pending_mods_not_saved_warningDialog(someWidgets)
	the_widget_struct       *someWidgets;
{

	Widget          shellWidget;
	Widget          cancel_button, ok_button, help_button;
	int             n;
	Arg             wargs[8];

	int             *currentModSaved;
	int             *currentRangeModSaved;
	char            string[501], string_temp[101];
	XmString        xmMessageString;

	Display         *display;
	Window          root;

	long            offset = 0;
	Atom            type;
	int             format, nitems, left;
	int             i, last_item, number_of_items;

	help_struct     *help_data;




 display = XtDisplay(someWidgets->tree_shell);
 root = DefaultRootWindow(display);

 help_data = (help_struct *) malloc(sizeof(help_struct));
 help_data->parent = someWidgets->tree_shell;
 help_data->message_widget_name = "mods_pending_not_saved_warning";

 memset(string, '\0', 501);
 memset(string_temp, '\0', 101);
 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_rangemods_saved,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_rangemods_saved_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&currentRangeModSaved) == Success &&
	type == IFPA_rangemods_saved_type){
	;

	}
 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_current_mod_saved,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_current_mod_saved_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&currentModSaved) == Success &&
	type == IFPA_current_mod_saved_type){
	;
	}

	if(*currentModSaved == 0 || *currentRangeModSaved == 1)
		{
		strcpy(string, "The last Run-time mod that you made has not been saved.\n");
		sprintf(string_temp, "If you select \"%s\", this mod will not be saved.\n",
				someWidgets->current_command);
		strcat(string, string_temp);
		strcat(string, "If you want to save this mod, select \"Cancel\".\n");
		sprintf(string_temp, "This will cancel execution of the \"%s\" command.\n",
			someWidgets->current_command);
		strcat(string, string_temp);
		strcat(string, "You must then select \"Save\" in the Mods Control menu to actually");
		strcat(string, " save this mod,\n");
		strcat(string, "and Rerun the segment to have this mod");
		strcat(string, " affect the model results.\n");

		xmMessageString = XmStringCreateLtoR(string, "charset1");

		n = 0;
		XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
		XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
		XtSetArg(wargs[n], XmNokLabelString, XmStringCreateSimple(someWidgets->current_command)); n++;
		XtSetArg(wargs[n], XmNcancelLabelString, XmStringCreateSimple("Cancel")); n++;
		XtSetArg(wargs[n], XmNdefaultButtonType, XmDIALOG_OK_BUTTON); n++;
		shellWidget = XmCreateWarningDialog
					(
					someWidgets->tree_shell,
					"mods_pending_not_saved_warningShell",
					wargs,
					n
					);
		XtAddCallback(shellWidget, XmNhelpCallback, popup_help_window, "MODS_NOT_SAVED_DIALOG");
		XtAddCallback(shellWidget, XmNokCallback, continue_with_requested_command_saved, someWidgets);
		XmStringFree(xmMessageString);

		XtManageChild(shellWidget);
		*currentRangeModSaved = 0;
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

}



/* *****************************************************************************

	create_NWSRFS_working_Dialog()

   ***************************************************************************** */

void create_NWSRFS_working_Dialog(someWidgets)
	the_widget_struct       *someWidgets;
{
	Widget          shell, separator;
	Widget          cancel_button, ok_button, help_button;
	Arg             wargs[5];
	int             n;
	Boolean         defaultPosition;

defaultPosition = FALSE;

n = 0;
XtSetArg(wargs[n], XmNdefaultPosition, defaultPosition); n++;

shell = XmCreateWorkingDialog
			(
			someWidgets->tree_shell,
			"NWSRFS_working_Shell",
			wargs,
			n
			);

someWidgets->NWSRFS_working_shell = shell;

ok_button = XmMessageBoxGetChild(shell, XmDIALOG_OK_BUTTON);
XtUnmanageChild(ok_button);

help_button = XmMessageBoxGetChild(shell, XmDIALOG_HELP_BUTTON);
XtUnmanageChild(help_button);

cancel_button = XmMessageBoxGetChild(shell, XmDIALOG_CANCEL_BUTTON);
XtUnmanageChild(cancel_button);

separator = XmMessageBoxGetChild(shell, XmDIALOG_SEPARATOR);
XtUnmanageChild(separator);
}



/* *****************************************************************************

	create_NWSRFS_stopped_Dialog()

   ***************************************************************************** */

void create_NWSRFS_stopped_Dialog(someWidgets)
	the_widget_struct       *someWidgets;
{
	Widget          shell;
	Widget          cancel_button, ok_button, help_button;
	Arg             wargs[5];
	int             n;
	Boolean         defaultPosition;

defaultPosition = FALSE;

n = 0;
XtSetArg(wargs[n], XmNdefaultPosition, defaultPosition); n++;

shell = XmCreateWarningDialog(someWidgets->toplevel, "NWSRFS_stopped_Shell", wargs, n);

someWidgets->NWSRFS_stopped_shell = shell;

ok_button = XmMessageBoxGetChild(shell, XmDIALOG_OK_BUTTON);
XtUnmanageChild(ok_button);

help_button = XmMessageBoxGetChild(shell, XmDIALOG_HELP_BUTTON);
XtUnmanageChild(help_button);

cancel_button = XmMessageBoxGetChild(shell, XmDIALOG_CANCEL_BUTTON);
XtUnmanageChild(cancel_button);
}



/* *****************************************************************************

	popup_modsNotDeleted_warningDialog()


   ***************************************************************************** */

void popup_modsNotDeleted_warningDialog(someWidgets)
	the_widget_struct       *someWidgets;
{

	Widget          shellWidget;
	Widget          cancel_button, ok_button, help_button;
	Arg             wargs[4];

	int             format, nitems, left;
	int             i, last_item, number_of_items;
	int             n;
	int             *number_of_mods_to_delete;

	char            string[201];
	char            num_modsString[3];

	XmString        xmMessageString;

	Display         *display;
	Window          root;

	long            offset = 0;
	Atom            type;

	help_struct     *help_data;



display = XtDisplay(someWidgets->tree_shell);
root = DefaultRootWindow(display);

help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = someWidgets->tree_shell;
help_data->message_widget_name = "mods_not_deleted_warning";


if(XGetWindowProperty
	(
	display,
	root,
	IFPA_num_mods_to_delete_fromFile,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_num_mods_to_delete_fromFile_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&number_of_mods_to_delete) == Success && type == IFPA_num_mods_to_delete_fromFile_type){

	memset(num_modsString, '\0', 3);
	sprintf(num_modsString, "%d", *number_of_mods_to_delete);
	}

strcpy(string, "There are ");
strcat(string, num_modsString);
strcat(string, " Run-time mods that have been deleted for this mod,\n");
strcat(string, "but changes to the Mods file have not been made. Unless you\n");
strcat(string, "save these changes now, these mods will still be used.");

xmMessageString = XmStringCreateSimple(string);

n = 0;
XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
XtSetArg(wargs[n], XmNokLabelString, XmStringCreateSimple("Continue")); n++;
shellWidget = XmCreateWarningDialog
			(
			someWidgets->tree_shell,
			"mods_not_written_warningShell",
			wargs,
			n
			);
XtAddCallback(shellWidget, XmNhelpCallback, popup_help_window, "MODS_NOT_DELETED_DIALOG");
XtAddCallback(shellWidget, XmNokCallback, continue_with_next_segment, someWidgets);
XmStringFree(xmMessageString);

XtManageChild(shellWidget);

ok_button = XmMessageBoxGetChild(shellWidget, XmDIALOG_OK_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(ok_button, wargs, 1);

help_button = XmMessageBoxGetChild(shellWidget, XmDIALOG_HELP_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(help_button, wargs, 1);


cancel_button = XmMessageBoxGetChild(shellWidget, XmDIALOG_CANCEL_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(cancel_button, wargs, 1);


}



/* *****************************************************************************

	int AskUser()

	Creates a dialog-box displaying the question string and buttons for "OK",
	"Cancel" and "Help".

	Provides a clear flow of control for synchronous queries;  AskUser() blocks
	further user interaction until 'answer' is set to one of the valid values.
	If it is not a "yes" answer, the code drops out of the callback and back to
	an event-processing loop:

	This is accomplished by entering a second event-processing loop and
	waiting until the user answers the question; the answer is returned to the
	calling function.

	The variable 'answer' is set when the user finally selects one of the choices.
	The code unwraps back to the point at which an answer was needed and
	continues from there.

	from: Dan Heller

   ***************************************************************************** */

int AskUser(w, string)
	Widget          w;
	char            *string;
{
	int             answer = RET_NONE;      /* Some not-used marker...      */
	Widget          dialog;
	Arg             wargs[4];
	int             n = 0;
	XtAppContext    context;
	XmString        xmstr;
	XmString        xmstr_Yes;
	XmString        xmstr_No;


 /* modify so it will recognize new line characters in the string 
  * dp - 3 Sept. 1997
  */
 xmstr = XmStringCreateLtoR(string, "charset1");

 n = 0;
 XtSetArg(wargs[n], XmNmessageString, xmstr); n++;
 XtSetArg(wargs[n], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL); n++;
 xmstr_Yes = XmStringCreateSimple("Yes");
 XtSetArg(wargs[n], XmNokLabelString, xmstr_Yes); n++;
 xmstr_No = XmStringCreateSimple("No");
 XtSetArg(wargs[n], XmNcancelLabelString, xmstr_No); n++;
 dialog = XmCreateQuestionDialog(w, "query_dialog", wargs, n);
 XtAddCallback(dialog, XmNokCallback, response, &answer);
 XtAddCallback(dialog, XmNcancelCallback, response, &answer);
 XtAddCallback(dialog, XmNhelpCallback, response, &answer);
 XtManageChild(dialog);

 XmStringFree(xmstr_Yes);
 XmStringFree(xmstr_No);


 context = XtWidgetToApplicationContext(w);
 while (answer == RET_NONE || XtAppPending(context))
	{
	XtAppProcessEvent(context, XtIMAll);
	}

 XmStringFree(xmstr);           /* Free space held by 'xmstr'...        */
 XtDestroyWidget(dialog);       /* Blow away the dialog box and shell...*/
 return answer;

}



/* *****************************************************************************

	void response()

   ***************************************************************************** */

void response(w, client, call)
	Widget          w;
	XtPointer       client;
	XtPointer       call;
{

 int *answer = (int *) client;
 XmAnyCallbackStruct *reason = (XmAnyCallbackStruct *) call;
	switch (reason->reason) {
	case XmCR_OK:
		*answer = RET_YES;      /* some #define value */
		break;
	case XmCR_CANCEL:
		*answer = RET_NO;
		break;
	case XmCR_HELP:
		*answer = RET_HELP;
		break;
	default:
		return;
	}
}



/* *****************************************************************************

	create_Version_Dialog()

   ***************************************************************************** */

void create_Version_Dialog(data, version)
	the_widget_struct       *data;
	char                    *version;
{
	Widget          shell;
	Widget          cancel_button, ok_button, help_button;

	int             n;
	int             length;

	char            *string;
	char            *message = "NWS River Forecast System - Interactive Forecast Program\n(NWSRFS-IFP)\nVersion: ";

	Arg             wargs[5];
	XmString        cancel_xmstring, message_xmstring;
	Display         *display;
	XmFontList      fontList;
	XFontStruct     *font;



 display = XtDisplay(data->toplevel);
 /*font = XLoadQueryFont(display, "-*-helvetica-bold-r-*--14-*");*/
 font = XLoadQueryFont(display, "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1");
 
 fontList = XmFontListCreate(font, "charset1");

 length = strlen(message) + strlen(version) + 1;
 string = (char *) malloc(sizeof(char) * length);
 memset(string, '\0', length);

 strcpy(string, message);
 strcat(string, version);

 message_xmstring = XmStringCreateLtoR(string, "charset1");
 cancel_xmstring = XmStringCreateLtoR("OK", "charset1");


 n = 0;
 XtSetArg(wargs[n], XmNcancelLabelString, cancel_xmstring); n++;
 XtSetArg(wargs[n], XmNmessageAlignment, XmALIGNMENT_CENTER); n++;
 XtSetArg(wargs[n], XmNmessageString, message_xmstring); n++;
 XtSetArg(wargs[n], XmNdialogType, XmDIALOG_INFORMATION); n++;
 shell = XmCreateMessageDialog(data->toplevel, "IFP_version_Shell", wargs, n);

 XtAddCallback(shell, XmNcancelCallback, reset_version_ToggleButton, data->version_ToggleButton);

 XmStringFree(cancel_xmstring);
 XmStringFree(message_xmstring);

 data->version_shell = shell;

 ok_button = XmMessageBoxGetChild(shell, XmDIALOG_OK_BUTTON);
 XtUnmanageChild(ok_button);

 help_button = XmMessageBoxGetChild(shell, XmDIALOG_HELP_BUTTON);
 XtUnmanageChild(help_button);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/run_dialogs.c,v $";
 static char rcs_id2[] = "$Id: run_dialogs.c,v 1.5 2006/04/07 13:30:26 aivo Exp $";}
/*  ===================================================  */

}

