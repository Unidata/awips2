
/* ****************************************************************************************************

	final_popup.c

			a question dialog application that queries the user on whether or not updated
			forecast group & carryover dates information should be obtained from the
			central database server

	Coded by:       Tom Adams
	Affiliation:    NOAA/NWS/Office of Hydrology/Hydrologic Research Lab.

	Date:           07/02/91
        Modified by:    D. Page - added code to save gif files - 08/01/95
   **************************************************************************************************** */

#include "libXifp.h"
#include "run_dates.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"
#include "ifp_help.h"

#include "final_popup.h"




int popup_send_mods_Dialog(the_widget_struct *someWidgets)
{
	Widget          shell;
	Widget          yes_button, no_button, cancel_button, help_button;
	Widget          help_shell;

	Display         *display;
	Window          root;

	int             n;
	int             answer = RET_NONE;      /* Some not-used marker...      */

	Arg             wargs[8];
	char            string[151];

	XmString        xmMessageString;
	XtAppContext    context;

	help_struct     *help_data;
	help_cb_struct  *context_help;




 display = XtDisplay(someWidgets->toplevel);
 root = DefaultRootWindow(display);

 strcpy(string, "You are about to end the NWSRFS IFP.\n\n");
 if(save_gif)
 {
    strcat(string, "Do you want to send the new Run-time Modifications\n");
    strcat(string, "            and GIF files to the OFS?\n");
 }
 else
    strcat(string, "Do you want to send the new Run-time Modifications to the OFS?\n");   

 xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

 n = 0;
 XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
 XtSetArg(wargs[n], XmNokLabelString, XmStringCreate("Yes", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNcancelLabelString, XmStringCreate("No", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNhelpLabelString, XmStringCreate("Cancel", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
 shell = XmCreateQuestionDialog(someWidgets->toplevel, "send_mods_shell", wargs, n);
 someWidgets->send_mods_shell = shell;

 XtAddCallback(shell, XmNokCallback, update_info, someWidgets);
 XtAddCallback(shell, XmNokCallback,     response, &answer);
 XtAddCallback(shell, XmNcancelCallback, clean_gif_files, someWidgets);
 XtAddCallback(shell, XmNcancelCallback, response, &answer);
 XtAddCallback(shell, XmNhelpCallback,   response, &answer);


 yes_button = XmMessageBoxGetChild(shell, XmDIALOG_OK_BUTTON);
 XtVaSetValues(yes_button, XtNwidth, 100, NULL);

 cancel_button = XmMessageBoxGetChild(shell, XmDIALOG_CANCEL_BUTTON);
 XtVaSetValues(cancel_button, XtNwidth, 100, NULL);

 help_button = XmMessageBoxGetChild(shell, XmDIALOG_HELP_BUTTON);
 XtVaSetValues(help_button, XtNwidth, 100, NULL);

 XtManageChild(shell);

 context = XtWidgetToApplicationContext(someWidgets->toplevel);
 while (answer == RET_NONE || XtAppPending(context))
	{
	XtAppProcessEvent(context, XtIMAll);
	}

 XtDestroyWidget(shell);
 return answer;

}




/* *****************************************************************************

	update_info()

   ***************************************************************************** */

void update_info(Widget w, the_widget_struct *someWidgets, 
                 XmAnyCallbackStruct *call_data)
{

 final_mods_to_ofs(someWidgets->toplevel);   /* new program name for using OFS files */
 if(save_gif)
    gif_files_to_ofs(someWidgets->toplevel);    /* to send gif files to OFS */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/final_popup.c,v $";
 static char rcs_id2[] = "$Id: final_popup.c,v 1.1 1995/09/08 14:55:25 page Exp $";}
/*  ===================================================  */

}

