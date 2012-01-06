
/* ****************************************************************************************************

	NWSRFS_no_startup.c

			a message dialog application that informs the user that only one copy of
			the IFP/NWSRFS application is permitted to run at a time

	Coded by:       Tom Adams
	Affiliation:    NOAA/NWS/Office of Hydrology/Hydrologic Research Lab.

	Date:           06/26/91

   **************************************************************************************************** */

#include "NWSRFS_no_startup.h"
#include "libXs.h"
#include "ifp_help.h"




NWSRFS_no_startup_main (argc, argv)
	int             argc;
	char            **argv;
{
	Widget          toplevel;
	Widget          shell;
	Widget          ok_button, cancel_button, help_button;

	XEvent          event;
	Display         *display;
	Window          root;

	int             n;

	Arg             wargs[5];
	char            string[101];

	XmString        xmMessageString;

	help_struct     *help_data;



XtCallbackRec okCallbackList[] =
	{
	{exit_dialog, NULL},
	{(XtCallbackProc) NULL,(caddr_t) NULL}
	};



 toplevel = XtInitialize(argv[0], "NWSRFS_cant_run", NULL, 0, &argc, argv);


 display = XtDisplay(toplevel);
 root = DefaultRootWindow(display);




help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = toplevel;
help_data->message_widget_name = "cant_run_NWSRFS_dialog";


strcpy(string, "Only one copy of the Interactive Forecast Program/NWSRFS can run at a time.");

xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

XtSetArg(wargs[0], XmNmessageString, xmMessageString);
XtSetArg(wargs[1], XmNokCallback, okCallbackList);
XtSetArg(wargs[2], XmNminimizeButtons, FALSE);
XtSetArg(wargs[3], XmNdialogType, XmDIALOG_ERROR);
shell = (Widget)XmCreateMessageBox
		(
		toplevel,
		"op_errorShell",
		wargs,
		4
		);
XtAddCallback(shell, XmNhelpCallback, create_help_Dialog, help_data);


ok_button = (Widget)XmMessageBoxGetChild(shell, XmDIALOG_OK_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(ok_button, wargs, 1);

help_button = (Widget)XmMessageBoxGetChild(shell, XmDIALOG_HELP_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(help_button, wargs, 1);

XtManageChild(shell);

cancel_button = (Widget)XmMessageBoxGetChild(shell, XmDIALOG_CANCEL_BUTTON);
XtUnmanageChild(cancel_button);




 XtRealizeWidget(toplevel);
 XtMainLoop();

}




/* *****************************************************************************

	exit_dialog()

   ***************************************************************************** */

void exit_dialog(w, client_data, call_data)
	Widget          w;
	caddr_t         *client_data;
	caddr_t         *call_data;
{

 exit(0);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/NWSRFS_no_startup/RCS/NWSRFS_no_startup.c,v $";
 static char rcs_id2[] = "$Id: NWSRFS_no_startup.c,v 1.2 2006/04/19 13:24:40 aivo Exp $";}
/*  ===================================================  */

}
