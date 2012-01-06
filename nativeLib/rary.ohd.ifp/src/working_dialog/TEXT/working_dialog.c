
#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"


void    popup_working_dialog();
void    exit_working_dialog();



working_dialog_main (argc, argv)
	int             argc;
	char            **argv;
{
	Widget          toplevel;
	Display         *display;
	Window          root;
	XEvent          event;





 toplevel = XtInitialize (argv[0], "Working_Dialog", NULL, 0, &argc, argv);

 intern_the_atoms(toplevel);

 display = XtDisplay(toplevel);
 root = DefaultRootWindow(display);

 popup_working_dialog(toplevel);

 XtRealizeWidget(toplevel);

 XSelectInput(display, root, PropertyChangeMask);

 /***************************    MAIN EVENT LOOP   **************************/

 while(TRUE)             /*      we're checking for PropertyNotify events        */
	{
	XtNextEvent(&event);

	switch  (event.type)
		{
		case PropertyNotify:
			if(event.xproperty.window == root && event.xproperty.atom == IFPA_end_of_file_initializations)
					exit_working_dialog();
			break;

		default:
			XtDispatchEvent(&event);
			break;
		}
	}


}



/* ***************************************************************************

	popup_working_dialog()

   *************************************************************************** */

void popup_working_dialog(toplevel)
	Widget          toplevel;
{

	Widget          shell;
	Widget          cancel_button, ok_button, help_button;
	Widget          separator;
	int             n;
	Arg             wargs[3];/*--AV change from 1 to 3--*/
	char            *string;
	XmString        xmstr_message;



 string = (char *) malloc(101);
 strcpy(string, "Please wait while files are being initialized for NWSRFS-IFP...");

 xmstr_message = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

 n = 0;
 XtSetArg(wargs[n], XmNmessageString, xmstr_message); n++;

 XtSetArg(wargs[n], XmNdialogType, XmDIALOG_WORKING); n++;
 shell = XtCreateManagedWidget("working_dialog_shell", xmMessageBoxWidgetClass,
 toplevel, wargs, n);

 ok_button = XmMessageBoxGetChild(shell, XmDIALOG_OK_BUTTON);
 XtUnmanageChild(ok_button);

 help_button = XmMessageBoxGetChild(shell, XmDIALOG_HELP_BUTTON);
 XtUnmanageChild(help_button);

 cancel_button = XmMessageBoxGetChild(shell, XmDIALOG_CANCEL_BUTTON);
 XtUnmanageChild(cancel_button);

 separator = XmMessageBoxGetChild(shell, XmDIALOG_SEPARATOR);
 XtUnmanageChild(separator);



}




/* ***************************************************************************

	exit_working_dialog()

   *************************************************************************** */

void exit_working_dialog()
{

	exit(0);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/working_dialog/RCS/working_dialog.c,v $";
 static char rcs_id2[] = "$Id: working_dialog.c,v 1.2 2002/02/11 19:51:53 dws Exp $";}
/*  ===================================================  */

}
