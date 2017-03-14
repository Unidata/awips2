/* File: help_dialogs.c
 */




#include "libXifp.h"
#include "ifp_help.h"
#include "ifp_atoms.h"


XtCallbackProc  remap_previous_Dialog();


/* ****************************************************************************

	 create_help_Dialog()  - creates dialog for the help dialog widgets



   **************************************************************************** */

void create_help_Dialog(w, help, call_data)
	Widget          w;              /* Widget data structure        */
	help_struct     *help;          /* Data structure for help      */
	caddr_t         call_data;      /* Call back structure pointer  */
{
	Widget          message_shell, help_button, ok_button, cancel_button;
	char            name[50];       /* Array of character name      */
	char            *message_string;        /* Message string pointer to character  */
	Arg             wargs[5];       /* Window resource data structure array */
	XmString        xmMessage;
	XtCallbackRec   cancelCallbackList[2];
 
cancelCallbackList[0].callback = (XtCallbackProc) remap_previous_Dialog;
cancelCallbackList[0].closure  = (Widget) w;
cancelCallbackList[1].callback = (XtCallbackProc) NULL;
cancelCallbackList[1].closure  = (XtPointer) NULL;

memset(name, '\0', 50);


strcpy(name, "Help_for_");
strcat(name, help->message_widget_name);


XtSetArg(wargs[0], XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON);
XtSetArg(wargs[1], XmNcancelLabelString, XmStringCreate("OK",
				XmSTRING_DEFAULT_CHARSET));
XtSetArg(wargs[2], XmNcancelCallback, cancelCallbackList);
message_shell = XmCreateInformationDialog
				(
				help->parent,
				name,
				wargs,
				3
				);
/*
 * The implementation of the Xt resource translation manager is not
 *      interpreting '\n' (ie, the new line character), so a fix is
 *      made by getting the XmNmessageString resource, reconverting
 *      it to a string, then using XmStringCreateLtoR to interpret
 *      the '\n';
 * The XmNmessageString is then passed back to messageBox widget...
 */

/*
XtSetArg(wargs[0], XmNmessageString, &xmMessage);
XtGetValues(message_shell, wargs, 1);

XmStringGetLtoR(xmMessage, XmSTRING_DEFAULT_CHARSET, &message_string);

XtSetArg(wargs[0], XmNmessageString, XmStringCreateLtoR(message_string,
		XmSTRING_DEFAULT_CHARSET));
XtSetValues(message_shell, wargs, 1);
*/

XtManageChild(message_shell);

cancel_button = XmMessageBoxGetChild(message_shell, XmDIALOG_CANCEL_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(cancel_button, wargs, 1);


ok_button = XmMessageBoxGetChild(message_shell, XmDIALOG_OK_BUTTON);
help_button = XmMessageBoxGetChild(message_shell, XmDIALOG_HELP_BUTTON);
XtUnmanageChild(ok_button);
XtUnmanageChild(help_button);

}





/* ****************************************************************************

	 remap_previous_Dialog()
		       remaps the previous dialog widget and pops up the
		       message shell

   **************************************************************************** */

XtCallbackProc remap_previous_Dialog(w, message_shell, call_data)
	Widget          w;              /* Widget data structure        */
	Widget          message_shell;  /* Message shell widget data structure  */
	caddr_t         call_data;      /* Call back structure pointer      */
{

XtManageChild(message_shell);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/help_dialogs.c,v $";
 static char rcs_id2[] = "$Id: help_dialogs.c,v 1.1 1995/09/08 15:00:46 page Exp $";}
/*  ===================================================  */

}
