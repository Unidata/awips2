
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/PushB.h>
#include "global_defs.h"



/* ****************************************************************************

	 create_help_Dialog()

   **************************************************************************** */

Widget create_helpDialog(help)
	help_struct     *help;
{
	Widget          message_shell, help_button, ok_button, cancel_button;
	Widget          shell;
	char            name[50];
	char            *message_string;
	int             n;

	Arg             wargs[5];
	XmString        xmMessage;



 memset(name, '\0', 50);
 strcpy(name, "Help_for_");
 strcat(name, help->message_widget_name);



 n = 0;
 XtSetArg(wargs[n], XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON); n++;
 XtSetArg(wargs[n], XmNcancelLabelString, XmStringCreate("OK", XmSTRING_DEFAULT_CHARSET)); n++;
 message_shell = (Widget) XmCreateInformationDialog
				(
				help->parent,
				name,
				wargs,
				n
				);
 XtAddCallback(message_shell, XmNcancelCallback, unmap_HelpDialog, NULL);

 cancel_button = (Widget)XmMessageBoxGetChild(message_shell, XmDIALOG_CANCEL_BUTTON);
 XtSetArg(wargs[0], XmNwidth, 100);
 XtSetValues(cancel_button, wargs, 1);


 ok_button = (Widget)XmMessageBoxGetChild(message_shell, XmDIALOG_OK_BUTTON);
 help_button = (Widget)XmMessageBoxGetChild(message_shell, XmDIALOG_HELP_BUTTON);
 XtUnmanageChild(ok_button);
 XtUnmanageChild(help_button);

 return(message_shell);

}





/* ****************************************************************************

	 map_HelpDialog()

   **************************************************************************** */

void map_HelpDialog(w, shell, call_data)
	Widget          w;
	Widget          shell;
	XtPointer       *call_data;
{

	XWindowAttributes       attributes;

 if(XtIsManaged(shell))
	{
	XGetWindowAttributes(XtDisplay(w), XtWindowOfObject(shell), &attributes);
	if(attributes.map_state == IsUnmapped)  XtMapWidget(shell);
	else return;
	}
 else   XtManageChild(shell);


}


/* ****************************************************************************

	 unmap_HelpDialog()

   **************************************************************************** */

void unmap_HelpDialog(w, client_data, call_data)
	Widget          w;
	XtPointer       *client_data;
	XtPointer       *call_data;
{

 XtUnmanageChild(w);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/OnlineHelp/RCS/helpDialog.c,v $";
 static char rcs_id2[] = "$Id: helpDialog.c,v 1.2 2006/04/19 21:16:16 aivo Exp $";}
/*  ===================================================  */

}
