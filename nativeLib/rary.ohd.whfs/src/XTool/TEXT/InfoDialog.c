/*
	File:		InfoDialog.c
	Date:		12/21/1994
	Author:		Dale Shelton
	
	Purpose:	Provide the user with an XmDialog -
			in the form of a MessageBox with 
			XmDIALOG_INFORMATION semantics.
		
*/


#include "Xtools.h"


Widget 	InfoDialog(Widget w, char *msg)
{
	Widget		msgBox;
	Arg		arg[2];
	
	
	/*
		Set the resources for the dialog box.
	*/
	XtSetArg(arg[0], XmNtitle, "");
	XtSetArg(arg[1], XmNdialogType, XmDIALOG_INFORMATION);
	
	
	/*
		Create the dialog box.
	*/
	msgBox = XmCreateMessageDialog(w, "msgBox", arg, 2);		
	XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));


        /*
                Set the dialog message label.
        */
        XtSetArg(arg[0], XmNmessageString,
        	 XmStringCreateLtoR(msg,
        	 		    (XmStringCharSet)XmFONTLIST_DEFAULT_TAG));
        XtSetValues(msgBox, arg, 1);


        /*
                Manage the dialog box.
        */
        XtManageChild(msgBox);
        XtPopup(XtParent(msgBox), XtGrabNone);	
	return(msgBox);
}


