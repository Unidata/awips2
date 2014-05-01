/*
	File:		ErrorDialog.c
	Author:		Dale Shelton
	Date:		7/21/94
	
	Purpose:
	
*/


#include "Xtools.h"


Widget  ErrorDialog(Widget widget, char *msg)
{
	Widget		msgBox;
	Arg		arg[5];
	int		ac;
	
	
	/*
		Set the resources for the dialog box.
	*/
	ac = 0;
	XtSetArg(arg[ac], XmNtitle, "Error Dialog"); ac++;
	XtSetArg(arg[ac], XmNdialogType, XmDIALOG_ERROR); ac++;
	XtSetArg(arg[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++; 
	XtSetArg(arg[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
	XtSetArg(arg[ac], XmNnoResize, True); ac++;
	
		
	/*
		Create the dialog box.
	*/
	msgBox = XmCreateMessageDialog(widget, "msgBox", arg, ac);
	XtDestroyWidget(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
	XtDestroyWidget(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));
	

	/*
		Set the dialog message label.
	*/	
	ac = 0;
	XtSetArg(arg[ac], XmNmessageString,
		 XmStringCreateLtoR(msg,
				    (XmStringCharSet)XmFONTLIST_DEFAULT_TAG)); ac++;
	XtSetValues(msgBox, arg, ac);
	
	
	/*
		Manage the dialog box.
	*/			
	XtManageChild(msgBox);
	XtPopup(XtParent(msgBox), XtGrabNone);
	return(msgBox);	
}



