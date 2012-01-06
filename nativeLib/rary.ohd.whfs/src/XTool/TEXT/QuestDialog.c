/*
	File:		QuestionDialog.c
	Date:		11/02/1994
	Author:		Dale Shelton
	
	Purpose:	Provide the user with an XmDialog -
			in the form of a MessageBox with 
			XmDIALOG_QUESTION semantics.
		
*/


#include "Xtools.h"


Widget 	QuestionDialog(Widget w, char *msg)
{
	Widget	msgBox;
	Arg	arg[3];
	
	
	/*
		Set the resources for the dialog box.
	*/
	XtSetArg(arg[0], XmNtitle, "");
	XtSetArg(arg[1], XmNdialogType, XmDIALOG_QUESTION);
	XtSetArg(arg[2], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL);
		
		
	/*
		Create the dialog box.
	*/
	msgBox = XmCreateMessageDialog(w, "msgBox", arg, 3);		
	XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));


        /*
                Set the dialog message label.
                Use StringCreateLtoR to allow embedded newlines.
        */
        XtSetArg(arg[0], XmNmessageString,
        	 XmStringCreateLtoR(msg, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG));
        XtSetValues(msgBox, arg, 1);


        /*
                Manage the dialog box.
        */
        XtManageChild(msgBox);
        XtPopup(XtParent(msgBox), XtGrabNone);	
	return(msgBox);
}

Widget  SendDialog(Widget w, char *msg)
{
        Widget  msgBox;
        Arg     arg[3];
        char    button_label[40] = "Send";


        /*
                Set the resources for the dialog box.
                The title can be set by the calling function.
        */
        XtSetArg(arg[0], XmNtitle, "");
        XtSetArg(arg[1], XmNdialogType, XmDIALOG_QUESTION);
        XtSetArg(arg[2], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL);


        /*
                Create the dialog box.
        */
        msgBox = XmCreateMessageDialog(w, "msgBox", arg, 3);
        XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));


        /*
                Set the dialog message label, and set the ok button to Send,
                and set the default behavior to Cancel.
                Use StringCreateLtoR to allow embedded newlines.
        */
        XtSetArg(arg[0], XmNmessageString,
                 XmStringCreateLtoR(msg, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG));
        XtSetArg(arg[1], XmNokLabelString,
                 XmStringCreateLtoR(button_label, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG));
        XtSetArg(arg[2], XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON);
        XtSetValues(msgBox, arg, 3);


        /* color the text - WAS NOT ABLE TO GET THIS TO WORK */

        /*
        int     slen;
        Widget  childw;

        childw = XmMessageBoxGetChild(msgBox, XmDIALOG_MESSAGE_LABEL);
        printf("childw=%p\n", childw);

        printf("isLabel=%d\n", XmIsLabel(childw));

        slen = strlen("red") + 1;
        XtVaSetValues(childw, XtVaTypedArg, XmNforeground, XmRString, "red", slen, NULL);
        */

        /*
        XmNtopShadowColor, XmNbottomShadowColor, XmNborderColor no effect...
        slen = strlen("red") + 1;
        XtVaSetValues(msgBox, XtVaTypedArg, XmNforeground, XmRString, "red", slen, NULL);

        printf("isDialogShell=%d\n", XmIsDialogShell(msgBox));
        printf("isMessageBox=%d\n", XmIsMessageBox(msgBox));
        */


        /*
                Manage the dialog box.
        */
        XtManageChild(msgBox);
        XtPopup(XtParent(msgBox), XtGrabNone);
        return(msgBox);
}




void QuestionDialogWithCallbacks(Widget parent, char *message, char *title,
				 XtCallbackProc ok_callback_function,
				 XtCallbackProc cancel_callback_function)
{

   Widget	questionDialog,
      		okButton,
		cancelButton;
   
   
   questionDialog = QuestionDialog(parent, message);
   
   SetTitle(questionDialog, title);
   
   okButton = XmMessageBoxGetChild(questionDialog, XmDIALOG_OK_BUTTON);

   
   if (ok_callback_function)
   {
      okButton = XmMessageBoxGetChild(questionDialog,
				      XmDIALOG_OK_BUTTON);
      XtAddCallback(okButton, XmNactivateCallback,
		    ok_callback_function, questionDialog);
      
   }	
   
   
   if (cancel_callback_function)
   {      
      cancelButton = XmMessageBoxGetChild(questionDialog,
					  XmDIALOG_CANCEL_BUTTON);
      
      XtAddCallback(cancelButton, XmNactivateCallback,
		    cancel_callback_function, questionDialog);
      
   }	
   
   
   return;	
}


void	QuestionDialogCancel(Widget w, XtPointer ptr, XtPointer cbs)
{
	Widget	questionDialog = (Widget) ptr;
	   	
	
   	if (questionDialog)
	   XtDestroyWidget(questionDialog);
	
	
	
	
   	return;
}

