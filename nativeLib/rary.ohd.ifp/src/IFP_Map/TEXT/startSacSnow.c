/* ***************************************************************************************************
      startSacSnow()  activates script file to run java application for SAC and SNOW models
      Coded by:               Ai Vo
      Date:                   03/12/01

 *************************************************************************************************** */
#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#define COMMAND_LINE_LENGTH 80

void sacsnowMsgDialog(Widget , char *);

/* **************************************************************

	startSnow() calls script file runsnow and popups a window
		    message.


   ************************************************************** */
static 	Widget	msgBox;
void startSnow(w, someWidgets, call_data)
Widget                  w;
the_widget_struct       *someWidgets;
caddr_t                 *call_data;
	
{
  char      system_command_line[COMMAND_LINE_LENGTH];
  
  int 	    len, len2;
  void process_clicks();
  XtAppContext  app = XtWidgetToApplicationContext (w);
  static  XtIntervalId  id;
  
  memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
  len = strlen("ifp_scripts_dir");
  get_apps_defaults("ifp_scripts_dir", &len, system_command_line , &len2);
  strcat(system_command_line,"/runsnow&");
  /* printf("Runing  %s\n",system_command_line);*/
  printf("Running SNOW Display: %s \n",system_command_line);
  system(system_command_line);
  
  /* kill message dialog window after 8 seconds */
  id = XtAppAddTimeOut (app, 8000L, process_clicks, False);
  sacsnowMsgDialog(XtParent(w),"SNOW Display is coming up....Please wait");
 
}


/* **************************************************************

	startSac() calls script file runsac and popups a window
		    message.


   ************************************************************** */

void startSac(w, someWidgets, call_data)
Widget                  w;
the_widget_struct       *someWidgets;
caddr_t                 *call_data;
	
{
    
  char      system_command_line[COMMAND_LINE_LENGTH];
  int 	    len, len2;
  void process_clicks();
  XtAppContext  app = XtWidgetToApplicationContext (w);
  static  XtIntervalId  id;
 
  
  memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
  
  len = strlen("ifp_scripts_dir");
  get_apps_defaults("ifp_scripts_dir", &len, system_command_line , &len2);
  strcat(system_command_line,"/runsac&");
  printf("Running SAC Display: %s \n",system_command_line);
  system(system_command_line);
  /* kill message dialog window after 8 seconds */
  id = XtAppAddTimeOut (app, 8000L, process_clicks, False);
  sacsnowMsgDialog(XtParent(w),"SAC Display is coming up....Please wait");
  
  
  
}


void sacsnowMsgDialog(Widget widget, char *msg)

{
	/*static 	Widget	msgBox;*/
	Arg		arg[4];
	int		ac;
       
	
	if ( ! msgBox )
	{
		ac = 0;
		XtSetArg(arg[ac], XmNtitle, "SAC/SNOW MESSAGE"); ac++;
		XtSetArg(arg[ac], XmNdialogType, XmDIALOG_WORKING); ac++;
		XtSetArg(arg[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++; 
		msgBox = XmCreateWorkingDialog(widget, "msgBox", arg, ac);
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));

	}
	ac = 0;
	XtSetArg(arg[ac], XmNmessageString,
		 XmStringCreateLtoR(msg,
				    (XmStringCharSet)XmFONTLIST_DEFAULT_TAG)); ac++;
	XtSetValues(msgBox, arg, ac);
	
	XtManageChild(msgBox);
	XtPopup(XtParent(msgBox), XtGrabNone);
        

}

void process_clicks(w,client_data, id)
Widget w;
XtPointer client_data;
XtIntervalId  id;
{
    
    XtUnmanageChild(msgBox);
    

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/startSacSnow.c,v $";
 static char rcs_id2[] = "$Id: startSacSnow.c,v 1.3 2003/08/12 15:06:17 aivo Exp $";}
/*  ===================================================  */

}
