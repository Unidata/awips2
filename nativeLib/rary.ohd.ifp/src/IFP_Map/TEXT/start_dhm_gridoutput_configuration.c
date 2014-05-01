/* ******************************************************************************************
* File: start_dhm_gridoutput_configuration.c
* Author(s): DHM Team
* Date Created: 5/5/07
* Development group: OHD HSEB  
* Purpose: This file contains a set of routines that will call script
*         "run_gridoutput_configuration" to run an application to configure
*         grid output display. It also displays a motif message dialog to 
*         to let the user knows that a java gui will bring up shortly.
* Module(s):
*           dhm_gridoutput_configuration_MsgDialog()
*           process_dhm_gridoutput_configuration_clicks
*
 *********************************************************************************************/

#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#define COMMAND_LINE_LENGTH 80

void dhm_gridoutput_configuration_MsgDialog(Widget , char *);

/* **************************************************************

	start_dhm_gridoutput_configuration() calls script file run_gridoutput_configuration
        and popups a window message.


   ************************************************************** */
static 	Widget	msgBox;


void start_dhm_gridoutput_configuration(w, someWidgets, call_data)
Widget                  w;
the_widget_struct       *someWidgets;
caddr_t                 *call_data;
	
{
  char      system_command_line[COMMAND_LINE_LENGTH];
  
  int 	    len, len2;
  void process_dhm_gridoutput_configuration_clicks();
  XtAppContext  app = XtWidgetToApplicationContext (w);
  static  XtIntervalId  id;
  
  memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
  len = strlen("ifp_scripts_dir");
  get_apps_defaults("ifp_scripts_dir", &len, system_command_line , &len2);
  strcat(system_command_line,"/run_gridoutput_configuration&");
  system(system_command_line);
  
  /* kill message dialog window after 3 seconds */
  id = XtAppAddTimeOut (app, 3000L, process_dhm_gridoutput_configuration_clicks, False);
  dhm_gridoutput_configuration_MsgDialog(XtParent(w),
             "DHM Config Grids Output Display is coming up....Please wait");
 
}



/****************************************
Routine : dhm_gridoutput_configuration_MsgDialog().  Message Dialog to inform
          user that the Grid Output Configuration will appear.
input:  Widget name
        string character message

****************************************/
void dhm_gridoutput_configuration_MsgDialog(Widget widget, char *msg)

{
	/*static 	Widget	msgBox;*/
	Arg		arg[4];
	int		ac;       
	
	if ( ! msgBox )
	{
		ac = 0;
		XtSetArg(arg[ac], XmNtitle, "DHM CONFIG GRIDS OUTPUT MESSAGE"); ac++;
		XtSetArg(arg[ac], XmNdialogType, XmDIALOG_WORKING); ac++;
		XtSetArg(arg[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++; 
		msgBox = XmCreateWorkingDialog(widget, "msgBox", arg, ac);
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));

	}
	ac = 0;
	XtSetArg(arg[ac], XmNmessageString,
		 XmStringCreateLtoR(msg,
	        (XmStringCharSet)XmFONTLIST_DEFAULT_TAG)); 
	ac++;
	XtSetValues(msgBox, arg, ac);
	
	XtManageChild(msgBox);
	XtPopup(XtParent(msgBox), XtGrabNone);
        

}

/****************************************
Routine : process_dhm_gridoutput_configuration_clicks().  
         Handles Dialog- Ok is clicked.

****************************************/

void process_dhm_gridoutput_configuration_clicks(w,client_data, id)
Widget w;
XtPointer client_data;
XtIntervalId  id;
{
    
    XtUnmanageChild(msgBox);



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
