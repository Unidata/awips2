// ********************************************************************
// +++ 	tmcp.c: GUI for the workstation test/practice mode system
//
// Inputs:  This program is launched from the appLauncher menu
//
//
//  Overview of the workstation test/practice mode system:
//
//  The test mode control program, tmcp, provides the user
//  interface for changing the workstation mode.  The
//  permitted mode transitions are:
//      operational --> test
//	operational --> practice
//	test --> operational
//      practice --> operational
//
//  Tmcp can be launched from the command line or from the
//  appLauncher menu on the graphics workstation.  It cannot
//  be launched on the text workstation.  It takes no
//  arguments.
//
//  The tmcp GUI provides:
//  o  A status line indicating the workstation mode (test,
//     practice, or operational).
//  o  A message area.
//  o  Buttons for each mode.
//  o  A cancel button.
//
//  To change workstation mode, the user will start tmcp and click
//  the button for the desired mode.  The buttons for
//  unavailable modes are grayed out.
//
//  Certain preconditions must be met before tmcp will
//  change the workstation mode:
//  o  D-2D must not be running on either the text or
//     graphics workstations.
//  o  The same user must be logged in to both the text and
//     the graphics workstations.
//  o  The names of the associated graphics and text
//     workstations must have the same final digit.
//
//  If a precondition check fails a message to that effect
//  will appear in the message window.  The user can correct
//  the precondition and attempt the mode transition again.
//
//  The following discussion is phrased in terms of test
//  mode.  It applies equally to practice mode.
//
//  When the user clicks the "test" button to go into test
//  mode from operational mode, tmcp checks the
//  preconditions.  If they are not met, it displays an
//  appropriate message to the user and does not change
//  mode.
//
//  If the preconditions are met, tmcp changes the
//  workstation mode as requested.
//
//  There are two views of the workstation mode:
//  1.  The user view.  Whenever the workstation is in test
//      mode a blinking window appears on all
//	three graphics workstation displays and on the text
//	workstation display stating what the mode is.
//  2.  The application program view.  Whenever the
//      workstation is in test mode, the
//	/awips2/fxa/bin/getTestMode executable will return an
//	exit value characteristic of the mode.  Please see
//	the header comment in getTestMode.C for details.
//
//  To put the workstation into test mode, tmcp launches a
//  persistent background processes called tmb on the graphics
//  and text workstations.
//
//  It is the persistent tmb background processes, one
//  running on the text workstation and one on the graphics
//  workstation, that maintain the workstation mode.  The
//  /awips2/fxa/bin/getTestMode executable interrogates both
//  of the processes to determine the workstation mode.  So
//  tmb and getTestMode work together to maintain the
//  application program view of the mode.
//
//  After launching tmb, tmcp launches another pair of
//  persistent background processes called MonitorTestMode.
//  Once again, there is an instance of this process on the
//  graphics workstation and an instance on the text
//  workstation.  Each MonitorTestMode periodically checks
//  with both tmb instances to verify that both are running.
//  Also periodically, it starts up a shortlived X Windows
//  process called showBanner.
//
//  Showbanner displays the windows on the three graphics
//  displays and on the text workstation display to tell
//  the user what the workstation mode is.  ShowBanner exits
//  after a few seconds, and is then restarted tmb.  (That's
//  what makes the window blink, and ensures that it cannot
//  be covered by another application for more than a few
//  seconds.) So tmb, MonitorTestMode, and showBanner work
//  together to maintain the user view of the mode.
//
//  When the user wishes to return to operational mode from
//  test mode, he or she starts up tmcp again.  This time,
//  the user clicks the "operational" button.  Once again,
//  tmcp checks preconditions.  In this case the only
//  precondition is that D-2D must not be running.  If the
//  precondition test is successful, tmcp sends socket
//  messages to the local and remote tmb instances to tell
//  them to exit.
//
//  MonitorTestMode detects (by the absence of the tmb
//  process on both workstations) that the workstation is
//  back in operational mode.  At that point MonitorTestMode
//  exits.
//
//  If problems with the test mode programs are detected, or
//  if the test mode protocols are violated in any way, the
//  workstation goes into "panic" mode.  In panic mode a
//  banner is displayed to the user stating that there is a
//  problem and that he or she should immediately start up
//  tmcp and go back to operational mode.
//
//  When the workstation is in panic mode,
//  /awips2/fxa/bin/getTestMode returns a value that is not
//  correct for test, practice, or operational mode.  When
//  an application program gets that value it means that
//  there is no way of determining what mode the user
//  believes the workstation to be in.  The application must
//  take appropriate action.  (In most cases, the
//  application would probably display an error message on
//  its own GUI, if it has one, and then exit.)
//
//  The following are examples of situations where the
//  workstation will be in panic mode:
//  o  The user logs off one of the workstations (text or
//     graphics) but remains logged on to the other.
//  o  One of the two tmb processes dies.
//  o  One of the two MonitorTestMode processes dies.
//
//
// History:
// 23-dec-04 P. Wu   Initial
// 15-mar-05 P. Wu - SLC RFC requests that Xt (Text) workstation may not exist.
//		     In that case, the test mode program will still running on
//		     the graphic station alone (runLocal=1).
// 20-Jul-05 P. Wu - DR16442. Required to call showBanner_script to test
//                   display on Xt# workstation.
// 08-Aug-05 P. Wu - DR16521. Change the GUI message to handle the case of CPU
//                   resources problem.
//
// ---*************************************************************************
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/MainW.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <iostream>
#include "testmode.H"
#include "tmbUtil.H"
#include "hourglass.H"
#include "hourglassM.H"

#define DEF_HEIGHT          300    // For both max and min height
#define DEF_WIDTH           525    // For both max and min width
#define UPDATE_TIME         5000   // in milli-seconds or 5 second
#define FONT_NAME           "-*-helvetica-bold-r-normal--12-*"
#define RUN_TMB             "$FXA_HOME/bin/tmb"
#define RUN_TMB_EXIT        "$FXA_HOME/bin/tmb_exit"
#define RUN_GETTESTMODE     "$FXA_HOME/bin/getTestMode"
#define RUN_MONITOR_TM      "$FXA_HOME/bin/MonitorTestMode_script "
#define REMOTE_CHECK        "$FXA_HOME/bin/tmbRemoteCheck "

// To test if display is working for showBanner, we pass no realization flag
// and minimum time 1 second and dummy x and y positions at 10.
#define TEST_SHOWBANNER     "$FXA_HOME/bin/showBanner norealization 1 10 10 -display :0.0"
#define TEST_REMOTE_SHOWBANNER   "$FXA_HOME/bin/showBanner_script "

#define RES_CONVERT( res_name, res_value) \
        XtVaTypedArg, (res_name), XmRString, (res_value), strlen(res_value) + 1

Cursor  hourglass_pointer;
int  	Mode;       // Current mode
char    hostname[256];
char    txthostname[256];
int  	portNumber;
Widget  text_output;
Widget  pb_test, pb_practice, pb_operation, pb_cancel;
Widget  top_label;
Widget  toplevel;
Widget  rowcol;
Widget bform;
Widget rowcol2;
int  	runLocal = 0;       // run only local machine if = 1

// +++ 	Function: create_cursor
//Purpose: To create a hourly-glass curosr. This is used to tell user to wait.
// ---*************************************************************************
void create_cusor()
{
    XColor  color_defs[2];
    Pixmap  hourglass_pixmap;
    Pixmap  hourglass_mask_pixmap;
    Display *display;
    int     UxScreen;

    display = XtDisplay(toplevel);
    UxScreen =  XDefaultScreen(display);

    color_defs[0].pixel = BlackPixel(display, UxScreen);
    color_defs[1].pixel = WhitePixel(display, UxScreen);
    XQueryColors (XtDisplay(toplevel), DefaultColormap (display, UxScreen),
        color_defs, 2);

    hourglass_pixmap = XCreatePixmapFromBitmapData (display,
        DefaultRootWindow (display), hourglass_bits, hourglass_width,
	hourglass_height, WhitePixel (display, UxScreen),
	BlackPixel (display, UxScreen), 1);
    hourglass_mask_pixmap =
        XCreatePixmapFromBitmapData (display, DefaultRootWindow(display),
        hourglass_mask_bits, hourglass_mask_width, hourglass_mask_height,
	WhitePixel (display, UxScreen), BlackPixel (display, UxScreen), 1);
    hourglass_pointer = XCreatePixmapCursor (display, hourglass_pixmap,
        hourglass_mask_pixmap, &color_defs[0], &color_defs[1],
        hourglass_x_hot, hourglass_y_hot);

}

// +++ 	Function: check_mode
//Purpose: This routine is called at startup time.
//       It requests getTestMode program to return the current mode status.
//       It then updates the mode on the top label widget of the window to
//       let user knows the mode status.
// ---*************************************************************************
void check_mode()
{
    int rc;
    char command[512];
    XmString label;
    XmString button_label;

    /* Check to see if TMB is already running */
    sprintf(command,RUN_GETTESTMODE);
    rc = system(command)>>8;
    std::cout << "Checking for a running tmb: getTestMode returns " <<
        rc << std::endl;

    Mode = rc;

    if (rc != OPERATION_MODE)
    {
        // Determine if we are in test mode or practice mode
        // Set buttons sensitivity accordingly
	if (rc == PANIC_MODE)
        {
           label = XmStringCreateLocalized("Mode unknown."
                          " Reset to operational mode immediately.");
           XtVaSetValues(top_label, XmNlabelString,label, NULL);
        }
        else if (rc == TEST_MODE)
        {
           label = XmStringCreateLocalized(
	           "TEST Mode (Comms Live)");
           XtVaSetValues(top_label, XmNlabelString,label, NULL);

           button_label = XmStringCreateLocalized(
           "  You are in Mode \nTest (Comms Live) ");
           XtVaSetValues(pb_test, XmNlabelString,button_label, NULL);
        }
        else if (rc == PRACTICE_MODE)
        {
           label = XmStringCreateLocalized(
	   "PRACTICE Mode (In House)");
           XtVaSetValues(top_label, XmNlabelString,label, NULL);

           button_label = XmStringCreateLocalized(
           " You are in Mode \nPractice (In House)");
           XtVaSetValues(pb_practice, XmNlabelString,button_label, NULL);
        }

        XtVaSetValues(pb_test, XmNsensitive, FALSE, NULL);
        XtVaSetValues(pb_practice, XmNsensitive, FALSE, NULL);
        button_label = XmStringCreateLocalized(
           "  Change Mode to \n    Operational    ");
        XtVaSetValues(pb_operation, XmNlabelString,button_label, NULL);
    }
    else
    {
           XtVaSetValues(pb_test, XmNsensitive, TRUE, NULL);
           XtVaSetValues(pb_practice, XmNsensitive, TRUE, NULL);
           XtVaSetValues(pb_operation, XmNsensitive, FALSE, NULL);
    }
}

// +++ 	Function: UxConvertFontList
//
//Purpose: An interface routine to invoke Xt resource converters for the fonts.
// ---*************************************************************************
XmFontList   UxConvertFontList(char *fontlist_str)
{
        XrmValue        from, to;
        XmFontList      fontlist = NULL;
        Boolean         status;

        from.size = strlen( fontlist_str ) + 1;
        from.addr = fontlist_str;

        to.size = sizeof(XmFontList);
        to.addr = (caddr_t) &fontlist;

        status = XtConvertAndStore( text_output, XmRString, &from,
                                    XmRFontList, &to );
        return ( fontlist );
}

// +++ 	Function: insert_text
//
//Purpose: This routine allows status text information to be inserted and
//         displayed on the scroll window.
// ---*************************************************************************
void insert_text(Widget status, char *message)
{
    XtUnmanageChild (rowcol2);
    XtUnmanageChild (bform);
    XtUnmanageChild (rowcol);

	XmTextPosition curpos;
	curpos = XmTextGetInsertionPosition(status);
	XmTextInsert(status, curpos, message);
	curpos = curpos +strlen(message);
	XmTextShowPosition(status, curpos);
	XmTextSetInsertionPosition(status, curpos);

	XtManageChild (rowcol2);
    XtManageChild (bform);
	XtManageChild (rowcol);
}

// +++ 	Function: request_tmbs_to_exit
//
//Purpose: An interface to request both local and remote TMB programs to exit.
// ---*************************************************************************
void request_tmbs_to_exit(char *hostname, char *txthostname,int portNumber,
             int mode)
{
    int rc;
    char 	command[512];
    char 	lcommand[512];

    sprintf(lcommand,"%s %s %d & ",RUN_TMB_EXIT,hostname,portNumber);
    if (mode == PANIC_MODE) {
        sprintf(command,"%s/bin/tmb",getenv("FXA_HOME"));
        rc = findprocess(command);
        if (rc > 0) {
            rc = system(lcommand)>>8;
            std::cout << "First invocation of tmb_exit" << std::endl;
        }
    }
    else {
        rc = system(lcommand)>>8;
        std::cout << "First invocation of tmb_exit" << std::endl;
    }


    sprintf(lcommand,"%s %s %d & ",RUN_TMB_EXIT, txthostname,portNumber);

    if (!runLocal) {
        if (mode == PANIC_MODE) {
            sprintf(command,"ssh -x %s %s ",txthostname, RUN_TMB_EXIT);
            rc = system(command)>>8;
            if (rc == 1) {
                rc = system(lcommand)>>8;
                std::cout << "Second invocation of tmb_exit" << std::endl;
            }
        }
        else {
                rc = system(lcommand)>>8;
                std::cout << "Second invocation of tmb_exit" << std::endl;
        }
    }
}

// +++ 	Function: check_mode_ready_to_go
//Purpose: This routine is called to check if the tmb and Monitor programs
//	 are ready or not by calling getTestMode to check the mode status.
// ---*************************************************************************
int check_mode_ready_to_go(int mode)
{
    int rc = OPERATION_MODE;
    char command[512];
    int count = 1;
    while (rc != mode) {
	// Wait a little bit before checking the status
	sleep(1);
    	sprintf(command,RUN_GETTESTMODE);
    	rc = system(command)>>8;
	if (count > 10) break;
	count++;
    }

    if (rc != mode) {
        insert_text(text_output,"Warning! Unable to set mode. Please check system resources.\n\n");
        insert_text(text_output,"There may be processes hogging the CPU on both lx and xt machines.\n\n");
	XmUpdateDisplay(text_output);
        request_tmbs_to_exit(hostname, txthostname, portNumber, OPERATION_MODE);
    }
    return rc;

}

// +++ 	Function: check_grph_txt_wrkstn
//
//Purpose: This routine checks the text workstation number against the number
//          obtained from the "FXA_WARNGEN_PRODUCT_ID". If they don't match,
//          it notifies user the mismatch through the text window.
// ---*************************************************************************
int check_grph_txt_wrkstn(char *textWrkStnName)
{
    char wrkwgEnv[256];
    char msg[512];
    int last;

    // Condition 1- Is the Text workstation number match with the lx#s
    sprintf(wrkwgEnv,"%s",getenv("FXA_WARNGEN_PRODUCT_ID"));

    last = strlen(wrkwgEnv);
    // if (wrkwgEnv[5] != hostname[2])
    if (wrkwgEnv[last-1] != hostname[2])
    {
        // XBell(XtDisplay(toplevel) , 100);
        insert_text(text_output,"Mismatch of Graphic & Text workstation numbers.\n");
	sprintf(msg,"Graphic workstation=%s and Text workstation=%s \n",
            hostname,wrkwgEnv);
        insert_text(text_output,msg);
        return FAIL;
    }
   return PASS;
}

// +++ 	Function: check_local_processes
//
//Purpose: This routine checks the local workstation  to see if fxaWish and
//         others are running or not. It will notify user to quit if any of
//         those applications is still running.
// 10-Feb-05 P. Wu	Add textWish check
// ---*************************************************************************
int check_local_processes()
{
    int rc;
    char msg[512];

    rc = findprocess("fxaWish");
    // Remind the user to quit D-2D first by sending to msg window
    if (rc > 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found D-2D is still running on the graphic "
                   "workstation %s. Please exit D-2D first.\n\n",hostname);
     	   insert_text(text_output,msg);
	   return FAIL;
	}

    // Check for CAVE
    rc = findprocess("./cave");
    if (rc > 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found CAVE is still running on the graphic "
                   "workstation %s. Please exit CAVE first.\n\n",hostname);
     	   insert_text(text_output,msg);
	   return FAIL;
	}

    // Check for warnGenWish
    rc = findprocess("warnGenWish");
    // Remind the user to quit warnGenWish first by sending to msg window
    if (rc > 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found warnGenWish is still running on the graphic "
                   "workstation %s. Please exit warnGenWish first.\n\n",hostname);
     	   insert_text(text_output,msg);
	   return FAIL;
	}
    // Check for WWA
    rc = findprocess("wwa");
    // Remind the user to quit wwa first by sending to msg window
    if (rc > 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found wwa is still running on the graphic "
                   "workstation %s. Please exit wwa first.\n\n",hostname);
     	   insert_text(text_output,msg);
	   return FAIL;
	}
    // Check for RiverPro
    rc = findprocess("/awips/hydroapps/whfs/bin/rpf.LX");
    // Remind the user to quit wwa first by sending to msg window
    if (rc > 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found RiverPro is still running on the graphic "
                   "workstation %s. Please exit RiverPro first.\n\n",hostname);
     	   insert_text(text_output,msg);
	   return FAIL;
	}
    // Check for GFE
    rc = findprocess("/awips/GFESuite/bin/run/gfe");
    // Remind the user to quit wwa first by sending to msg window
    if (rc > 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found GFE is still running on the graphic "
                   "workstation %s. Please exit GFE first.\n\n",hostname);
     	   insert_text(text_output,msg);
	   return FAIL;
	}
    // Check for textWish
    rc = findprocess("textWish");
    // Remind the user to quit textWish first by sending to msg window
    if (rc > 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found TextWS or expiration notice is still active "
                   "on the graphic workstation %s."
                   "Please exit the TextWS or remove the "
                   "expiration notice first.\n\n",hostname);
     	   insert_text(text_output,msg);
	   return FAIL;
	}

    return PASS;
}

// +++ 	Function: check_remote_processes
//
//Purpose: This routine checks if D-2D or fxaWish is currently running in the
//          text workstation or not. If it founds D-2D running, it will notify
//          user to quit the program.
// 10-Feb-05 P. Wu	Add textWish check
// ---*************************************************************************
int check_remote_processes(char *textWrkStnName)
{
    char command[512];
    char msg[512];
    int rc;
    sprintf(msg,"Checking preconditions for %s ... \n\n",textWrkStnName);
    insert_text(text_output,msg);
    XmUpdateDisplay(text_output);

    sprintf(command,"ssh -x %s %s ",textWrkStnName, REMOTE_CHECK);
    rc = system(command)>>8;

    if (rc == 1)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found D-2D is still running on the text "
                   "workstation %s. Please exit D-2D first.\n\n",textWrkStnName);
     	   insert_text(text_output,msg);
	   return FAIL;
	}

    if (rc == 2)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found warnGenWish is still running on the text "
                   "workstation %s. Please exit warnGenWish first.\n\n",
                   textWrkStnName);
     	   insert_text(text_output,msg);
	   return FAIL;
	}

    if (rc == 3)
	{
           // XBell(XtDisplay(toplevel) , 100);
     	   sprintf(msg,"Found WWA is still running on Text "
                   "workstation %s. Please exit WWA first.\n\n",
                   textWrkStnName);
     	   insert_text(text_output,msg);
	   return FAIL;
	}

    if (rc == 4)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found RiverPro is still running on the text "
                   "workstation %s. Please exit RiverPro first.\n\n",
                   textWrkStnName);
     	   insert_text(text_output,msg);
	   return FAIL;
	}
    if (rc == 5)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found GFE is still running on the text "
                   "workstation %s. Please exit GFE first.\n\n",
                   textWrkStnName);
     	   insert_text(text_output,msg);
	   return FAIL;
	}
    if (rc == 6)
	{
           // XBell(XtDisplay(toplevel) , 100);
	   sprintf(msg,"Found TextWS or expiration notice is still active "
                   "on the text workstation %s."
                   "Please exit the TextWS or remove the "
                   "expiration notice first.\n\n",textWrkStnName);
     	   insert_text(text_output,msg);
	   return FAIL;
	}
    if (rc == 7)
    {
 	   sprintf(msg,"Found CAVE is still running on the text "
                    "workstation %s. Please exit CAVE first.\n\n",textWrkStnName);
      	   insert_text(text_output,msg);
 	   return FAIL;
    }
   return PASS;
}

// +++ 	Function: check_local_desktops
//
//Purpose: This driver routine checks the number of desktops (KDE) or
//         workspace (GNOME) on local graphic workstation.
// ---*************************************************************************
int check_local_desktops()
{
    char msg[1024];
    int rc;

    rc = check_desktops_or_workspace();

    if (rc != 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
           sprintf(msg,"Found more than one desktop or workspace in your setup.\n"
		  "You need to run Control Center program, kcontrol to set it back to \n"
                  "1 for each of the 3 monitors in order to continue. \n"
		  "This is also true and applied to the Text workstation. \n\n");
     	   insert_text(text_output, msg);
	   return FAIL;
	}
   return PASS;
}


// +++ 	Function: check_local_display
//
//Purpose: This routine calls showBanner program to run on graphic workstation
//          without widget realization in order to check the local display.
//          It will notify user if there is any display problem.
// ---*************************************************************************
int check_local_display()
{
    char msg[512];
    char command[512];
    int rc;
    sprintf(msg,"Checking display for %s ... \n\n",hostname);
    insert_text(text_output,msg);
    XmUpdateDisplay(text_output);
    sprintf(command,TEST_SHOWBANNER);
    rc = system(command)>>8;
    if (rc != 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
           sprintf(msg,"Problem with display on %s. Please check. \n\n",
                           hostname);
     	   insert_text(text_output, msg);
	   return FAIL;
	}
   return PASS;
}

// +++ 	Function: check_remote_display
//
//Purpose: This routine calls showBanner program to run on text workstation
//          without widget realization in order to check the local display.
//          It will notify user if there is any display problem.
// ---*************************************************************************
int check_remote_display(char *textWrkStnName)
{
    char msg[512];
    char command[512];
    int rc;
    sprintf(msg,"Checking display for %s ... \n\n",textWrkStnName);
    insert_text(text_output,msg);
    XmUpdateDisplay(text_output);
    sprintf(command,"ssh -x %s %s %s norealization 1 10 10 -display :0.0",
            textWrkStnName, TEST_REMOTE_SHOWBANNER, textWrkStnName);

    // sprintf(command,"ssh -x %s %s ",textWrkStnName, TEST_SHOWBANNER);
    rc = system(command)>>8;
    if (rc != 0)
	{
           // XBell(XtDisplay(toplevel) , 100);
           sprintf(msg,"Problem with display on %s. Please check. \n\n",
                           textWrkStnName);
     	   insert_text(text_output, msg);
	   return FAIL;
	}
   return PASS;
}

// +++ 	Function: checkPreCond_Op2Test
//
//Purpose: This routine is called when user selects from operation mode to
//          test mode. There are several pre-conditions that must be met
//          checked in order to transition to another mode.
// ---*************************************************************************
int checkPreCond_Op2Test(char *textWrkStnName)
{
    int rc;
    char msg[512];

    insert_text(text_output,"Please wait... checking preconditions.\n\n");
    XmUpdateDisplay(text_output);

    // Remote conditions checks:
    if (!runLocal) {
        if (check_grph_txt_wrkstn(textWrkStnName)== FAIL)
            return FAIL;
        // Condition 3- Is D-2D or fxaWish running on text WS?
        if (check_remote_processes(textWrkStnName)==FAIL)
         return FAIL;
        // Remind the user to logon to text workstation in the msg window
        rc = check_remote_login(textWrkStnName);

        if (rc == 0) {
           // XBell(XtDisplay(toplevel) , 100);
           sprintf(msg,"Please logon to text workstation %s"
                  " under your user account first!\n\n", textWrkStnName);
           insert_text(text_output,msg);
	   return FAIL;
        }
        // Condition -  X-Display accessible on txt WS?
        insert_text(text_output,"Checking remote displays\n\n");
        XmUpdateDisplay(text_output);
        if (check_remote_display(textWrkStnName)==FAIL)
            return FAIL;
    }

    // Condition 2- Is D-2D or fxaWish running here on graphics WS?
    if(check_local_processes()==FAIL)
        return FAIL;

    // Condition -  X-Display accessible on graphics WS?
    insert_text(text_output,"Please wait... checking displays\n\n");
    XmUpdateDisplay(text_output);
    if (check_local_display()==FAIL)
        return FAIL;

    // Condition - multiple desktops or workspace
    if (check_local_desktops()==FAIL)
        return FAIL;

   return PASS;
}

// +++ 	Function: checkPreCond_Op2Prac
//
//Purpose: This routine is called when user selects from operation mode to
//          practice mode. There are several pre-conditions that must be met
//          checked in order to transition to another mode.
// ---*************************************************************************
int checkPreCond_Op2Prac(char *textWrkStnName)
{
    char msg[512];
    int rc;
    insert_text(text_output,"Please wait... checking preconditions\n\n");
    XmUpdateDisplay(text_output);

    // Remote conditions checks:
    if (!runLocal) {
        if (check_grph_txt_wrkstn(textWrkStnName)== FAIL)
            return FAIL;
        // Condition 3- Is D-2D or fxaWish running on text WS?
        if (check_remote_processes(textWrkStnName)==FAIL)
         return FAIL;
        // Remind the user to logon to text workstation in the msg window
        rc = check_remote_login(textWrkStnName);

        if (rc == 0) {
           // XBell(XtDisplay(toplevel) , 100);
           sprintf(msg,"Please logon to text workstation %s"
                  " under your user account first!\n\n", textWrkStnName);
           insert_text(text_output,msg);
	   return FAIL;
        }
        // Condition -  X-Display accessible on txt WS?
        insert_text(text_output,"Checking remote displays\n\n");
        XmUpdateDisplay(text_output);
        if (check_remote_display(textWrkStnName)==FAIL)
            return FAIL;
    }

    // Condition 2- Is D-2D or fxaWish running here on graphics WS?
    if(check_local_processes()==FAIL)
        return FAIL;

    // Condition -  X-Display accessible on graphics WS?
    insert_text(text_output,"Please wait... checking displays\n\n");
    XmUpdateDisplay(text_output);
    if (check_local_display()==FAIL)
        return FAIL;

    // Condition - multiple desktops or workspace
    if (check_local_desktops()==FAIL)
        return FAIL;

   return PASS;
}

// +++ 	Function: checkPreCond_Test2Op
//
//Purpose: This routine is called when user selects from test mode to
//          operation mode. There are several pre-conditions that must be met
//          checked in order to transition to another mode.
// ---*************************************************************************
int checkPreCond_Test2Op(char *textWrkStnName)
{
    // Required condition - Is D-2D or fxaWish running?
    if(check_local_processes()==FAIL)
        return FAIL;

    if (!runLocal) {
        if (check_remote_processes(textWrkStnName)==FAIL)
            return FAIL;
    }

    return PASS;
}

// +++ 	Function: checkPreCond_Prac2Op
//
//Purpose: This routine is called when user selects from practice mode to
//          operation mode. There are several pre-conditions that must be met
//          checked in order to transition to another mode.
// ---*************************************************************************
int checkPreCond_Prac2Op(char *textWrkStnName)
{
    // Required condition - Is D-2D or fxaWish running?
    if(check_local_processes()==FAIL)
        return FAIL;

    if (!runLocal) {
        if (check_remote_processes(textWrkStnName)==FAIL)
            return FAIL;
    }

    return PASS;
}

// +++ 	Function: checkPreCond_Panic2Op
//
//Purpose: This routine is called when user selects from panic mode to
//          operation mode. There are several pre-conditions that must be met
//          checked in order to transition to another mode.
// ---*************************************************************************
int checkPreCond_Panic2Op(char *textWrkStnName)
{
    // Required condition - Is D-2D or fxaWish running?
    if(check_local_processes()==FAIL)
        return FAIL;

    if (!runLocal) {
        if (check_remote_processes(textWrkStnName)==FAIL)
            return FAIL;
    }

    return PASS;
}

// +++ 	Function: x_error
//
//Purpose: A routine reports any X-Window related errors.
// ---*************************************************************************
static int x_error(Display *dpy, XErrorEvent * err_event)
{
    char                buf[256];
    XGetErrorText (dpy, err_event->error_code, buf, (sizeof buf));
    std::cout << "XGetErrorText returned error text: " << buf << std::endl;
    return 0;
}

// +++ 	Function: xt_error
//
//Purpose: A dummy routine reports any Xt-related errors.
// ---*************************************************************************
static void xt_error(char *message)
{
}

// +++ 	Function: events_handler
//
//Purpose: This routine is the callbacks or response to the user-interface
//          when user selects the buttons to change the testing modes as well
//          as the exit button.
// ---*************************************************************************
static void events_handler( Widget w, XtPointer client_data, XtPointer call_data)
{
    int rc;
    int	pc;  // pre-condition flag
    int which = (long) client_data;
    XmString label;
    XmString button_label;
    char 	command[512];

    switch (which) {
	case TEST_MODE :
            XDefineCursor (XtDisplay(toplevel), XtWindow (toplevel),
	        hourglass_pointer);
            XFlush(XtDisplay(toplevel));
            pc = checkPreCond_Op2Test(txthostname);
            if (pc)
            {
                sprintf(command,"%s %d %d %s %d & ", RUN_TMB,
                    portNumber,TEST_MODE, txthostname, runLocal);
                rc = system(command)>>8;

                if (!runLocal) {
                    sprintf(command,"ssh -x %s %s %d %d %s %d & ",
                    txthostname,RUN_TMB,portNumber,TEST_MODE, hostname,
                    runLocal);
                    rc = system(command)>>8;
                }

                std::cout << "Entering TEST_MODE" << std::endl;

                // Launch the monitor tmb programs
                sprintf(command,"%s &",RUN_MONITOR_TM);
                rc = system(command)>>8;

                if (!runLocal) {
                    sprintf(command, "ssh -x %s %s &",txthostname,
                                    RUN_MONITOR_TM);
                    rc = system(command)>>8;
                }

		rc = check_mode_ready_to_go(TEST_MODE);

		if (rc == TEST_MODE) {
                    XtVaSetValues(pb_test, XmNsensitive, FALSE, NULL);
                    XtVaSetValues(pb_practice, XmNsensitive, FALSE, NULL);
                    XtVaSetValues(pb_operation, XmNsensitive, TRUE, NULL);
                    label = XmStringCreateLocalized( "Test mode (Comms Live)");
                    XtVaSetValues(top_label, XmNlabelString,label, NULL);

         		insert_text(text_output,
		    	"Start Test Mode (Comms Live) and this program will exit.\n\n");
                	XmUpdateDisplay(text_output);
                	// Wait a litte bit before exiting
                	sleep(5);
                	exit(0);
		}
            }
            XUndefineCursor (XtDisplay(toplevel), XtWindow(toplevel));
	    break;
	case PRACTICE_MODE :
                XDefineCursor (XtDisplay(toplevel), XtWindow (toplevel),
		    hourglass_pointer);
                XFlush(XtDisplay(toplevel));
                /* If currently under operation mode, need to check the
		   2 preconditions */
                pc = checkPreCond_Op2Prac(txthostname);
		if (pc)
		{
    			sprintf(command,"%s %d %d %s %d &",RUN_TMB,
			    portNumber,PRACTICE_MODE, txthostname, runLocal);
    			rc = system(command)>>8;
                        if (!runLocal) {
                        sprintf(command,"ssh -x %s %s %d %d %s %d & ", txthostname,
                                RUN_TMB, portNumber,PRACTICE_MODE, hostname, runLocal);
                        rc = system(command)>>8;
                        }
			std::cout << "Entering PRACTICE_MODE" << std::endl;

                        // Launch the monitor tmb programs
    			sprintf(command,"%s &",RUN_MONITOR_TM);
    			rc = system(command)>>8;
                        if (!runLocal) {
    			sprintf(command, "ssh -x %s %s &",txthostname,RUN_MONITOR_TM);
    			rc = system(command)>>8;
                        }

			rc = check_mode_ready_to_go(PRACTICE_MODE);

			if (rc == PRACTICE_MODE) {
                  	        XtVaSetValues(pb_practice, XmNsensitive, FALSE, NULL);
                  	        XtVaSetValues(pb_test, XmNsensitive, FALSE, NULL);
                  	        XtVaSetValues(pb_operation, XmNsensitive, TRUE, NULL);
                                label = XmStringCreateLocalized ("Practice Mode (In House)");
                                XtVaSetValues(top_label, XmNlabelString,label, NULL);
         			insert_text(text_output,
		    		"Start Practice Mode (In house) and this program will exit.\n\n");
                		XmUpdateDisplay(text_output);
                		// Wait a litte bit before exiting
                		sleep(5);
                		exit(0);
			}
		}
                XUndefineCursor (XtDisplay(toplevel), XtWindow(toplevel));
	    break;
	case OPERATION_MODE :
                XDefineCursor (XtDisplay(toplevel), XtWindow (toplevel),
		    hourglass_pointer);
                XFlush(XtDisplay(toplevel));
                if (Mode == TEST_MODE)
                    pc = checkPreCond_Test2Op(txthostname);
                else if (Mode == PRACTICE_MODE)
                    pc = checkPreCond_Prac2Op(txthostname);
                else
                    pc = checkPreCond_Panic2Op(txthostname);

		if (pc)
		{
                    insert_text(text_output,"Please wait... \n\n");
                    XDefineCursor (XtDisplay(toplevel),
			    XtWindow (toplevel), hourglass_pointer);
                    XFlush(XtDisplay(toplevel));

                    request_tmbs_to_exit(hostname, txthostname, portNumber,
                                    Mode);
		    std::cout << "Entering operational mode." << std::endl;
		    // Wait for all the processes to exit
                    sleep(3);

		    // Check and make sure all processes are indeed gone
		    // check_cleanup();

		    XtVaSetValues(pb_test, XmNsensitive, TRUE, NULL);
                    XtVaSetValues(pb_practice, XmNsensitive, TRUE, NULL);
                    XtVaSetValues(pb_operation, XmNsensitive, FALSE, NULL);
                    label = XmStringCreateLocalized (
		        "Operational Mode");
                    XtVaSetValues(top_label, XmNlabelString,label, NULL);
                    insert_text(text_output,
		        "You are now back in operational mode.\n\n");

                    button_label = XmStringCreateLocalized(
                    "  Change Mode to \nTest (Comms Live) ");
                    XtVaSetValues(pb_test, XmNlabelString,button_label, NULL);
                    button_label = XmStringCreateLocalized(
                    "  Change Mode to \nPractice (In House)");
                    XtVaSetValues(pb_practice, XmNlabelString,button_label, NULL);

		}
                XUndefineCursor (XtDisplay(toplevel), XtWindow(toplevel));
	        XtError ("This is a Set UP call!");
	    break;
	case EXIT_MODE :
	        XtWarning ("This is a Cancel call.");
	        exit(0);
	    break;
    }
}

// +++ 	Function: startup_check
//
//Purpose:  The purpose of this routine is to see if there are any socket
//          problems arise at startup. By launching getTestMode, we can tell
//          if it went thru or not. After 1 second or so, and if getTestMode
//          is still running, we need to call tmb_exit to clean things up.
//          Note that we may not need this routine since the later design
//          changes may solve the early problem that we have (TBD).
// ---*************************************************************************
void startup_check()
{
    int rc;
    char command[512];

    // Check to see if anything hangs
    sprintf(command,"%s & ",RUN_GETTESTMODE);
    rc = system(command)>>8;
    sleep(1);

    sprintf(command,"%s ",RUN_GETTESTMODE);
    rc = findprocess(command);
    if (rc > 0)
        request_tmbs_to_exit(hostname, txthostname, portNumber, Mode);
}

// +++ 	Function: check_file
//
//Purpose:  The purpose of this routine is to see if a specified fiel
//          is presented or not.
// ---*************************************************************************
int check_file(char *fileName)
{
    struct stat buf;
    char command[512];
    int i;

    i = stat(fileName, &buf);
    if (i!=0) {
        sprintf(command,"xmessage -buttons OK:OK 'Executable or "
                       "script- %s is missing'",fileName);
        system(command);
	std::cout << "File-" << fileName << " is missing." <<
	    std::endl;
        return 1;
    }
    return 0;
}

// +++ 	Function: check_all_files
//
//Purpose:  The purpose of this routine is to see if all required scripts
//          and executables are presented or not.
// ---*************************************************************************
int check_all_files()
{
    char fxa_home[512];
    char command[1024];
    int rc = 0;
    sprintf(fxa_home,"%s",getenv("FXA_HOME"));
    sprintf(command,"%s/bin/MonitorTestMode_script",fxa_home);
    rc += check_file(command);
    sprintf(command,"%s/bin/MonitorTestMode",fxa_home);
    rc += check_file(command);
    sprintf(command,"%s/bin/showBanner_script",fxa_home);
    rc += check_file(command);
    sprintf(command,"%s/bin/showBanner",fxa_home);
    rc += check_file(command);
    sprintf(command,"%s/bin/tmb",fxa_home);
    rc += check_file(command);
    sprintf(command,"%s/bin/tmb_exit",fxa_home);
    rc += check_file(command);
    sprintf(command,"%s/bin/tmbRemoteCheck",fxa_home);
    rc += check_file(command);
    sprintf(command,"%s/bin/consoleUser",fxa_home);
    rc += check_file(command);
    return rc;
}


// +++ 	Function: main
//
//Purpose:  This is the main driver of this GUI program which is nothing more
//          than have a label widget which reports the latest mode status. And
//          a scroll text-window that communicate with the user on any problem.
//          Below the text-wind, there are 4 buttons:
//          (1) Test mode - set test mode
//          (2) Practice mode - set practice mode
//          (3) Operation mode - not in any test or practice mode
//          (4) Cancel - exit the program.
//
//          Most of the code are dealing with GUI or widget and the callbacks.
// ---*************************************************************************
int main(int argc, char *argv[])
{
    XtAppContext app;
    Arg          args[10];
    int          n;
    int 	 ck;
    int 	 len;
    char 	 command[512];
    int 	 rc;

    XmFontList fontlist;
    XmFontListEntry entry;
    XFontStruct *font;
    Display *dpy;

    std::cout << "Startup" << std::endl;

    sprintf(command,"%s/bin/tmcp",getenv("FXA_HOME"));

    // Check if another copy of tmb is already running or not
    if (findprocess(command)>0)
    {
        sprintf(command,"xmessage -buttons OK:OK 'TMCP is already running'");
        system(command);
	std::cout << "Another copy of tmcp is already running.  Exiting." <<
	    std::endl;
	exit(0);
    }

    // This program is intended to run on graphic workstation only
    // Check for the lx# first before proceed
    gethostname(hostname, 256);

    // May Need To Update This Logic; it should not be possible to run
    // tmcp on text workstations.
    /*
    if (strncmp(hostname,"xt",2)=0) {
        sprintf(command,"xmessage -buttons OK:OK 'TMCP is intended to run on "
                        "graphical workstations only.'");
        system(command);
	exit(0);
    }
    */

    // Process input arguments: port number
    // Need to find out what is the hostname of the corresponding text
    // workstation
    len=strlen(hostname);
    strcpy(txthostname,"xt");
    memcpy(&txthostname[2],&hostname[2],len-2);

    // Check if the text workstation is present or not
    sprintf(command,"ping -c 1 -w 1 %s >/dev/null 2>/dev/null",txthostname);
    rc = system(command);
    if (rc != 0)
            runLocal = 1;

    // Should get the port number from the command line argument
    portNumber = atoi(DEFAULT_PORT_NUM);

    // Check to see if any socket problem
    startup_check();

    // Check if all files are installed
    ck = check_all_files();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "TMCP",
	NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget ("rowcol",
	xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmVERTICAL, NULL);

    dpy = XtDisplay (toplevel);
    font = XLoadQueryFont (dpy, "-*-helvetica-bold-r-normal-*-180-75-75-*");
    entry = XmFontListEntryCreate ("tag1", XmFONT_IS_FONT, font);
    fontlist = XmFontListAppendEntry (NULL, entry);
    font = XLoadQueryFont (dpy, "-*-helvetica-bold-r-normal-*-180-75-75-*");
    entry = XmFontListEntryCreate ("tag2", XmFONT_IS_FONT, font);
    fontlist = XmFontListAppendEntry (fontlist, entry);
    XtFree ((char *) entry);
    top_label = XtVaCreateManagedWidget ("Operational Mode",
        xmLabelWidgetClass, rowcol, XmNalignment,   XmALIGNMENT_BEGINNING,
        XmNfontList, fontlist, NULL);

    // Create output_text as a ScrolledText window
    n = 0;
    XtSetArg(args[n], XmNrows,             15); n++;
    XtSetArg(args[n], XmNcolumns,          80); n++;
    XtSetArg(args[n], XmNeditable,         False); n++;
    XtSetArg(args[n], XmNeditMode,         XmMULTI_LINE_EDIT); n++;
    XtSetArg(args[n], XmNwordWrap,         True); n++;
    XtSetArg(args[n], XmNscrollHorizontal, False); n++;
    XtSetArg(args[n], XmNcursorPositionVisible, False); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
    text_output = XmCreateScrolledText(rowcol, "text_output", args, n);

    XtManageChild (text_output);

    bform = XmCreateForm(rowcol, "form", NULL, 0);

    rowcol2 = XtVaCreateWidget ("rowcol",
	xmRowColumnWidgetClass, bform,
        XmNbottomAttachment, XmATTACH_WIDGET,
        XmNorientation, XmHORIZONTAL, NULL);

    pb_test = XtVaCreateManagedWidget ("  Change Mode to \nTest (Comms Live) ",
        xmPushButtonWidgetClass, rowcol2, XmNwidth, 150, XmNheight,60,
	XmNsensitive, TRUE,
        RES_CONVERT(XmNfontList, FONT_NAME), NULL);

    XtAddCallback (pb_test, XmNactivateCallback, events_handler, (void*)
                   TEST_MODE);

    pb_practice = XtVaCreateManagedWidget ("  Change Mode to\nPractice (In House)",
        xmPushButtonWidgetClass, rowcol2, XmNwidth, 150, XmNheight,60,
	XmNsensitive, TRUE,
        RES_CONVERT(XmNfontList, FONT_NAME), NULL);

    XtAddCallback (pb_practice, XmNactivateCallback, events_handler,
                  (void*)PRACTICE_MODE);

    pb_operation = XtVaCreateManagedWidget ("     You are in      \n     Operational     ",
        xmPushButtonWidgetClass, rowcol2, XmNsensitive, TRUE,
        XmNwidth, 150, XmNheight,60,
        RES_CONVERT(XmNfontList, FONT_NAME), NULL);
    XtAddCallback (pb_operation, XmNactivateCallback, events_handler,
                  (void*)OPERATION_MODE);

    pb_cancel = XtVaCreateManagedWidget ("       Cancel        ",
        xmPushButtonWidgetClass, rowcol2,
        XmNwidth, 150, XmNheight,60,
        RES_CONVERT(XmNfontList, FONT_NAME), NULL);
    XtAddCallback (pb_cancel, XmNactivateCallback, events_handler,
                  (void*)EXIT_MODE);

    XtManageChild (rowcol2);
    XtManageChild (bform);
    XtManageChild (rowcol);
    check_mode();

    // catch Xt errors
    XtAppSetErrorHandler (app, xt_error);
    XtAppSetWarningHandler (app, xt_error);

    // and Xlib errors
    XSetErrorHandler (x_error);

    XtVaSetValues(toplevel, XmNtitle, "Test Mode Control", NULL);
    XtVaSetValues(toplevel, XmNmaxHeight, DEF_HEIGHT, NULL);
    XtVaSetValues(toplevel, XmNmaxWidth, DEF_WIDTH, NULL);
    XtVaSetValues(toplevel, XmNminHeight, DEF_HEIGHT, NULL);
    XtVaSetValues(toplevel, XmNminWidth, DEF_WIDTH, NULL);

    // Create cursor
    create_cusor();

    XtRealizeWidget (toplevel);

    if (ck !=0)
        insert_text(text_output,"Found missing executable or script\n");

    XtAppMainLoop (app);
}
