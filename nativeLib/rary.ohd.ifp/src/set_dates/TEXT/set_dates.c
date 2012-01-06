/* ************************************************************************************************

       set_dates.c program - originally programmed by Tom Adams, HRL, 1990.
	       modified by Tom Adams to use create_controls from libXifp.a - July 1990.

	  ...this version modified 7/11/90 for the Motif widget set for use on an IBM-RS6000

   ************************************************************************************************ */

#include "set_dates.h"
#include "ifp_atoms.h"
#include "ifp_help.h"
#include "c_call_f/upinio.h"
#include "c_call_f/upchkd_wrapper.h"
#include  "text_dates.h"
extern MYDATETYPE Adate[3], *pAdate[2];
extern display_widgets *create_ctrls_text (Widget, char *, int, int);
/* void popup_Bad_Date_dialog(Widget ,char  [] );*/
set_dates_main (argc, argv)
	int             argc;
	char            **argv;
{
	Widget          toplevel, bb, set, cancel_button;

	Widget          popup_bb, popup_remove;
	the_widgets     *run_display;
	Widget          date_up, date_down;

	Arg             wargs[5];
	int             which_widget, bg;
	int             len;

	char    system_command[100];

/*----------------------------------------------------------------------*/
/* Assign directory pathnames for NWSRFS OFS file assignments           */
/*----------------------------------------------------------------------*/
 {
  char            global_dirs[] = "syst oper";
  int             lenstr = 9;
  int             dir_chk_status;

  (void) UPINIO();
  (void) UPCHKD_WRAPPER(global_dirs,&lenstr,&dir_chk_status);

  if(dir_chk_status != 0) exit(0);
 }
toplevel = XtInitialize(argv[0], "Set_dates", NULL, 0, &argc, argv);

	dpy = XtDisplay(toplevel);
	root = DefaultRootWindow(dpy);

/* ********************************** Intern the Atoms *********************************  */

intern_the_atoms(toplevel);

/*      Create a bulletin board widget to hold command and display widgets                      */

bb = XtCreateManagedWidget("board", xmBulletinBoardWidgetClass, toplevel, NULL, 0);

/*      Create static text widgets to hold date/time headings                  */

XtSetArg(wargs[0], XmNlabelString,
	XmStringCreate("Start of run", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("start", xmLabelWidgetClass, bb, wargs, 1);
XtSetArg(wargs[0], XmNlabelString,
	XmStringCreate("End of run", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("end", xmLabelWidgetClass, bb, wargs, 1);
XtSetArg(wargs[0], XmNlabelString,
	XmStringCreate("End of observations", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("observations", xmLabelWidgetClass, bb, wargs, 1);

if(!date_rootWindow_properties_present(toplevel))
  {
   memset(system_command, '\0', 100);

 /* call routine to get the scripts directory path */
   len = strlen("ifp_scripts_dir");
   get_apps_defaults("ifp_scripts_dir", &len, system_command, &len);

   strcat(system_command, "/post_default_run_dates_script");
   system(system_command);
  }

run_display  = (the_widgets  *) malloc(sizeof(the_widgets));

run_display->start = create_controls (bb, "start_run", TEN);

run_display->end = create_ctrls_text (bb, "end_run", TWENTY, 1);
/* AV testing here */
run_display->end_obs = create_ctrls_text (bb, "end_obs_data", THIRTY, 0);
/* AV testing here */
date_up = XtCreateManagedWidget("date_up", xmArrowButtonWidgetClass, bb, NULL, 0);
date_down = XtCreateManagedWidget("date_down", xmArrowButtonWidgetClass, bb, NULL, 0);

/*      Callbacks to increment and decrement the dates & times...                               */
/*
 XtAddCallback(date_up, XmNarmCallback, multiple_increment, run_display);
 XtAddCallback(date_down, XmNarmCallback, multiple_decrement, run_display);
*/

/*pAdate->aUp = date_up;
pAdate->aDown = date_down;*/
/* AV added here to change set_date from pushbutton widget to text widget */
XtAddCallback(date_up,   XmNarmCallback,    ArrowUpCB,    NULL);
XtAddCallback(date_down, XmNarmCallback,    ArrowDownCB,  NULL);

XtAddCallback(date_up,   XmNdisarmCallback, ArrowUpCB,    NULL);
XtAddCallback(date_down, XmNdisarmCallback, ArrowDownCB,  NULL);




/*      Add a "Cancel" button to the bulletin board widget                                              */

cancel_button = create_quit_button("Cancel", bb);               /* Exits without saving changes         */

XtSetArg(wargs[0], XmNbackground, &bg);
XtGetValues(cancel_button, wargs, 1);

XtSetArg(wargs[0], XmNborderWidth, 4);
XtSetArg(wargs[1], XmNborderColor, bg);
XtSetValues(cancel_button, wargs, 2);

/*      Add a "Set" button to the bulletin board to set the date/time                                   */

XtSetArg(wargs[0], XmNshowAsDefault, 1);
set = XtCreateManagedWidget("Set", xmPushButtonWidgetClass, bb, wargs, 1);
/*      Callback to set dates and exit if OK...                                                         */
XtAddCallback(set, XmNactivateCallback, set_dates, toplevel);
XtSetKeyboardFocus(bb, set);


/* ************ Create a popup_shell toplevel widget for invalid dates ***************  */

 popup_shell = XtCreateApplicationShell("Set_Dates_Message", topLevelShellWidgetClass, NULL, 0);

 popup_bb = XtCreateManagedWidget("popup_bb", xmBulletinBoardWidgetClass, popup_shell, NULL, 0);

 popup_text = XtCreateManagedWidget("popup_text", xmLabelWidgetClass, popup_bb, NULL, 0);

 XtSetArg(wargs[0], XmNlabelString,
	XmStringCreate("OK", XmSTRING_DEFAULT_CHARSET));
 XtCreateManagedWidget("start", xmLabelWidgetClass, bb, wargs, 1);

 popup_remove = XtCreateManagedWidget("Remove", xmPushButtonWidgetClass, popup_bb, wargs, 1);
 XtAddCallback(popup_remove, XmNactivateCallback, remove_popup, popup_shell);


 which_widget = NONE_SELECTED;

 XChangeProperty
	(
	dpy,
	root,
	IFPA_display_selected,
	IFPA_display_selected_type,
	8,
	PropModeReplace,
	(unsigned char *)&which_widget,
	4
	);




XtRealizeWidget(toplevel);
XtMainLoop();

}




/* ****************************************************************************

	 set_dates()
		set the run_dates Atom from the dates retrieved by XGetWindowProperty
		of the display_dates Atom, then exit the program

   **************************************************************************** */

void set_dates (set, toplevel, call_data)
	Widget                  set;
	Widget                  toplevel;
	XmAnyCallbackStruct     call_data;
{
	Display         *display;
	Window          root;
	date            *start_date; /* start of run */
	date            *end_date;   /* end of run */
	date            *end_obs_date;  /* end of obervation data */
	int             type;   /* type of data stored in the window property */
	int             format; /* format of the stored data */
	int             nitems; /* number of bytes retrieved */
	int             left;   /* remaining bytes stored in the window */
	long            offset = 0;
	int             start_ok, end_ok, end_obs_ok;  /* status flags */
	Arg             wargs[1];
	char            str[200];
        char            str_message[100];
	display = XtDisplay(set);
	root = DefaultRootWindow(display);


        if ( isCurTimeInvalid() != 0) {
             /* dates is not valid */
             return;
        }
        UpdateDates(toplevel);

	   if(XGetWindowProperty(
				display,
				root,
				IFPA_display_start_date,
				offset,
				(long) sizeof(date),
				FALSE,
				(Atom)IFPA_display_start_date_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&start_date
				) == Success && type == IFPA_display_start_date_type)

	   start_ok = check_date(start_date);


	   if(XGetWindowProperty(
				display,
				root,
				IFPA_display_end_obs_date,
				offset,
				(long) sizeof(date),
				FALSE,
				(Atom)IFPA_display_end_obs_date_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&end_obs_date
				) == Success && type == IFPA_display_end_obs_date_type)

	   end_obs_ok = check_date(end_obs_date);


	   if(XGetWindowProperty(
				display,
				root,
				IFPA_display_end_date,
				offset,
				(long) sizeof(date),
				FALSE,
				(Atom)IFPA_display_end_date_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&end_date
				) == Success && type == IFPA_display_end_date_type)


	   end_ok = check_date(end_date);

 if(date_compare(start_date, end_date) >= 0)            /* Only if start_date >= end_date                       */
	{
        strcpy(str_message,"The date set for the end of the run is not acceptable...");
        popup_Bad_Date_dialog(toplevel,str_message);
	return;
	}
 else if(date_compare(end_obs_date, end_date) > 0)            /* Only if end_obs_date >= end_date                       */
	{
        strcpy(str_message,"The date set for the end of the observations date is not acceptable...");
        popup_Bad_Date_dialog(toplevel,str_message);
	return;
	}
 else if(date_compare(start_date, end_obs_date) > 0)            /* Only if start_date >= end_obs_date                       */
	{
        strcpy(str_message,"The date set for the end of the observations date is not acceptable...");
        popup_Bad_Date_dialog(toplevel,str_message);
	return;
	}
 else if(date_compare(end_date, start_date) > MAX_RUN_PERIOD) /* Only if end_date - start_date > 744 hours      */
	{
        strcpy(str_message,"The date set for the end of the run is not acceptable...");
        popup_Bad_Date_dialog(toplevel,str_message);
	return;
	}

 if(start_ok != 1  ||  end_obs_ok != 1  ||  end_ok != 1)
	{
	memset(str, '\0', 200);
	if(start_ok != 1)   strcat(str,"The start run date is not valid.\n\n");
	if(end_obs_ok != 1) strcat(str,"The end obs date is not valid.\n\n");
	if(end_ok != 1)     strcat(str,"The end run date is not valid.\n\n");
	strcat(str,"You must fix this before setting the dates.\n");

	if(XtIsRealized(popup_shell))
		{
		XMapWindow(display, XtWindow(popup_shell));
		XMapSubwindows(display, XtWindow(popup_shell));
		}
	else XtRealizeWidget(popup_shell);

	XtSetArg(wargs[0], XmNlabelString, XmStringCreate(str, XmSTRING_DEFAULT_CHARSET));
	XtSetValues(popup_text, wargs, 1);
	}
 else   {

	XChangeProperty
		(
		display,
		root,
		IFPA_run_start_date,
		IFPA_run_start_date_type,
		8,
		PropModeReplace,
		(unsigned char *)start_date,
		sizeof(date)
		);
	XFlush(XtDisplay(set));

printf("Start date: %d/%d/%d %d %s\n", start_date->month,
				       start_date->day,
				       start_date->year,
				       start_date->hour,
				       start_date->time_zone);

	XChangeProperty
		(
		display,
		root,
		IFPA_run_end_obs_date,
		IFPA_run_end_obs_date_type,
		8,
		PropModeReplace,
		(unsigned char *)end_obs_date,
		sizeof(date)
		);
	XFlush(XtDisplay(set));

printf("End obs date: %d/%d/%d %d %s\n", end_obs_date->month,
					 end_obs_date->day,
					 end_obs_date->year,
					 end_obs_date->hour,
					 end_obs_date->time_zone);

	XChangeProperty
		(
		display,
		root,
		IFPA_run_end_date,
		IFPA_run_end_date_type,
		8,
		PropModeReplace,
		(unsigned char *)end_date,
		sizeof(date)
		);
	XFlush(XtDisplay(set));

printf("End date: %d/%d/%d %d %s\n", end_date->month,
				     end_date->day,
				     end_date->year,
				     end_date->hour,
				     end_date->time_zone);

	XFree(start_date);
	XFree(end_date);
	XFree(end_obs_date);

	}
 XFlush(XtDisplay(set));
 exit(0);
}

void exit_set_dates(w, client_data, call_data)
	Widget                  w;
	caddr_t                 client_data;
	XmAnyCallbackStruct     *call_data;
{

}



void remove_popup(w, popup_shell, call_data)
	Widget          w;
	Widget          popup_shell;
	caddr_t         call_data;
{

XUnmapSubwindows(XtDisplay(popup_shell), XtWindow(popup_shell));
XUnmapWindow(XtDisplay(popup_shell), XtWindow(popup_shell));

}




/* ************************ 'Quit' button Callbacks ******************** */

static void arm_callback(w, flag, call_data)
	Widget                  w;
	int                     *flag;
	XmAnyCallbackStruct     *call_data;
{

*flag = FALSE;

}


static void activate_callback(w, flag, call_data)
	Widget                  w;
	int                     *flag;
	XmAnyCallbackStruct     *call_data;
{

*flag = TRUE;

}

static void disarm_callback(w, flag, call_data)
	Widget                  w;
	int                     *flag;
	XmAnyCallbackStruct     *call_data;
{

if(*flag)
	{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
	}
}

/* **********************************************************************

	create_quit_button()
		creates a 'Quit' button with a variable label


   ********************************************************************** */

Widget create_quit_button(label, parent)
	char            *label;
	Widget          parent;
{

	Widget          w;
	static int      really_quit;


w = XtCreateManagedWidget(label, xmPushButtonWidgetClass, parent, NULL, 0);

XtAddCallback(w, XmNarmCallback, arm_callback, &really_quit);
XtAddCallback(w, XmNdisarmCallback, disarm_callback, &really_quit);
XtAddCallback(w, XmNactivateCallback, activate_callback, &really_quit);

return(w);

}



/* **********************************************************************

	popup_Bad_endDate_dialog()

   ********************************************************************** */

void popup_Bad_Date_dialog(Widget toplevel,char string[])

{
	Widget          shell;
	Widget          cancel_button, ok_button, help_button;
	Arg             wargs[8];

	/*char            string[101];*/
        char            str_msg[100];
	XmString        xmMessageString;
	int             n;

	help_struct     *help_data;




help_data = (help_struct *) malloc(sizeof(help_struct));
memset(str_msg,'\0',100);

help_data->parent = toplevel;
help_data->message_widget_name = "bad_endDate_error";

strncpy(str_msg,string,strlen(string));

xmMessageString = XmStringCreateLtoR(str_msg, XmSTRING_DEFAULT_CHARSET);
n = 0;
XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
XtSetArg(wargs[n], XmNokLabelString, XmStringCreate("OK", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XmNminimizeButtons, FALSE); n++;
shell = XmCreateWarningDialog
			(
			toplevel,
			"cancelAll_warningShell",
			wargs,
			n
			);
XtAddCallback(shell, XmNhelpCallback, create_help_Dialog, help_data);


ok_button = XmMessageBoxGetChild(shell, XmDIALOG_OK_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(ok_button, wargs, 1);


help_button = XmMessageBoxGetChild(shell, XmDIALOG_HELP_BUTTON);
XtSetArg(wargs[0], XtNwidth, 100);
XtSetValues(help_button, wargs, 1);

cancel_button = XmMessageBoxGetChild(shell, XmDIALOG_CANCEL_BUTTON);
XtUnmanageChild(cancel_button);

XtManageChild(shell);

}




/* ****************************************************************************

	 date_rootWindow_properties_present()

   **************************************************************************** */

int date_rootWindow_properties_present(toplevel)
	Widget                  toplevel;
{
	Display         *display;
	Window          root;
	date            *start_date; /* start of run */
	date            *end_date;   /* end of run */
	date            *end_obs_date;  /* end of obervation data */
	int             type;   /* type of data stored in the window property */
	int             format; /* format of the stored data */
	int             nitems; /* number of bytes retrieved */
	int             left;   /* remaining bytes stored in the window */
	long            offset = 0;

	display = XtDisplay(toplevel);
	root = DefaultRootWindow(display);


 if(!(XGetWindowProperty(
	display,
	root,
	IFPA_run_start_date,
	offset,
	(long) sizeof(date),
	FALSE,
	(Atom)IFPA_run_start_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&start_date
	) == Success && type == IFPA_run_start_date_type)) return(FALSE);


 if(!(XGetWindowProperty(
	display,
	root,
	IFPA_run_end_obs_date,
	offset,
	(long) sizeof(date),
	FALSE,
	(Atom)IFPA_run_end_obs_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&end_obs_date
	) == Success && type == IFPA_run_end_obs_date_type)) return(FALSE);


 if(!(XGetWindowProperty(
	display,
	root,
	IFPA_run_end_date,
	offset,
	(long) sizeof(date),
	FALSE,
	(Atom)IFPA_run_end_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&end_date
	) == Success && type == IFPA_run_end_date_type)) return(FALSE);

/*
 * return true only if all run dates (start, end_obs, and end) are set
 */
 return(TRUE);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/set_dates/RCS/set_dates.c,v $";
 static char rcs_id2[] = "$Id: set_dates.c,v 1.5 2006/03/29 14:27:10 aivo Exp $";}
/*  ===================================================  */

}
