
#include "libXifp.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"
#include "ifp_help.h"
#include "help.h"




/* ****************************************************************************

	 run_nwsrfs()
		   - Code to initiate NWSRFS (fork a new Unix process)...
		   - Certain menu items are made insensitive until the hydrologic
		     models have finished running for the first segment...
		   - Universal & Non-universal root window properties are set...
		   - The run_info_popup is created, which displays:
			+ Start Date, Run End Date, & End of Observations
			+ A List of segments in computational order

   **************************************************************************** */
/* selectedSegmentStr is used by run_callback.c */
void remove_maxminFile() ;
char        selectedSegmentStr[9];
void fork_nwsrfs(Display *display);
void run_nwsrfs(set, someWidgets, call_data)
	Widget                  set;
	the_widget_struct       *someWidgets;
	caddr_t                 call_data;
{
	Display         *display;
	Window          root;

	char            *the_segments, *string;
	char            *first_blank;
	char            *segment_name;

	int             type, format, nitems, left;
	int             i, number_of_Segments;
	long            offset = 0;

	XmString        *xmstring_Segments;
	Arg             wargs[3];




 display = XtDisplay(set);
 root = DefaultRootWindow(display);
 
 remove_maxminFile();
 /* initialize selected segment flag for turn on/off non-universal techniques */
 strcpy(selectedSegmentStr,"END");
 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_segments,
	offset,
	(long) 801,
	FALSE,
	IFPA_run_segments_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&the_segments
	) == Success && type == IFPA_run_segments_type){

	segment_name = (char *) malloc(9);
	memset(segment_name, '\0', 9);

	/* Set Menu items SENSITIVE...                                          */
	
        XtSetSensitive(someWidgets->rerun_widget, TRUE);
	XtSetSensitive(someWidgets->showRunSegments_widget, TRUE);
/*	XtSetSensitive(someWidgets->showTulsaPlot_widget, TRUE);
	XtSetSensitive(someWidgets->showTimeSeriesTable_widget, TRUE);*/
	XtSetSensitive(someWidgets->showOtherMods_widget, TRUE);
	XtSetSensitive(someWidgets->showModsViewer_widget, TRUE);
	XtSetSensitive(someWidgets->new_ForecastGroup_widget, TRUE);

	/* Set Menu items INSENSITIVE...                                */
	XtSetSensitive(someWidgets->showTulsaPlot_widget, FALSE);
	XtSetSensitive(someWidgets->showTimeSeriesTable_widget, FALSE);
	
	XtSetSensitive(someWidgets->begin_widget, FALSE);
	XtSetSensitive(someWidgets->revert_widget, FALSE);
	XtSetSensitive(someWidgets->deleteSegments_widget, FALSE);
	XtSetSensitive(someWidgets->keepSubset_widget, FALSE);
	XtSetSensitive(someWidgets->reset_widget, FALSE);
	XtSetSensitive(someWidgets->setDates_widget, FALSE);
	XtSetSensitive(someWidgets->showOperationsTable_widget, FALSE);
	XtSetSensitive(someWidgets->showRatingCurve_widget, FALSE);
	XtSetSensitive(someWidgets->run_multiple_widget, FALSE);
	if(someWidgets->universal_widget != NULL)
			XtSetSensitive(someWidgets->universal_widget, FALSE);


	create_runInfo_popup(set, someWidgets, NULL);

	XtVaGetValues(someWidgets->listWidget_for_forecastSegments,
		      XmNitems,     &xmstring_Segments,
		      XmNitemCount, &number_of_Segments,
		      NULL);

	if(number_of_Segments > 1) XtSetSensitive(someWidgets->next_widget, TRUE);
	else inLastSegment = TRUE;


	/* Find the next segment name...                                                                */
	/* Segments names appear in the list widget in computational order... upstream -> downstream... */
	XmStringGetLtoR(xmstring_Segments[0], XmSTRING_DEFAULT_CHARSET, &string);


	/* Set the Current Segment root window property...                                              */
	XChangeProperty
		(
		display,
		root,
		IFPA_current_segment,
		IFPA_current_segment_type,
		8,
		PropModeReplace,
		(unsigned char *) string,
		strlen(string)
		);


	strcpy(someWidgets->current_segment, string);

	/* Pass universal techniques to other NWSRFS/IFP components             */
	/*      through an X Window property...                                 */
	univ_techs_to_window_property(set);

	if((first_blank = strstr(string, " ")) != NULL)
			strncpy(segment_name, string, first_blank - string);

	/* The following 2 lines added to handle case where                    */
	/*  the segment name is 8 characters long and therefore there is no    */
	/*  first_blank found.  The string must still be copied into the       */
	/*  segment_name to be used by non_univ_techs_to_window_property.      */
	/* Changed by gfs - hrl - 20 Aug 1994.                                 */
	else
                        strncpy(segment_name, string, 8);

	/* Pass non-universal techniques to other NWSRFS/IFP components         */
	/*      through an X Window property...                                 */
	non_univ_techs_to_window_property(segment_name, set, someWidgets);


	for(i = 0; i <= sub_group_num; i++)
		{
		if(someWidgets->head[i] != NULL)
			{
			remove_segment_callbacks(someWidgets->head[i]);
			add_segment_callbacks(someWidgets->head[i], handle_segment_selected, someWidgets);
			add_segment_callbacks(someWidgets->head[i], tell_which_tree, someWidgets);
			}
		}

	if(someWidgets->currentSegment_widget == NULL)
			someWidgets->currentSegment_widget = someWidgets->head[0]->segment_widget;

	whichTree_index = 0;
	NWSRFS_has_begun = TRUE;

        /* post window property to save (or not) gif files 
           save_gif is global - set in read_write_data - dp -8/3/95
        */
        post_save_gif_atom(someWidgets->toplevel, save_gif);
        
	/* Call to begin execution of NWSRFS hydrologic component...            */
	fork_nwsrfs(display);
	}

else    {               /*      No segments posted, so SysBeep...               */
	XBell(display, 100);
	XFlush(display);
	}

}






/* ****************************************************************************

	 create_displays()
		creates static text widgets for display of the date & time
		and arrow widgets for incrementing & decrementing the date
		& time

   **************************************************************************** */

display_widgets *create_displays (bboard, name, which_form_widget)
	Widget          bboard;                 /* XmBulletinBoard widget...    */
	char            *name;                  /* Widget name                  */
	int             which_form_widget;
{
	Widget          form;
	Widget          time_zone;
	Arg             wargs[2];
	char            string[5], the_zone[5];
	struct  tm      *time_pointer;
	int             widget_zero, widget_one, widget_two, widget_three;
	int             type, format, nitems, left;
	int             n, which_widget;
	int             the_month, the_day, the_year, the_hour;

	long            offset = 0;
	long            tp;

	date            *start_date, *end_date, *end_obs_date, *the_date;

	Display         *display;
	Window          root;

	display_widgets *control;


 display = XtDisplay(bboard);
 root = DefaultRootWindow(display);


 control = (display_widgets *) malloc(sizeof(display_widgets));

/*      Create a Form widget to hold date & time displays and controls                          */

form = XtVaCreateManagedWidget(name, xmRowColumnWidgetClass, bboard,
			       XmNorientation, XmHORIZONTAL,
			       XmNnumColumns, 5,
			       NULL);

which_widget = which_form_widget;

switch(which_form_widget)
	{
		case TEN:
			if(XGetWindowProperty
			(
			display,
			root,
			IFPA_run_start_date,
			offset,
			(long) sizeof(date),
			FALSE,
			IFPA_run_start_date_type,
			(Atom *)&type,
			(int *)&format,
			(unsigned long *)&nitems,
			(unsigned long *)&left,
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_start_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				strcpy(the_zone, the_date->time_zone);
				}
			break;

		case TWENTY:
			if(XGetWindowProperty
			(
			display,
			root,
			IFPA_run_end_date,
			offset,
			(long) sizeof(date),
			FALSE,
			IFPA_run_end_date_type,
			(Atom *)&type,
			(int *)&format,
			(unsigned long *)&nitems,
			(unsigned long *)&left,
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_end_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				strcpy(the_zone, the_date->time_zone);
				}
			break;


		case THIRTY:
			if(XGetWindowProperty
			(
			display,
			root,
			IFPA_run_end_obs_date,
			offset,
			(long) sizeof(date),
			FALSE,
			IFPA_run_end_obs_date_type,
			(Atom *)&type,
			(int *)&format,
			(unsigned long *)&nitems,
			(unsigned long *)&left,
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_end_obs_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				strcpy(the_zone, the_date->time_zone);
				}
			break;

	}



/*      here    */

/*      Create static text widgets to hold dates & times:                                       */
/*              - intialize to the last dates & times set or the current system time & date     */

     switch(the_month)
			{
			case    1:
				strcpy(string, "Jan");
				break;

			case    2:
				strcpy(string, "Feb");
				break;

			case    3:
				strcpy(string, "Mar");
				break;

			case    4:
				strcpy(string, "Apr");
				break;

			case    5:
				strcpy(string, "May");
				break;

			case    6:
				strcpy(string, "Jun");
				break;

			case    7:
				strcpy(string, "Jul");
				break;

			case    8:
				strcpy(string, "Aug");
				break;

			case    9:
				strcpy(string, "Sep");
				break;

			case    10:
				strcpy(string, "Oct");
				break;

			case    11:
				strcpy(string, "Nov");
				break;

			case    12:
				strcpy(string, "Dec");
				break;

			default:
				strcpy(string, "None");
				break;
			}

 control->month = XtVaCreateManagedWidget("month", xmLabelWidgetClass, form,
					  XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
					  NULL);

 sprintf(string, "%d", the_day);
 control->day = XtVaCreateManagedWidget("day", xmLabelWidgetClass, form,
					  XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
					  NULL);

 sprintf(string, "%d", the_year);
 control->year = XtVaCreateManagedWidget("year", xmLabelWidgetClass, form,
					  XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
					  NULL);

 sprintf(string, "%d", the_hour);
 control->time = XtVaCreateManagedWidget("hour", xmLabelWidgetClass, form,
					  XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
					  NULL);


 time_zone = XtVaCreateManagedWidget("zone", xmLabelWidgetClass, form,
					  XtVaTypedArg, XmNlabelString, XmRString, the_zone, strlen(the_zone)+1,
					  NULL);

 return(control);

}




/* ****************************************************************************

	 check_for_dates()
			checks to see if ALL the dates have been posted for
			the NWSRFS run; returns:
						YES = 1, if true,
						NO  = 0, otherwise.

   **************************************************************************** */

int check_for_dates ()
{
	Arg             wargs[1];
	char            string[5];
	date            *start_date, *end_date, *end_obs_date;

	Display         *display;
	Window          root;

	int             type, format, nitems, left;
	long            offset = 0;

	display = XtDisplay(global_toplevel);
	root = DefaultRootWindow(display);


/*      here    */


	if
	  (
	  (XGetWindowProperty
			(
			display,
			root,
			IFPA_run_start_date,
			offset,
			(long) sizeof(date),
			FALSE,
			IFPA_run_start_date_type,
			(Atom *)&type,
			(int *)&format,
			(unsigned long *)&nitems,
			(unsigned long *)&left,
			(unsigned char **)&start_date
			) == Success && type == IFPA_run_start_date_type)

	&& (XGetWindowProperty
			(
			display,
			root,
			IFPA_run_end_date,
			offset,
			(long) sizeof(date),
			FALSE,
			IFPA_run_end_date_type,
			(Atom *)&type,
			(int *)&format,
			(unsigned long *)&nitems,
			(unsigned long *)&left,
			(unsigned char **)&end_date
			) == Success && type == IFPA_run_end_date_type)

	&& (XGetWindowProperty
			(
			display,
			root,
			IFPA_run_end_obs_date,
			offset,
			(long) sizeof(date),
			FALSE,
			IFPA_run_end_obs_date_type,
			(Atom *)&type,
			(int *)&format,
			(unsigned long *)&nitems,
			(unsigned long *)&left,
			(unsigned char **)&end_obs_date
			) == Success && type == IFPA_run_end_obs_date_type)

	)       return (YES);

	else    return (NO);

}


/* ****************************************************************************

	 fill_list()
			fills the list widget with the segment_names for the
			NWSRFS run - obtained from the window property.

   **************************************************************************** */

void fill_list (num_list)
	int             *num_list;
{
	Arg             wargs[1];
	char            string[9], segment_name[9], *the_segments;
	Display         *display;
	Window          root;

	int             type, format, nitems, left;
	int             i, n;
	int             k = 0;
	long            offset = 0;
	int             num_chars = 0;

	the_list        **list_widget;


 display = XtDisplay(global_toplevel);
 root = DefaultRootWindow(display);

 memset(string, '\0', 9);

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_segments,
	offset,
	(long) 801,
	FALSE,
	IFPA_run_segments_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&the_segments
	) == Success && type == IFPA_run_segments_type){

	i = 0;


	*num_list = strlen(the_segments)/8;
	list_widget = (the_list **) malloc(sizeof(the_list *) * (*num_list));

	for(k = 0; k < *num_list; k++)
		{
		list_widget[k] = (the_list *) malloc(sizeof(the_list));
		memset(list_widget[k]->name, '\0', 9);
		strncpy(list_widget[k]->name, the_segments, 8);
		the_segments += 8;
		}

	xmstr_Run_list = (XmString *)XtRealloc((char *)xmstr_Run_list, sizeof(XmString) * (*num_list));

	for(k = 0; k < *num_list; k++)
		{
		xmstr_Run_list[k] = XmStringCreateLtoR(list_widget[k]->name, XmSTRING_DEFAULT_CHARSET);
		}
        if(list_widget != NULL) /*--AV add check here for linux */
	     free(list_widget);

	}                       /*      end if 'XGetProp...'    */

	else    {
		 XtSetArg(wargs[0], XmNlabelString,
				    XmStringCreate("No segments", XmSTRING_DEFAULT_CHARSET));
		 XtCreateManagedWidget("no_segments", xmLabelWidgetClass, global_toplevel, wargs, 1);
		}


}                               /*      end of fill_list()      */




/* ****************************************************************************

	 exit_run()
			un-maps the 'parent' widget

   **************************************************************************** */

void exit_run(enabling_widget, parent, call_data)
	Widget          enabling_widget;
	Widget          parent;
	caddr_t         call_data;
{
	int             is_already_running = FALSE;

	Display         *display;
	Window          root;



 display = XtDisplay(enabling_widget);
 root = DefaultRootWindow(display);

 XDeleteProperty(display, root, IFPA_IFP_NWSRFS_is_running);

 XFlush(display);

 exit(0);

}



/* ****************************************************************************

	 make_display_widgets()
			make the display widgets,
			       if (check_for_dates(global_toplevel) == YES);
			returns: the widget 'message_shell'

   **************************************************************************** */

the_widgets *make_display_widgets (some_widgets)
	the_widget_struct       *some_widgets;
{

	Widget          toplevel, parent;
	Widget          execute, list, scroll_window;
	Widget          message_shell, message_bb, run_ok;
	char            string[50];
	the_widgets     *run_display;
	Arg             wargs[5];

	Display         *display;
	XmString        next_xmstring;
	int             k, num_list;
	int             n = 0;


memset(string, '\0', 50);

toplevel = some_widgets->run_info_shell;
parent = some_widgets->run_info_bb;     /* A bulletin-board widget created in main()    */


display = XtDisplay(toplevel);

/*      Create static text widgets to hold date/time headings                  */

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Dates for this run are:", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("message", xmLabelWidgetClass, parent, wargs, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Segments for this run are:", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("list_msg", xmLabelWidgetClass, parent, wargs, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Start of run", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("start", xmLabelWidgetClass, parent, wargs, 1);
XtSetArg(wargs[0], XmNlabelString, XmStringCreate("End of run", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("end", xmLabelWidgetClass, parent, wargs, 1);
XtSetArg(wargs[0], XmNlabelString, XmStringCreate("End of observations", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("observations", xmLabelWidgetClass, parent, wargs, 1);

run_display  = (the_widgets  *) malloc(sizeof(the_widgets));

run_display->start = create_displays (parent, "start_run", TEN);
run_display->end = create_displays (parent, "end_run", TWENTY);
run_display->end_obs = create_displays (parent, "end_obs_data", THIRTY);

/*      Add an "OK" button to the bulletin board to execute NWSRFS                                  */
/*
execute = XtCreateManagedWidget("OK", xmPushButtonWidgetClass, parent, NULL, 0);
XtAddCallback(execute, XmNactivateCallback, run_nwsrfs, toplevel);   Callback to execute NWSRFS
*/

XtSetArg(wargs[0], XmNscrolledWindowMarginWidth, 0);
XtSetArg(wargs[1], XmNscrollingPolicy, XmAUTOMATIC);
scroll_window = XtCreateManagedWidget("scroll_window", xmScrolledWindowWidgetClass, parent, wargs, 2);

fill_list(&num_list);

/* printf("Number of items in the list, num_list = %d\n", num_list);  */

if(num_list > 0)
	{
	XtSetArg(wargs[0], XmNwidth, 100);
	XtSetArg(wargs[1], XmNitems, xmstr_Run_list);
	XtSetArg(wargs[2], XmNitemCount, num_list);
	XtSetArg(wargs[3], XmNvisibleItemCount, num_list);
	list = XtCreateManagedWidget("list", xmListWidgetClass, scroll_window, wargs, 4);

	some_widgets->listWidget_for_forecastSegments = list;
	}

return(run_display);

}



/* **************************************************************************************

	no_downstream_segment_selected_error ()
		Creates a Error popup dialog if a downstream segment is selected -
		for Partial_Run & Upstream_Run

   ************************************************************************************** */

void no_downstream_segment_selected_error (someWidgets)
	the_widget_struct       *someWidgets;
{


}



/* **************************************************************************************

	downstream_segment_selected_warning ()
		Creates a Warning popup dialog if a downstream segment is selected -
		for Run_All only...

   ************************************************************************************** */

void downstream_segment_selected_warning ()
{

}



/* ****************************************************************************

	 make_message_widgets()
			make the message widgets
			    if (check_for_dates(global_toplevel) != YES)

   **************************************************************************** */

void make_message_widgets ()
{
	Widget          message_shell;
	char            string[200];
	XmString        xmMessageString;
	Arg             wargs[4];

	Display         *display;
	help_struct     *help_data;
	int             n;


/*
 static XtCallbackRec cancelCallbackList[] =
	{
	{exit_run, message_shell},
	{(XtCallbackProc) NULL, (caddr_t) NULL}
	};

 static XtCallbackRec okCallbackList[] =
	{
	{exit_run, message_shell},
	{(XtCallbackProc) NULL, (caddr_t) NULL}
	};
*/


memset(string, '\0', 200);

display = XtDisplay(global_toplevel);

strcpy(string, "No dates have been set for the NWSRFS run;\n");
strcat(string, "-- set the dates for the run by using the\n");
strcat(string, "'Set dates' command in the main menu.");
xmMessageString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

n = 0;
/*
XtSetArg(wargs[n], XmNcancelCallback, cancelCallbackList); n++;
XtSetArg(wargs[n], XmNokCallback, okCallbackList); n++;
*/
XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
message_shell = XmCreateWarningDialog
				(
				global_toplevel,
				"seg_selected_msg",
				wargs,
				n
				);

help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = global_toplevel;
help_data->message_widget_name = "no_dates_set_dialog";

XtAddCallback(message_shell, XmNhelpCallback, popup_help_window, "NO_DATES_SET");
XtAddCallback(message_shell, XmNokCallback, exit_run, message_shell);
XtAddCallback(message_shell, XmNcancelCallback, exit_run, message_shell);



XtManageChild(message_shell);

}




/* ****************************************************************************

	univ_techs_to_window_property()



   **************************************************************************** */

void univ_techs_to_window_property(w)
	Widget          w;
{

	Display         *display;
	Window          root;




 display = XtDisplay(w);
 root = DefaultRootWindow(display);
/*
 * printf("Universal Techniques:\n");
 * printf(" mod_units = %d\n",                universalTechniques->mod_units);
 * printf(" mod_sac_units = %d\n",            universalTechniques->mod_sac_units);
 * printf(" mod_api_units = %d\n",            universalTechniques->mod_api_units);
 * printf(" metric_units = %d\n",             universalTechniques->metric_units);
 * printf(" mod_warning = %d\n",              universalTechniques->mod_warning);
 * printf(" future_precip = %d\n",            universalTechniques->future_precip);
 * printf(" input_time_zone_code = %s\n",     universalTechniques->input_time_zone_code);
 * printf(" output_daylight_savings = %d\n",  universalTechniques->output_daylight_savings);
 * printf(" output_time_zone = %d\n",         universalTechniques->output_time_zone);
 * printf(" mod_time_zone_code = %s\n",       universalTechniques->mod_time_zone_code);
 */
 XChangeProperty
	 (
	 display,
	 root,
	 IFPA_univ_techniques,
	 IFPA_univ_techniques_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)universalTechniques,
	 sizeof(univ_techniques_struct)
	 );
}



/* ****************************************************************************

	non_univ_techs_to_window_property()



   **************************************************************************** */

void non_univ_techs_to_window_property(segmentName, w, someWidgets)
	char                    *segmentName;
	Widget                  w;
	the_widget_struct       *someWidgets;
{

	Display         *display;
	Window          root;
	node            *segment_node;
	int             i;



 display = XtDisplay(w);
 root = DefaultRootWindow(display);

 for(i = 0; i <= sub_group_num; i++)
 {
        
     if((segment_node = find_it(someWidgets->head[i], segmentName)) != NULL) break;
     
 }
 
 
 
 XChangeProperty
	 (
	 display,
	 root,
	 IFPA_current_segment_non_univ_techs,
	 IFPA_current_segment_non_univ_techs_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)segment_node->techniques,
	 sizeof(non_univ_techniques_struct)
	 );


}



/* ****************************************************************************

	 update_date_display()

   **************************************************************************** */

void update_date_display(the_date, date_widgets)
	date            *the_date;
	display_widgets date_widgets;
{
	Arg             wargs[2];
	char            string[5];





 memset(string, '\0', 5);

 switch(the_date->month)
	{
	case    1:
		strcpy(string, "Jan");
		break;

	case    2:
		strcpy(string, "Feb");
		break;

	case    3:
		strcpy(string, "Mar");
		break;

	case    4:
		strcpy(string, "Apr");
		break;

	case    5:
		strcpy(string, "May");
		break;

	case    6:
		strcpy(string, "Jun");
		break;

	case    7:
		strcpy(string, "Jul");
		break;

	case    8:
		strcpy(string, "Aug");
		break;

	case    9:
		strcpy(string, "Sep");
		break;

	case    10:
		strcpy(string, "Oct");
		break;

	case    11:
		strcpy(string, "Nov");
		break;

	case    12:
		strcpy(string, "Dec");
		break;

	default:
		strcpy(string, "None");
		break;
	}

XtSetArg(wargs[0], XmNlabelString, XmStringCreate(string, XmSTRING_DEFAULT_CHARSET));
XtSetValues(date_widgets.month, wargs, 1);

sprintf(string, "%d", the_date->day);
XtSetArg(wargs[0], XmNlabelString, XmStringCreate(string, XmSTRING_DEFAULT_CHARSET));
XtSetValues(date_widgets.day, wargs, 1);

sprintf(string, "%d", the_date->year);
XtSetArg(wargs[0], XmNlabelString, XmStringCreate(string, XmSTRING_DEFAULT_CHARSET));
XtSetValues(date_widgets.year, wargs, 1);

sprintf(string, "%d", the_date->hour);
XtSetArg(wargs[0], XmNlabelString, XmStringCreate(string, XmSTRING_DEFAULT_CHARSET));
XtSetValues(date_widgets.time, wargs, 1);

/*
XtSetArg(wargs[0], XmNlabelString, XmStringCreate(the_date->time_zone, XmSTRING_DEFAULT_CHARSET));
XtSetValues(date_widgets->time_zone, wargs, 1);
*/

}



/* ****************************************************************************

	 handle_segment_selected()

   **************************************************************************** */

void handle_segment_selected(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	XmAnyCallbackStruct     *call_data;
{

	Arg             wargs[5];
	Display         *display;
	Window          root;

	char            *segmentName;
	char            *currentSegment;
	char            *first_blank;
	char            *string;

	XmString        *xmstr_segmentName;

	int             foreground;
	int             background;
	int             *number_of_TulPlots;
	int             type, format, nitems, left;
	int             i, n;
	int             k = 0;
	int             length;
	long            offset = 0;

	int             number_of_Segments;
	XmString        *xmstring_Segments;
	char            *last_segment_name;
	char            current_segment_no_blanks[9];

	node            *the_node;
        



 string = (char *) malloc(9);
 memset(string, '\0', 9);

 display = XtDisplay(w);
 root = DefaultRootWindow(display);


 if(call_data != NULL)
	{
	if((call_data->event->xbutton.state & ShiftMask) == ShiftMask)
	       {
	       /*printf("Shift key is pressed...\n"); */                         
	       return;
	       }
	}

 if(someWidgets->currentSegment_widget == w && NWSRFS_has_begun == TRUE)
	 {
	 XBell(display, 100);
	 return;
	 }

 XtVaGetValues(w, XmNlabelString, &xmstr_segmentName, NULL);

 XmStringGetLtoR((XmString)xmstr_segmentName, XmSTRING_DEFAULT_CHARSET, &segmentName);
 printf("The %s segment was selected.\n", segmentName);
 /* store selected segment.  Routing run_next_segment() in run_callbac.c
 will compare currentsegment with selectedsegment.  If currentsegment is not
 equal to selectedsegment, set sacsnow switch to off; turn the switch to on
 when currentsegment=selectedsegment */
 memset(selectedSegmentStr,'\0',9);
 strcpy(selectedSegmentStr,segmentName);
 /* printf("The %s is the selectedsegment .\n", selectedSegmentStr);*/
 /* When user selects goto segment turn off non-universal techniques for sacsnow*/
 
 for(i = 0; i <= sub_group_num; i++)
 {
        
	if((the_node = find_it(someWidgets->head[i], segmentName)) != NULL) break;
       
 }
 
 if(the_node->techniques->sac_snow == 1)
 {
     
     the_node->techniques->sac_snow = 0;
     non_univ_techs_to_window_property(segmentName, w, someWidgets);
     
 }

/*end av added */
 if(someWidgets->previous_segment == w)
	{ /* The selected widget is already selected, so unselect it, do other things, & return...      */

	/*      restore_default_colors(someWidgets->previous_segment);          */

	reset_segment_colors(w, segmentName, someWidgets);
	reset_previous_MAPBasin_selected(someWidgets->overlays);

	someWidgets->previous_segment = NULL;
	XDeleteProperty(display, root, IFPA_goto_downstream_segment);

	if(NWSRFS_has_begun == TRUE)
	    {
	    if(XGetWindowProperty
		    (
		    display,
		    root,
		    IFPA_number_of_TulPlots,
		    offset,
		    (long) 9,
		    FALSE,
		    IFPA_number_of_TulPlots_type,
		    (Atom *)&type,
		    (int *)&format,
		    (unsigned long *)&nitems,
		    (unsigned long *)&left,
		    (unsigned char **)&number_of_TulPlots
		    ) == Success && type == IFPA_number_of_TulPlots_type){

	      if(*number_of_TulPlots > 0)
		      {
		      XtSetSensitive(someWidgets->continue_widget, TRUE);
		      XtSetSensitive(someWidgets->next_widget, FALSE);
		      }
	      else    {
		      XtSetSensitive(someWidgets->continue_widget, FALSE);

		      /* See if we are in the last segment...                        */
		      XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
		      XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
		      XtGetValues(someWidgets->listWidget_for_forecastSegments, wargs, 2);

		      XmStringGetLtoR(xmstring_Segments[number_of_Segments - 1],
				      XmSTRING_DEFAULT_CHARSET, &last_segment_name);

		      memset(current_segment_no_blanks, '\0', 9);
		      strcpy(current_segment_no_blanks, someWidgets->current_segment);


		      /* Remove trailing blanks */
		      if((first_blank = strstr(current_segment_no_blanks, " ")) != NULL)
					*first_blank = '\0';

		      /* Remove trailing blanks */
		      if((first_blank = strstr(last_segment_name, " ")) != NULL)
					*first_blank = '\0';

		      if(strcmp(current_segment_no_blanks, last_segment_name) == 0)
			    {
			    inLastSegment = TRUE;
			    XtSetSensitive(someWidgets->next_widget, FALSE);
			    }
		      else
			    {
			    inLastSegment = FALSE;
			    XtSetSensitive(someWidgets->next_widget, TRUE);
			    }
		      }
	      }

	   XtSetSensitive(someWidgets->rerun_widget, FALSE);
	   XtSetSensitive(someWidgets->go_to_widget, FALSE);
	   }

	return;
	}
 else   {       /* The selected segment widget was not the previously selected widget...        */

	foreground = get_pixel_by_name(w, "orange");
	background = get_pixel_by_name(w, "LightGray");
	set_widget_colors(w, foreground, background);

	for(i = 0; i <= sub_group_num; i++)
	       {
	       if((the_node = find_it(someWidgets->head[i], segmentName)) != NULL)
		       {
		       whichTree_index = the_node->which_tree;
		       break;
		       }
	       }

	if(someWidgets->previous_segment != NULL)
	       restore_default_colors(someWidgets->previous_segment);

	someWidgets->previous_segment = w;

	reset_previous_MAPBasin_selected(someWidgets->overlays);
	set_MAPBasin_selected(the_node, someWidgets->overlays);

	if(NWSRFS_has_begun) XSetForeground(display, *someWidgets->overlays->Basin_gc,
					    get_pixel_by_name(w, flood_color_levels[SELECTED]));
	highlight_MAPBasin_selected(the_node, someWidgets->overlays);

	XtSetSensitive(someWidgets->rerun_widget, FALSE);
	XtSetSensitive(someWidgets->next_widget, FALSE);
	XtSetSensitive(someWidgets->continue_widget, FALSE);



	if(!NWSRFS_has_begun)
	       { /* We have not yet begun the run, so we are merely setting the downstream       */
		 /*      point to which we will run without showing Tulsa Plots...               */

	       XChangeProperty
		       (
		       display,
		       root,
		       IFPA_goto_downstream_segment,
		       IFPA_goto_downstream_segment_type,
		       8,
		       PropModeReplace,
		       (unsigned char *) segmentName,
		       strlen(segmentName)
		       );

	       return;
	       }


	length = strlen(segmentName);
	strcpy(someWidgets->segment_selected, segmentName);

	for(i = 0; i < 8 - length; i++) strcat(someWidgets->segment_selected, " ");


	if(XGetWindowProperty
	       (
	       display,
	       root,
	       IFPA_current_segment,
	       offset,
	       (long) 9,
	       FALSE,
	       IFPA_current_segment_type,
	       (Atom *)&type,
	       (int *)&format,
	       (unsigned long *)&nitems,
	       (unsigned long *)&left,
	       (unsigned char **)&currentSegment
	       ) == Success && type == IFPA_current_segment_type){
		       strcpy(someWidgets->current_segment, currentSegment);
	       }

	XtSetSensitive(someWidgets->go_to_widget, TRUE);
	}

}


/* ****************************************************************************

	 restore_default_colors()

   **************************************************************************** */

void restore_default_colors(w)
	Widget          w;
{

	Arg     wargs[2];


 if(w == NULL) return;


 XtSetArg(wargs[0], XmNforeground, application_default_foreground);
 XtSetArg(wargs[1], XmNbackground, application_default_background);
 XtSetValues(w, wargs, 2);

}



/* ****************************************************************************

	 set_widget_colors()

   **************************************************************************** */

void set_widget_colors(w, foreground, background)
	Widget          w;
	int             foreground;
	int             background;
{

	Arg     wargs[2];

 if(w == NULL) return;

 XtSetArg(wargs[0], XmNforeground, foreground);
 XtSetArg(wargs[1], XmNbackground, background);
 XtSetValues(w, wargs, 2);


}

void remove_maxminFile() {

   char buf[80];

   FILE *fp;
   char  path_name[80];

   strcpy(path_name,(char *)getenv("HOME"));

   strcat(path_name,"/.ifp_files/tmpMinMax.dat");


   if ((fp = fopen(path_name,"r")) == NULL)  {

      return;

   }
   sprintf(buf,"rm %s", path_name);
   system(buf); 
  
}

/* ****************************************************************************

	 reset_segment_colors()

   **************************************************************************** */

void reset_segment_colors(w, segmentName, someWidgets)
	Widget                  w;
	char                    *segmentName;
	the_widget_struct       *someWidgets;
{

	Arg     wargs[2];
	int     n;
	int     i;
	int     pixel;
	node    *the_node;



 if(w == NULL) return;

 /* Get the node struct for the selected Segment widget...                                      */
 for(i = 0; i <= sub_group_num; i++)
	{
	if((the_node = find_it(someWidgets->head[i], segmentName)) != NULL)
		{
		whichTree_index = the_node->which_tree;
		break;
		}
	}

 /* Find the correct widget foreground color for the Segment's current flood status...          */
 if(the_node->computed_status == NORMAL)     pixel = get_pixel_by_name(w, flood_color_levels[NORMAL]);
 else if(the_node->computed_status == ALERT) pixel = get_pixel_by_name(w, flood_color_levels[ALERT]);
 else if(the_node->computed_status == FLOOD) pixel = get_pixel_by_name(w, flood_color_levels[FLOOD]);
 else /* UNKNOWN */                          pixel = get_pixel_by_name(w, flood_color_levels[UNKNOWN]);

 set_widget_colors(w, pixel, application_default_background);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/display_funcs.c,v $";
 static char rcs_id2[] = "$Id: display_funcs.c,v 1.6 2006/04/07 13:29:37 aivo Exp $";}
/*  ===================================================  */

}
