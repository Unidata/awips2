
/*      date_func.c :   used in several applications in the NWSRFS-IFP for              */
/*                      handling dates & date widgets                                   */

#include "libXifp.h"
#include "ifp_atoms.h"
#include "mods.h"
#include "c_call_f/get_ofs_default_tzc.h"

#include  "text_dates.h"




/* ****************************************************************************

	 create_controls()
		creates static text widgets for display of the date & time
		and arrow widgets for incrementing & decrementing the date
		& time

   **************************************************************************** */
MYDATETYPE Adate[3], *pAdate[2]; 

display_widgets *create_controls (bboard, name, which_widget)
	Widget          bboard;
	char            *name;
	int             which_widget;
{
	Widget          form;
	Arg             wargs[2];
	
	char            string[5];
	char            the_zone[5];
	char            *time_zone_code;
	Widget          time_zone;

	int             widget_zero, widget_one, widget_two, widget_three;
	int             type;    /* type of data stored in the window property */
	int             format;  /* format of the stored data */
	int             nitems;  /* number of bytes retrieved */
	int             left;    /* remaining bytes stored in the window */
	int             which_form_widget;
	int             the_month, the_day, the_year, the_hour;
	int             timeZoneCodePropIsPresent = FALSE;
	long            offset = 0;
	long            tp;

	date            *start_date;    /* start of run */
	date            *end_date;      /* end of run */
	date            *end_obs_date;  /* end of observations */
	date            *the_date;
	date            *mods_start_date;   /* modification start date */
	date            *mods_end_date;     /* modification end date */
	date            *mods_end_obs_date; /* modification end of obervations date */

	Display         *display;
	Window          root;

	struct  tm      *time_pointer;
	display_widgets *control;



 display = XtDisplay(bboard);
 root = DefaultRootWindow(display);


 control = (display_widgets *) malloc(sizeof(display_widgets));

/* Create a Form widget to hold date & time displays and controls...            */

 form = XtVaCreateManagedWidget(name, xmRowColumnWidgetClass, bboard,
				XmNorientation, XmHORIZONTAL,
				XmNnumColumns,  5,
				NULL);

 which_form_widget = which_widget;



if(XGetWindowProperty
	(
	display,
	root,
	IFPA_time_zone_code,
	offset,
	(long) 5,
	FALSE,
	(Atom)IFPA_time_zone_code_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&time_zone_code
	) == Success && type == IFPA_time_zone_code_type){
	strcpy(the_zone, time_zone_code);
	timeZoneCodePropIsPresent = TRUE;
	}
else    GET_OFS_DEFAULT_TZC(the_zone);



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
			(Atom)IFPA_run_start_date_type,
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
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);


				XChangeProperty(
				display,
				root,
				IFPA_display_start_date,
				IFPA_display_start_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday - 3;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				/* Check that we have'nt wrapped around the beginning of the month      */
				if(the_day < 1)
					{       /*      Have wrapped the month...                       */
					if(--the_month < 1)
						{       /* Have wrapped the year...                     */
						the_year--;
						the_month = 12;
						}
					the_day = days_in_month(the_month, the_year) + the_day;
					}

				start_date = (date *) malloc(sizeof(date));
				start_date->month = the_month;
				start_date->day = the_day;
				start_date->year = the_year;
				start_date->hour = the_hour;
				strcpy(start_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_start_date,
				IFPA_display_start_date_type,
				8,
				PropModeReplace,
				(unsigned char *)start_date,
				sizeof(date)
				);
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
			(Atom)IFPA_run_end_date_type,
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
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_end_date,
				IFPA_display_end_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday + 5;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				/* Check if we've wrapped around the end of the month...                */
				if(the_day > days_in_month(the_month, the_year))
					{               /*      Have wrapped the month...               */
					the_day = the_day - days_in_month(the_month, the_year);
					if(++the_month > 12)
						{       /*      Have wrapped the year...                */
						the_year++;
						the_month = 1;
						}
					}


				end_date = (date *) malloc(sizeof(date));
				end_date->month = the_month;
				end_date->day = the_day;
				end_date->year = the_year;
				end_date->hour = the_hour;
				strcpy(end_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_end_date,
				IFPA_display_end_date_type,
				8,
				PropModeReplace,
				(unsigned char *)end_date,
				sizeof(date)
				);
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
			(Atom)IFPA_run_end_obs_date_type,
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
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_end_obs_date,
				IFPA_display_end_obs_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				end_obs_date = (date *) malloc(sizeof(date));
				end_obs_date->month = the_month;
				end_obs_date->day = the_day;
				end_obs_date->year = the_year;
				end_obs_date->hour = the_hour;
				strcpy(end_obs_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_end_obs_date,
				IFPA_display_end_obs_date_type,
				8,
				PropModeReplace,
				(unsigned char *)end_obs_date,
				sizeof(date)
				);
				}
			break;





		case FORTY:
			if(XGetWindowProperty
			(
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
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_start_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_start_date,
				IFPA_mods_start_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				mods_start_date = (date *) malloc(sizeof(date));
				mods_start_date->month = the_month;
				mods_start_date->day = the_day;
				mods_start_date->year = the_year;
				mods_start_date->hour = the_hour;
				strcpy(mods_start_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_start_date,
				IFPA_mods_start_date_type,
				8,
				PropModeReplace,
				(unsigned char *)mods_start_date,
				sizeof(date)
				);
				}
			break;


		case FIFTY:
			if(XGetWindowProperty
			(
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
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_end_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_end_date,
				IFPA_mods_end_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				mods_end_date = (date *) malloc(sizeof(date));
				mods_end_date->month = the_month;
				mods_end_date->day = the_day;
				mods_end_date->year = the_year;
				mods_end_date->hour = the_hour;
				strcpy(mods_end_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_end_date,
				IFPA_mods_end_date_type,
				8,
				PropModeReplace,
				(unsigned char *)mods_end_date,
				sizeof(date)
				);
				}
			break;


		case SIXTY:
			if(XGetWindowProperty
			(
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
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_end_obs_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_end_obs_date,
				IFPA_mods_end_obs_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				mods_end_obs_date = (date *) malloc(sizeof(date));
				mods_end_obs_date->month = the_month;
				mods_end_obs_date->day = the_day;
				mods_end_obs_date->year = the_year;
				mods_end_obs_date->hour = the_hour;
				strcpy(mods_end_obs_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_end_obs_date,
				IFPA_mods_end_obs_date_type,
				8,
				PropModeReplace,
				(unsigned char *)mods_end_obs_date,
				sizeof(date)
				);
				}
			break;

	}


	XFree(the_date);


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


 XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET)) ;
 control->month = XtCreateManagedWidget("month", xmPushButtonWidgetClass, form, wargs, 1);

 sprintf(string, "%d", the_day);
 XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET));
 control->day = XtCreateManagedWidget("day", xmPushButtonWidgetClass, form,  wargs, 1);

 sprintf(string, "%d", the_year);
 XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET));
 control->year = XtCreateManagedWidget("year", xmPushButtonWidgetClass, form,  wargs, 1);

 sprintf(string, "%d", the_hour);
 XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET));
 control->time = XtCreateManagedWidget("time", xmPushButtonWidgetClass, form, wargs, 1);

 XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR(the_zone, XmSTRING_DEFAULT_CHARSET));
 time_zone = XtCreateManagedWidget("zone", xmPushButtonWidgetClass, form, wargs, 1);

 widget_one = which_widget + 1;
 widget_two = which_widget + 2;
 widget_three = which_widget + 3;
 widget_zero = which_widget;

/*      Callbacks to select the date & time widgets                                             */

 XtAddCallback(control->month, XmNactivateCallback, select_widget, (XtPointer)widget_one);
 XtAddCallback(control->day, XmNactivateCallback, select_widget, (XtPointer)widget_two);
 XtAddCallback(control->year, XmNactivateCallback, select_widget, (XtPointer)widget_three);
 XtAddCallback(control->time, XmNactivateCallback, select_widget, (XtPointer)widget_zero);
 


 return(control);

}



/*--------------------------------------------------------------------*/

/**************************************************************************** */
Widget UpArrow, DownArrow, SelFlag;
Widget  toplevel, 
	form, 
	rowcol1, 
	rowcol2, 
	button, 
	arrUp,
	arrDown,
	labelw[6], 
	StartTimew[6],
	EndTimew[6] ;
        
display_widgets *create_ctrls_text (bboard, name, which_widget,indx)
	Widget          bboard;
	char            *name;
	int             which_widget;
        int             indx;
{
	Widget          form;
	Arg             wargs[5];        
	char            string[5];
	char            the_zone[5];
	char            *time_zone_code;
	Widget          time_zone;

	int             widget_zero, widget_one, widget_two, widget_three;
	int             type;    /* type of data stored in the window property */
	int             format;  /* format of the stored data */
	int             nitems;  /* number of bytes retrieved */
	int             left;    /* remaining bytes stored in the window */
	int             which_form_widget;
	int             the_month, the_day, the_year, the_hour;
	int             timeZoneCodePropIsPresent = FALSE;
	long            offset = 0;
	long            tp;

	date            *start_date;    /* start of run */
	date            *end_date;      /* end of run */
	date            *end_obs_date;  /* end of observations */
	date            *the_date;
	date            *mods_start_date;   /* modification start date */
	date            *mods_end_date;     /* modification end date */
	date            *mods_end_obs_date; /* modification end of obervations date */

	Display         *display;
	Window          root;

	struct  tm      *time_pointer;
	display_widgets *control;

        int i, n;

        char    *sLabels[] = {"Month", "Day", "Year", "Hour"};
        static  maxLen[6]  = { 3,       2,     4,      2,    };









 display = XtDisplay(bboard);
 root = DefaultRootWindow(display);


 control = (display_widgets *) malloc(sizeof(display_widgets));

/* Create a Form widget to hold date & time displays and controls...            */

 form = XtVaCreateManagedWidget(name, xmRowColumnWidgetClass, bboard,
				XmNorientation, XmHORIZONTAL,
				XmNnumColumns,  5,
				NULL);

 which_form_widget = which_widget;

 
if(XGetWindowProperty
	(
	display,
	root,
	IFPA_time_zone_code,
	offset,
	(long) 5,
	FALSE,
	(Atom)IFPA_time_zone_code_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&time_zone_code
	) == Success && type == IFPA_time_zone_code_type){
	strcpy(the_zone, time_zone_code);
	timeZoneCodePropIsPresent = TRUE;
	}
else    GET_OFS_DEFAULT_TZC(the_zone);



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
			(Atom)IFPA_run_start_date_type,
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
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);


				XChangeProperty(
				display,
				root,
				IFPA_display_start_date,
				IFPA_display_start_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday - 3;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				/* Check that we have'nt wrapped around the beginning of the month      */
				if(the_day < 1)
					{       /*      Have wrapped the month...                       */
					if(--the_month < 1)
						{       /* Have wrapped the year...                     */
						the_year--;
						the_month = 12;
						}
					the_day = days_in_month(the_month, the_year) + the_day;
					}

				start_date = (date *) malloc(sizeof(date));
				start_date->month = the_month;
				start_date->day = the_day;
				start_date->year = the_year;
				start_date->hour = the_hour;
				strcpy(start_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_start_date,
				IFPA_display_start_date_type,
				8,
				PropModeReplace,
				(unsigned char *)start_date,
				sizeof(date)
				);
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
			(Atom)IFPA_run_end_date_type,
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
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_end_date,
				IFPA_display_end_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday + 5;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				/* Check if we've wrapped around the end of the month...                */
				if(the_day > days_in_month(the_month, the_year))
					{               /*      Have wrapped the month...               */
					the_day = the_day - days_in_month(the_month, the_year);
					if(++the_month > 12)
						{       /*      Have wrapped the year...                */
						the_year++;
						the_month = 1;
						}
					}


				end_date = (date *) malloc(sizeof(date));
				end_date->month = the_month;
				end_date->day = the_day;
				end_date->year = the_year;
				end_date->hour = the_hour;
				strcpy(end_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_end_date,
				IFPA_display_end_date_type,
				8,
				PropModeReplace,
				(unsigned char *)end_date,
				sizeof(date)
				);
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
			(Atom)IFPA_run_end_obs_date_type,
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
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_end_obs_date,
				IFPA_display_end_obs_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				end_obs_date = (date *) malloc(sizeof(date));
				end_obs_date->month = the_month;
				end_obs_date->day = the_day;
				end_obs_date->year = the_year;
				end_obs_date->hour = the_hour;
				strcpy(end_obs_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_display_end_obs_date,
				IFPA_display_end_obs_date_type,
				8,
				PropModeReplace,
				(unsigned char *)end_obs_date,
				sizeof(date)
				);
				}
			break;





		case FORTY:
			if(XGetWindowProperty
			(
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
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_start_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_start_date,
				IFPA_mods_start_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				mods_start_date = (date *) malloc(sizeof(date));
				mods_start_date->month = the_month;
				mods_start_date->day = the_day;
				mods_start_date->year = the_year;
				mods_start_date->hour = the_hour;
				strcpy(mods_start_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_start_date,
				IFPA_mods_start_date_type,
				8,
				PropModeReplace,
				(unsigned char *)mods_start_date,
				sizeof(date)
				);
				}
			break;


		case FIFTY:
			if(XGetWindowProperty
			(
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
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_end_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_end_date,
				IFPA_mods_end_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				mods_end_date = (date *) malloc(sizeof(date));
				mods_end_date->month = the_month;
				mods_end_date->day = the_day;
				mods_end_date->year = the_year;
				mods_end_date->hour = the_hour;
				strcpy(mods_end_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_end_date,
				IFPA_mods_end_date_type,
				8,
				PropModeReplace,
				(unsigned char *)mods_end_date,
				sizeof(date)
				);
				}
			break;


		case SIXTY:
			if(XGetWindowProperty
			(
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
			(unsigned char **)&the_date
			) == Success && type == IFPA_run_end_obs_date_type){

				the_month = the_date->month;
				the_day = the_date->day;
				the_year = the_date->year;
				the_hour = the_date->hour;
				if(timeZoneCodePropIsPresent == TRUE) strcpy(the_date->time_zone, the_zone);
				else strcpy(the_zone, the_date->time_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_end_obs_date,
				IFPA_mods_end_obs_date_type,
				8,
				PropModeReplace,
				(unsigned char *)the_date,
				sizeof(date)
				);
				}
			else
				{
				time(&tp);
				time_pointer = localtime(&tp);

				the_month = time_pointer->tm_mon + 1;
				the_day = time_pointer->tm_mday;
				the_year = 1900 + time_pointer->tm_year;
				the_hour = time_pointer->tm_hour;

				mods_end_obs_date = (date *) malloc(sizeof(date));
				mods_end_obs_date->month = the_month;
				mods_end_obs_date->day = the_day;
				mods_end_obs_date->year = the_year;
				mods_end_obs_date->hour = the_hour;
				strcpy(mods_end_obs_date->time_zone, the_zone);

				XChangeProperty(
				display,
				root,
				IFPA_mods_end_obs_date,
				IFPA_mods_end_obs_date_type,
				8,
				PropModeReplace,
				(unsigned char *)mods_end_obs_date,
				sizeof(date)
				);
				}
			break;

	}


	XFree(the_date);


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
                        

 SelFlag   = 0;
    
       pAdate[indx] = &Adate[indx];
       pAdate[indx]->year  = the_year;
       pAdate[indx]->day   = the_day;
       pAdate[indx]->hour  = the_hour;
        
       pAdate[indx]->month = the_month;
       pAdate[indx]->sMonth = ( char *)GetMonthByNum( pAdate[indx]->month);

  

 n = 0;
 XtSetArg(wargs[n], XmNwidth,50); n++;
 XtSetArg(wargs[n], XmNheight, 20); n++;
 XtSetArg(wargs[n], XmNmaxLength, 3); n++;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET)); n++;
 
 control->month = XtCreateManagedWidget("month", xmTextWidgetClass, form, wargs, n);

 
 
 sprintf(string, "%d", the_day);
 
 n = 0;
 XtSetArg(wargs[n], XmNwidth,45); n++;
 XtSetArg(wargs[n], XmNheight, 20); n++;
 XtSetArg(wargs[n], XmNmaxLength, 2); n++;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET)); n++;

 control->day = XtCreateManagedWidget("day", xmTextWidgetClass, form,  wargs, n);
 
 
 sprintf(string, "%d", the_year);
 n = 0;
 XtSetArg(wargs[n], XmNwidth,55); n++;
 XtSetArg(wargs[n], XmNheight, 20); n++;
 XtSetArg(wargs[n], XmNmaxLength, 4); n++;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET)); n++;
 control->year = XtCreateManagedWidget("year", xmTextWidgetClass, form,  wargs, n);
 
 
 sprintf(string, "%d", the_hour);
 n = 0;
 XtSetArg(wargs[n], XmNwidth,45); n++;
 XtSetArg(wargs[n], XmNheight, 20); n++;
 XtSetArg(wargs[n], XmNmaxLength, 2); n++;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET)); n++;
 control->time = XtCreateManagedWidget("time", xmTextWidgetClass, form, wargs, n);
 
 XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR(the_zone, XmSTRING_DEFAULT_CHARSET));
 time_zone = XtCreateManagedWidget("zone", xmPushButtonWidgetClass, form, wargs, 1);

 pAdate[indx]->w[0] = control->month;
 pAdate[indx]->w[1] = control->day;
 pAdate[indx]->w[2] = control->year;
 pAdate[indx]->w[3] = control->time;
 
 widget_one = which_widget + 1;
 widget_two = which_widget + 2;
 widget_three = which_widget + 3;
 widget_zero = which_widget;

/*      Callbacks to select the date & time widgets                                             */


 initializeDate(indx);
 AddAllCallBacks(indx);
 


 return(control);

}





/* ****************************************************************************

	 get_date()
		gets the "display_date" Atom using XGetWindowProperty
		and returns a pointer to that date to increment/decrement

   **************************************************************************** */

date *get_date (w, display_selected)
	Widget          w;
	int             display_selected;
{
	long            offset = 0;
	Display         *display;
	Window          root;
	date            *start_date, *end_date, *end_obs_date;
	date            *mods_start_date, *mods_end_date, *mods_end_obs_date;
	date            *the_date_returned;
	int             type, format, nitems, left;
	int             which_form_widget;

	display = XtDisplay(w);
	root = DefaultRootWindow(display);


	which_form_widget = display_selected/10;

	switch(which_form_widget)
		{
		case 1:
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
					the_date_returned = start_date;
			break;

		case 2:
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
					the_date_returned = end_date;
			break;

		case 3:
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
					the_date_returned = end_obs_date;
			break;

		case 4:
			if(XGetWindowProperty
				(
				display,
				root,
				IFPA_mods_start_date,
				offset,
				(long) sizeof(date),
				FALSE,
				(Atom)IFPA_mods_start_date_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&mods_start_date
				) == Success && type == IFPA_mods_start_date_type)
					the_date_returned = mods_start_date;
			break;

		case 5:
			if(XGetWindowProperty
				(
				display,
				root,
				IFPA_mods_end_date,
				offset,
				(long) sizeof(date),
				FALSE,
				(Atom)IFPA_mods_end_date_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&mods_end_date
				) == Success && type == IFPA_mods_end_date_type)
					the_date_returned = mods_end_date;
			break;

		case 6:
			if(XGetWindowProperty
				(
				display,
				root,
				IFPA_mods_end_obs_date,
				offset,
				(long) sizeof(date),
				FALSE,
				(Atom)IFPA_mods_end_obs_date_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&mods_end_obs_date
				) == Success && type == IFPA_mods_end_obs_date_type)
					the_date_returned = mods_end_obs_date;
			break;


		default:
			break;
		}

	return (the_date_returned);
}



/* ****************************************************************************

	 change_widget()
		post the property change request and change the value displayed
		in the display widget

   **************************************************************************** */

void change_widget (the_display_widget, widget_value, the_date, display_selected)
	Widget          the_display_widget;
	int             widget_value;
	date            *the_date;
	int             display_selected;
{
	Display         *display;
	Window          root;
	char            display_value[5];
	Arg             wargs[1];

	int             which_form_widget;

	display = XtDisplay(the_display_widget);
	root = DefaultRootWindow(display);


	which_form_widget = display_selected/10;

	switch(which_form_widget)
		{
		case 1:
			  XChangeProperty
					(
					display,
					root,
					IFPA_display_start_date,
					IFPA_display_start_date_type,
					8,
					PropModeReplace,
					(unsigned char *)the_date,
					sizeof(date)
					);
			break;

		case 2:
			  XChangeProperty
					(
					display,
					root,
					IFPA_display_end_date,
					IFPA_display_end_date_type,
					8,
					PropModeReplace,
					(unsigned char *)the_date,
					sizeof(date)
					);
			break;

		case 3:
			  XChangeProperty
					(
					display,
					root,
					IFPA_display_end_obs_date,
					IFPA_display_end_obs_date_type,
					8,
					PropModeReplace,
					(unsigned char *)the_date,
					sizeof(date)
					);
			break;

		case 4:
			  XChangeProperty
					(
					display,
					root,
					IFPA_mods_start_date,
					IFPA_mods_start_date_type,
					8,
					PropModeReplace,
					(unsigned char *)the_date,
					sizeof(date)
					);
			break;

		case 5:
			  XChangeProperty
					(
					display,
					root,
					IFPA_mods_end_date,
					IFPA_mods_end_date_type,
					8,
					PropModeReplace,
					(unsigned char *)the_date,
					sizeof(date)
					);
			break;

		case 6:
			  XChangeProperty
					(
					display,
					root,
					IFPA_mods_end_obs_date,
					IFPA_mods_end_obs_date_type,
					8,
					PropModeReplace,
					(unsigned char *)the_date,
					sizeof(date)
					);
			break;

		default:
			break;
		}

	if(display_selected == START_MONTH        ||
	   display_selected == END_MONTH          ||
	   display_selected == END_OBS_MONTH      ||
	   display_selected == MODS_START_MONTH   ||
	   display_selected == MODS_END_MONTH     ||
	   display_selected == MODS_END_OBS_MONTH   )
		{
		show_month (the_display_widget, widget_value);
		}
	else
		{
		sprintf(display_value, "%d", widget_value);
		XtSetArg(wargs[0], XmNlabelString,
			XmStringCreate(display_value, XmSTRING_DEFAULT_CHARSET));
		XtSetValues(the_display_widget, wargs, 1);
		}
}



/* ****************************************************************************

	 show_month()
		changes the value displayed in the 'month' display widget

   **************************************************************************** */

void show_month (the_display_widget, which_month)
	Widget          the_display_widget;
	int             which_month;
{
	Arg             wargs[1];
	char            string[5];


     switch(which_month)
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
		XtSetValues(the_display_widget, wargs, 1);
}





/* ****************************************************************************

	 invert_widget()
		invert the widget's foreground & background colors

   **************************************************************************** */

void invert_widget(Widget w)
{
	Pixel                   fg, bg;
	XWindowAttributes       attributes;


 if(w == NULL)
 {
	return;
 }
 if(XtIsRealized(w))
	{
	XGetWindowAttributes(XtDisplay(w), XtWindow(w), &attributes);
	if(attributes.map_state == IsUnmapped) return;
	}
 else   return;

/*      Get the widget's current colors         */
 XtVaGetValues(w, XtNforeground, &fg, XtNbackground, &bg, NULL);

/*      Reverse and set the new colors          */
 XtVaSetValues(w, XtNforeground, bg, XtNbackground, fg, NULL);

}


/* ****************************************************************************

	 select_widget()

   **************************************************************************** */

void select_widget (w, which_widget, call_data)
	Widget                  w;
	int                     which_widget;
	XmAnyCallbackStruct     *call_data;
{

	Display         *display;
	Window          root;



/* printf("Inside 'select_widget()'...\n"); */

display = XtDisplay(w);
root = DefaultRootWindow(display);

if(w == prevSelected_date_widget)
	{
	invert_widget(w);
	prevSelected_date_widget = NULL;

	which_widget = NONE_SELECTED;

	XChangeProperty
		(
		display,
		root,
		IFPA_display_selected,
		IFPA_display_selected_type,
		8,
		PropModeReplace,
		(unsigned char *)&which_widget,
		4
		);

	return;
	}

invert_widget(w);
if(prevSelected_date_widget != NULL) invert_widget(prevSelected_date_widget);
prevSelected_date_widget = w;


XChangeProperty
	(
	display,
	root,
	IFPA_display_selected,
	IFPA_display_selected_type,
	8,
	PropModeReplace,
	(unsigned char *)&which_widget,
	4
	);

}



/* ****************************************************************************

	 increment()
		increments the date & time for the display widgets and calls
		change_widget to display & post the property  change

   **************************************************************************** */

void increment(w, date_widget, call_data)
	Widget                  w;
	the_widgets             *date_widget;
	XmAnyCallbackStruct     *call_data;
{
	date            *start_date, *end_date, *end_obs_date;
	Display         *display;
	Window          root;

	long            offset = 0;
	int             delta_t;
	int             *display_selected;
	int             type, format, nitems, left;

	display = XtDisplay(w);
	root = DefaultRootWindow(display);




if(XGetWindowProperty
		(
		display,
		root,
		IFPA_display_selected,
		offset,
		(long) 4,
		FALSE,
		(Atom)IFPA_display_selected_type,
		(Atom *)&type,
		(int *)&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&display_selected
		) == Success && type == IFPA_display_selected_type)
	{
	if(*display_selected != 0)
			{

			delta_t = get_time_step(w);

			switch(*display_selected)
				{
				case START_MONTH:
				case MODS_START_MONTH:
					start_date = get_date(w, *display_selected);
					change_month(1, *display_selected/10,
						start_date, date_widget);
					break;

				case START_DAY:
				case MODS_START_DAY:
					start_date = get_date(w, *display_selected);
					change_day(1, *display_selected/10,
						start_date, date_widget);
					break;

				case START_YEAR:
				case MODS_START_YEAR:
					start_date = get_date(w, *display_selected);
					change_year(1, *display_selected/10,
						start_date, date_widget);
					break;

				case START_TIME:
				case MODS_START_TIME:
					start_date = get_date(w, *display_selected);
					change_hour(delta_t, *display_selected/10,
						start_date, date_widget);
					break;

				case END_MONTH:
				case MODS_END_MONTH:
					end_date = get_date(w, *display_selected);
					change_month(1, *display_selected/10,
						end_date, date_widget);
					break;

				case END_DAY:
				case MODS_END_DAY:
					end_date = get_date(w, *display_selected);
					change_day(1, *display_selected/10,
						end_date, date_widget);
					break;

				case END_YEAR:
				case MODS_END_YEAR:
					end_date = get_date(w, *display_selected);
					change_year(1, *display_selected/10,
						end_date, date_widget);
					break;

				case END_TIME:
				case MODS_END_TIME:
					end_date = get_date(w, *display_selected);
					change_hour(delta_t, *display_selected/10,
						end_date, date_widget);
					break;

				case END_OBS_MONTH:
				case MODS_END_OBS_MONTH:
					end_obs_date = get_date(w, *display_selected);
					change_month(1, *display_selected/10,
						end_obs_date, date_widget);
					break;


				case END_OBS_DAY:
				case MODS_END_OBS_DAY:
					end_obs_date = get_date(w, *display_selected);
					change_day(1, *display_selected/10,
						end_obs_date, date_widget);
					break;

				case END_OBS_YEAR:
				case MODS_END_OBS_YEAR:
					end_obs_date = get_date(w, *display_selected);
					change_year(1, *display_selected/10,
						end_obs_date, date_widget);
					break;

				case END_OBS_TIME:
				case MODS_END_OBS_TIME:
					end_obs_date = get_date(w, *display_selected);
					change_hour(delta_t, *display_selected/10,
						end_obs_date, date_widget);
					break;

				default:
					break;

				}       /*      End "switch" statement          */
			}
	else    XBell(display, 0);    /*      SysBeep - no widget selected    */
	}
	else    XBell(display, 0);    /*      SysBeep - no property
								available */
}


/* ****************************************************************************

	 decrement()
		decrements the date & time for the display widgets and calls
		change_widget to display & post the property  change

   **************************************************************************** */

void decrement(Widget w, the_widgets *date_widget, XmAnyCallbackStruct *call_data)
{
	date            *start_date, *end_date, *end_obs_date;
	Display         *display;
	Window          root;

	long            offset = 0;
	int             delta_t;
	int             *display_selected;
	int             type, format, nitems, left;

	display = XtDisplay(w);
	root = DefaultRootWindow(display);


if(XGetWindowProperty
	(
	display,
	root,
	IFPA_display_selected,
	offset,
	(long) 4,
	FALSE,
	(Atom)IFPA_display_selected_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&display_selected
	) == Success && type == IFPA_display_selected_type)
	{
	if(*display_selected != 0)
			{

			delta_t = get_time_step(w);

			switch(*display_selected)
				{
				case START_MONTH:
				case MODS_START_MONTH:
					start_date = get_date(w, *display_selected);
					change_month(-1, *display_selected/10,
						start_date, date_widget);
					break;

				case START_DAY:
				case MODS_START_DAY:
					start_date = get_date(w, *display_selected);
					change_day(-1, *display_selected/10,
						start_date, date_widget);
					break;

				case START_YEAR:
				case MODS_START_YEAR:
					start_date = get_date(w, *display_selected);
					change_year(-1, *display_selected/10,
						start_date, date_widget);
					break;

				case START_TIME:
				case MODS_START_TIME:
					start_date = get_date(w, *display_selected);
					change_hour(-delta_t, *display_selected/10,
						start_date, date_widget);
					break;

				case END_MONTH:
				case MODS_END_MONTH:
					end_date = get_date(w, *display_selected);
					change_month(-1, *display_selected/10,
						end_date, date_widget);
					break;

				case END_DAY:
				case MODS_END_DAY:
					end_date = get_date(w, *display_selected);
					change_day(-1, *display_selected/10,
						end_date, date_widget);
					break;

				case END_YEAR:
				case MODS_END_YEAR:
					end_date = get_date(w, *display_selected);
					change_year(-1, *display_selected/10,
						end_date, date_widget);
					break;

				case END_TIME:
				case MODS_END_TIME:
					end_date = get_date(w, *display_selected);
					change_hour(-delta_t, *display_selected/10,
						end_date, date_widget);
					break;

				case END_OBS_MONTH:
				case MODS_END_OBS_MONTH:
					end_obs_date = get_date(w, *display_selected);
					change_month(-1, *display_selected/10,
						end_obs_date, date_widget);
					break;


				case END_OBS_DAY:
				case MODS_END_OBS_DAY:
					end_obs_date = get_date(w, *display_selected);
					change_day(-1, *display_selected/10,
						end_obs_date, date_widget);
					break;

				case END_OBS_YEAR:
				case MODS_END_OBS_YEAR:
					end_obs_date = get_date(w, *display_selected);
					change_year(-1, *display_selected/10,
						end_obs_date, date_widget);
					break;

				case END_OBS_TIME:
				case MODS_END_OBS_TIME:
					end_obs_date = get_date(w, *display_selected);
					change_hour(-delta_t, *display_selected/10,
						end_obs_date, date_widget);
					break;

				default:
					break;

				}       /*      End "switch" statement          */
			}
	else    XBell(display, 0);    /*      SysBeep - no widget selected    */
	}
	else    XBell(display, 0);    /*      SysBeep - no property available */
}




/* ****************************************************************************

	 exit_program()

   **************************************************************************** */

void exit_program(w, client_data, call_data)
	Widget                  w;
	caddr_t                 client_data;
	XmAnyCallbackStruct     call_data;
{

XFlush(XtDisplay(w));
exit(0);                        /*      Exit the program...     */

}



/* ******************************************************************************************

	get_time_step()
		This function gets & returns the time step used in the NWSRFS simulation
		for the current segment.

   ****************************************************************************************** */

int     get_time_step(w)
	Widget          w;
{
/*
 *  This is temporary, we still need to get the simulation Delta T...
 */
 return(6);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/date_funcs.c,v $";
 static char rcs_id2[] = "$Id: date_funcs.c,v 1.7 2006/04/07 14:10:33 aivo Exp $";}
/*  ===================================================  */

}
