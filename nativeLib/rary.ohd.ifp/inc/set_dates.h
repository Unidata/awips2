
/* ************************************************************

	set_dates.h

		include file for:

				set_dates.c
				date_funcs.c
				check_dates.c
				write_dates.c

	Coded:          ??/??/90
	Last modified:  6/04/91

	By:             Thomas Adams
	Affiliation:    NOAA/NWS/Office of Hydrology/HRL


   ************************************************************ */

#ifndef set_dates_h
#define set_dates_h

#include <stdio.h>
#include <time.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/BulletinB.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/Xm.h>
#include <X11/StringDefs.h>

/*
 * gfs 950225
 * typedef struct _date
 * 	{
 * 	int     month;
 * 	int     day;
 *	int     year;
 *	int     hour;
 *	char    time_zone[5];
 *	}       date;
 * gfs 950225
 */

#include "ifp_inc/Date.h"

typedef struct  _display_widgets
	{
	Widget  month;
	Widget  day;
	Widget  year;
	Widget  time;
	}       display_widgets;


typedef struct
	{
	display_widgets         *start;
	display_widgets         *end;
	display_widgets         *end_obs;
	}       the_widgets;

#define YES     1
#define NO              0
#define MAX_RUN_PERIOD  744

#define TEN     10
#define TWENTY  20
#define THIRTY  30

#define FORTY   40
#define FIFTY   50
#define SIXTY   60

#define NONE_SELECTED   0

#define START_TIME      10
#define START_MONTH     11
#define START_DAY       12
#define START_YEAR      13

#define END_TIME        20
#define END_MONTH       21
#define END_DAY         22
#define END_YEAR        23

#define END_OBS_TIME    30
#define END_OBS_MONTH   31
#define END_OBS_DAY     32
#define END_OBS_YEAR    33

#define MODS_START_TIME      40
#define MODS_START_MONTH     41
#define MODS_START_DAY       42
#define MODS_START_YEAR      43

#define MODS_END_TIME        50
#define MODS_END_MONTH       51
#define MODS_END_DAY         52
#define MODS_END_YEAR        53

#define MODS_END_OBS_TIME    60
#define MODS_END_OBS_MONTH   61
#define MODS_END_OBS_DAY     62
#define MODS_END_OBS_YEAR    63



Display         *dpy;
Window          root;
Widget          popup_shell, popup_text;
Widget          prevSelected_date_widget;


void            create_help_Dialog();

int             date_rootWindow_properties_present();

void            exit_set_dates();
void            remove_popup();
int             check_date();
void            set_dates();
display_widgets *create_controls();
void            select_widget();
date            *get_date();
void            increment();
void            decrement();
void            change_widget();
void            show_month();
int             update_selection();
void            invert_widget();

void            change_hour();
void            change_day();
void            change_month();
void            change_year();
int             days_in_month();
Widget          Xifp_create_quit_button();
Widget          create_quit_button();

void            multiple_increment();
void            multiple_decrement();


int             date_compare();
/*void            popup_Bad_Date_dialog();*/
void            popup_Bad_Date_dialog(Widget ,char  [] );
void            write_ifp_run_dates();  /* start_date, end_obs_date, end_date   */

#endif
