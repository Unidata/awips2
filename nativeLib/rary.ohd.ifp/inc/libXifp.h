/* libXifp.h */

#ifndef libXifp_h
#define libXifp_h


#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/Xm.h>
#include <Xm/ToggleBG.h>
#include <Xm/ToggleB.h>
#include <X11/StringDefs.h>
/* #include "libXs.h"           */

/*
 * gfs 950225
 *
 *typedef struct  _date
 *	{
 *	int     month;
 *	int     day;
 *	int     year;
 *	int     hour;
 *	char    time_zone[5];
 *	}       date;
 * gfs 950525
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


typedef struct
	{
	char            name[9];
	Widget          the_widget;
	}       the_list;


#define YES     1
#define NO      0

#define TEN     10
#define TWENTY  20
#define THIRTY  30

#define FORTY   40
#define FIFTY   50
#define SIXTY   60

#define NONE_SELECTED   0

#define RERUN_SEGMENT   0
#define NEXT_SEGMENT    1
#define RUN_FINISHED    2

#define MWM_BORDER_WIDTH                11
#define MWM_MENU_BORDER_HEIGHT          34




int             stay_in_CEX25;
int             segment_run_type;

int             n_list;

Widget  global_toplevel;
Widget  tulplot_w;               /* Shell widget for Tulsa plot table    */
Widget  graph_w;                 /* Shell widget for Tulsa plot graph    */
Widget  other_mods_shellWidget;  /* Shell widget for Other Mods...       */
Widget  prevSelected_date_widget;/* Added by (TEA) 03/10/93...           */

void            fcexec_function();
void            run_nwsrfs();
void            Write_mods();
display_widgets *create_displays();
int             check_for_dates();
void            fill_list();
void            realize_the_windows();
void            map_bb();
void            un_map_bb();
void            exit_run();
the_widgets     *make_display_widgets();
void            make_message_widgets();
void            no_downstream_segment_selected_error();
void            downstream_segment_selected_warning();
void            show_forecast_group();
void            change_hour();
void            change_day();
void            change_month();
void            change_year();
int             check_date();

void            create_help_Dialog();
void            help_event_handler();




/*      *************** GLOBAL CONSTANTS **************         */

unsigned int    Screen_Width;
unsigned int    Screen_Height;
unsigned int    Screen_Depth;

/*      ***********************************************         */
extern int get_apps_defaults(char *, int *, char *, int *);
#endif
