/* run_dates.h */

#ifndef run_dates_h
#define run_dates_h

/*
#include <time.h>
*/

#include <Xm/ArrowB.h>


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



int             which_date_widget_selected();
void            set_dates();
void            increment();
void            decrement();
display_widgets *create_controls();
int             update_selection();

void            remove_popup();
int             check_date();
void            select_widget();
date            *get_date();
void            change_widget();
void            show_month();
void            invert_widget();

void            change_hour();
void            change_day();
void            change_month();
void            change_year();
int             days_in_month();
Widget          Xifp_create_quit_button();
void            popdown_setDates_shell();


void            update_date_display();

#endif
