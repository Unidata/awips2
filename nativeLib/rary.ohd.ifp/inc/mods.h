/*  mods.h  */

#ifndef mods_h
#define mods_h

#include <Xm/ArrowB.h>
#include <Xm/Text.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <Xm/Scale.h>
#include <Xm/ToggleB.h>


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



int             numberOfModsSelectedToDeleteFromFile;
int             Selected_QMean_value_position;

/* The following line commented out by gfs 7/26/92 because CodeCenter
 *  would not let multiple .c files be loaded initializing this
 *  global variable.  Also changed line 2192 in mods_cbs.c file
 * (create_modsFromFile_viewer(theDisplayWidgets, "Mods from file", buffer);)
 *  to not reference modsFromFile_title variable.
 *
 * char            *modsFromFile_title = "Mods from file";
 */

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
void            remove_popup();


void            view_mods_from_file();
void            view_mods_from_ofs();
void            create_modsFromFile_viewer();
void            unmanage_widget();
void            restore_newModsList_callback();

void            multiple_increment();
void            multiple_decrement();

void            add_TS_entry();

void            popdown_modsFromFile_Viewer_shell();
void            popdown_viewer_shell();

char            *get_timeZone_code();

void            mod_selected_to_delete();

#endif
