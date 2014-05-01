
/* ********************************************************************************

	ifp_globals.h
		global constants, variables, functions for the NWSRFS Interactive
		Forecast Program

   ******************************************************************************** */

#ifndef ifp_globals_h
#define ifp_globals_h

#include <Xm/TextF.h>
#include "Mods_flags.h"

void     do_AEICQN();
void     do_AESCChng();
void     do_AIAdj();
void     do_APICQN();
void     do_BaseF();
void     do_BFRChng();
void     do_CBaseF();
void     do_IgnoreTS();
void     do_MFC();
void     do_RainSnow();
void     do_ROChng();
void     do_ROMult();
void     do_RRIChng();
void     do_RRIMult();
void     do_SacBaseF();
void     do_SetMsng();
void     do_SetQMean();
void     do_UHgChng();
void     do_WEChng();
void     mods_pl();

#define SKIP                            -1
#define RAIN                            0
#define SNOW                            1
#define ALL                             0
#define POOL                            1
#define MEAN                            2
#define INSTANTANEOUS                   3
#define NO_VALUE_SELECTED               -9
#define MAX_MODS                        100
#define MAX_NUM_SETQMEAN_VALUES         744
#define ALL_OPERATIONS                  -99
#define MAX_CARD_LENGTH                 70
#define MAX_NUMBER_OF_SELECTABLE_DATES  20


double          atof();
int             handle_PropertyNotify_event();
void            handle_KeyPress_event();
mod_data        *get_the_data();
void            handleBackSpace();
void            showTimeSeriesText();
void            check_mod_value();
void            get_mod_limits();
Widget          popup_warning_message();
void            IFP_Map_popdown_shell();
void            popdown_shell();
void            popdown_warning_shell();        /* This function is different from the one below        */
void            popdown_error_shell();
void            popdown_op_error_shell();
void            popdown_warningShell();         /* ... that is, this one !!                             */
void            popdown_errorShell();
void            popdown_tooManyQMeans_shell();
void            popup_too_many_dates_errorDialog();
void            cancel_mods_shell();
int             whichDateWidgetSelected();
int             check_SETQMEAN_entry();
void            parse_expression();
void            nextButton_event_handler();
void            text_entry_eventHandler();
date            *convert_display_widgets_to_date();
void            convert_string_to_date();
void            getModTSdates();
void            save_ModData_from_controlMenu();
void            save_ModData_from_warningDialog();
void            saveModData();
void            dont_save_mod();
int             test_last_two_mods();
int             compare_A1_modArrays();
int             compare_A2_modArrays();
int             compare_B1_modArrays();
int             compare_B2_modArrays();
int             compare_B3_modArrays();
void            reset_InputFocus_to_DefaultWindow();
void            show_mod_not_saved_Dialog();
void            mod_not_saved_warningDialog();
void            call_mod_create_func();
void            notify_mod_the_same_Dialog();
void            no_flows_entered_errorDialog();
void            get_new_mods();
void            show_selected_mod();
void            show_all_mods();
void            show_all_mods_of_a_type();
void            show_mods_selected();
void            remove_mods_selected();
void            flag_mod_selected();
void            popdown_modsList_shell();
void            decrement_errorMessageFlag();   /* Callback to decrement the counter for a dialog box   */
void            cancel_last_mod();              /* Deletes the last mod entry...                        */
void            cancel_all_mods();              /* Deletes all mods entered in the last creation cycle  */
void            handle_done_mods();             /* callback for 'done_mods' button to destroy toplevel  */
int             check_date_list();              /* Checks if at least 1 date was selected for SETMSNG   */
						/*      & RAINSNOW                                      */
Widget          create_mod_viewer();
void            add_item_to_new_modsList();
void            delete_last_item_from_new_modsList();
void            set_dont_save_mod_flag();


/*      *************** GLOBAL FLAGS **************     */

int             warningPoppedUp;
int             errorShellPoppedUp;
int             op_errorShellPoppedUp;
int             haveBeenWarned;
int             errorMessageDisplayed;
int             currentModSaved;
int             modsWindowUp;
int             modsOperationSelected;
int             testForTSChange;
int             tsPopupHasBeenUp;
int             tsTest;
int             RainSnow;
int             IgnoreTS;
int             stay_in_other_Mods;     /* Main event loop flag...      */
int             cancelAllMods;
int             dontSaveMod;
int             time_series_change_made;


int             mods_SAC_units;         /* If 0: ENGLISH, 1: METRIC     */
int             mods_API_units;         /* If 0: ENGLISH, 1: METRIC     */
int             mods_general_units;     /* If 0: ENGLISH, 1: METRIC     */
int             NWSRFS_general_units;   /* If 0: ENGLISH, 1: METRIC     */

float           LowerWarning_limit;     /* 'Other' Mods value limits    */
float           UpperWarning_limit;     /* 'Other' Mods value limits    */
float           LowerError_limit;       /* 'Other' Mods value limits    */
float           UpperError_limit;       /* 'Other' Mods value limits    */

int             Lower_Warning_inclusive;
int             Upper_Warning_inclusive;
int             Lower_Error_inclusive;
int             Upper_Error_inclusive;


char            *timeSeriesTextBuffer;
int             dialogMessageIsUp;      /* Flag (TRUE or FALSE) indicating a Message Dialog is active   */
int             leftParen_count;
int             rightParen_count;
int             isExpression;           /* Flag (YES or NO) indicating an expression for SETQMEAN...    */
int             num_prev_QMean_values;  /* Number of values previously entered in SETQMEAN              */
int             available_QMean_values; /* Number of values currently available to  SETQMEAN            */
int             currentModIndex;        /* Index for *Mod_array[]...                                    */
int             oldModIndex;            /* Index for *Mod_array[] entering the current Mods creation    */
					/*      cycle; if 'Cancel All' is selected, it is used to       */
					/*      set all Mod_array[] pointers to NULL beginning with     */
					/*      Mod_array[oldModIndex]...                               */
int             modListItem_selected;   /* The item_position of the item selected in the                */
					/*      new_mods_listWidget...                                  */


int             deciding_to_save_modDeletions;
int             make_modDeletions_fromFile;


/*      ***************** END GLOBAL FLAGS **************     */
#ifndef mods_info_h    /* ifndef added by dp - 950312 */
#define mods_info_h

#include "Mods_info.h"  /* added by gfs - 950225 */
#endif /* mods_info_h */

ModInfo   *Mod_array[MAX_MODS + 1];   /* Array to hold data for the Run-time Mods             */


Widget          global_toplevel, warning_popup;
/*
 * The following 2 variables are only used in mods.c.
 * CodeCenter objects to them being initialized everywhere
 *  that ifp_globals.h appears.
 * Commented out here and inserted in mods.c by gfs 7/26/92.
 *
 * Widget          show_newMods_widget = NULL;
 * Widget          show_modsFromFile_widget = NULL;
 */

int     atoi();
void    pop_down_shell();
void    remap_toplevel_shell();


Widget                  mod_popup();
void                    delete_the_property();
void                    print_the_text();
void                    showLowerBoundError();
void                    invertWidget();
void                    handleModsList();
void                    handleUHGCHNGList();
void                    set_mods_apply();
void                    set_forecast_groups_apply();
void                    set_pool();
void                    set_instantaneous();
void                    set_mean();
void                    set_all();
void                    set_plot();
void                    set_table();
void                    set_snow();
void                    set_rain();
void                    tooFewOperations_Error();
void                    create_TS_popup();
void                    popup_ts_popupShell();
void                    popdown_ts_popupShell();
void                    cancel_ts_popupShell();
void                    popdown_noDates_shell();
void                    addTSRow();
void                    handleTWSelection();
void                    handle_MissingDate();
void                    create_date_list();
int                     compare_dates();
char                    *make_date_string();
void                    setDateWidgetsInsensitive();
void                    getModStartDate();
Widget                  create_tooManyQMeans_errorShell();
void                    store_mod_data();
void                    save_mod_data();
Widget                  create_no_date_errorPopup();
void                    post_menu_handler();

void                    popup_cancelAll_warningDialog();
void                    set_cancelAllMods_to_Yes();

void                    show_too_many_mods_errorDialog();

void                    cancel_delete_mods_from_file();
void                    make_mod_deletions();
void                    popup_modsDeleted_warning();
void                    answer_yes_to_delete_mods();
void                    answer_no_to_delete_mods();





void                    mods_slider_moved_callback();

void                    change_the_hour();
void                    change_the_day();
void                    change_the_month();
void                    change_the_year();
int                     days_in_the_month();


int             numberOfOperations;
int             numberTSTextWidgets;
int             numberTimeSteps;
int             testForTSPopup;
int             firstTW;
int             prev_uh_time_series;


int             operations_number[MAX_OPERATIONS];
int             isTSdateWidgetSelected[MAX_NUM_SETQMEAN_VALUES];

#endif
