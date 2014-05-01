/* run_partial.h */

#ifndef run_partial_h
#define run_partial_h

#include "tree.h"

/*
/*AV 2/6/01      added these routines:
/*               void    sac_snow_on_cb();
/*		 void    sac_snow_off_cb();
/*		 void    sac_snow_no_change_cb();
/*                 
*/

void    build_popup();
void    destroy_tree_children();


void    fill_non_univ_techniques_struct();
void    non_univ_Techniques_init();


void    create_upstream_seg_tree();
void    create_partial_seg_tree();
void    concat_the_ids();


void    amend_seg_list();
void    add_to_the_list();
void    delete_from_the_list();
void    show_the_list();
void    set_the_segments();
void    make_forecast_group_subset();
void    remove_from_the_list();
void    reset_the_segments();
void    pop_down_shell();
void    post_menu_handler();
void    post_change();
void    tell_which_tree();
void    add_to_selected();
void    delete_from_selected();

void    build_popup();
void    popup_segment_info();


void    check_multiple_selected();
int     is_highlighted();
void    reset_segment_colors();

void    create_run_interface();
void    create_runInfo_popup();
void    select_next_segment();
void    go_to_selected_segment();
void    set_run_dates();
void    create_set_dates_popup();
void    invert_tree_widget();
void    set_save_gif();
void    post_save_gif_atom();
void    clean_gif_files();
int     get_save_gif_atom();

void    set_show_mods_viewer();
void    post_show_mods_viewer_atom();
int     get_show_mods_viewer_atom();

void    select_nextSegment();
void    show_run_segments();
void    show_deleted_segments();
void    show_Tulsa_plot();
void    show_plot_TS();
void    show_TimeSeries_table();
void    show_operations_table();
void    show_rating_curve();
void    show_other_mods();
void    show_ForecastGroup_topology();
void    set_universal();
void    set_non_universal();
void    rerun_segment();
void    popup_FGroup_selectionDialog();
void    run_next_segment();
void    continue_to_next_TulPlot();
void    exit_nwsrfs();
void    revert_to_original_tree();
void    handle_segment_selected();
void    remove_segment_callbacks();
void    add_segment_callbacks();
void    set_run_multiple();
void    set_ok_runMultipleSegments();
void    set_cancel_runMultipleSegments();
int     is_computationally_before();
int     currentSegment_is_the_gotoSegment();
int     popup_send_mods_Dialog();


void    create_Universal_Tech_popup();
void    create_nonUniversal_Tech_popup();
void    show_nonUniversal_segmentList();
void    hide_nonUniversal_segmentList();
void    non_univ_Techniques_init();
void    set_non_univ_techniques();

void    create_univ_Techniques_struct();
void    initialize_univ_Techniques_struct();
void    change_univ_Techniques_struct();
void    revert_univ_Techniques_struct();
char    *get_output_time_zone_code();

void    create_selectNext_popup();
void    continue_with_next_segment();

void    popup_mods_not_written_to_file_warningDialog();
void    popup_modsNotDeleted_warningDialog();
void    popup_quitting_NWSRFS_warningDialog();
void    set_leave_NWSRFS_to_Yes();

void    univ_techs_to_window_property();
void    non_univ_techs_to_window_property();
void    update_Dates_timeZone_code();
void    reset_Univ_widgets();

void    highlight_current_segment();
void    change_current_segment_status();
void    change_segment_color();
void    change_widget_color();
void    set_widget_colors();
void    restore_default_colors();


void    set_snowModel_buttons_to_default();
void    set_snowModel_buttons_to_noChange();
void    set_snowModel_buttons_for_selectedSegment();
void    reset_nonUniv_widgets();


void    set_Input_Z();
void    set_Input_EST();
void    set_Input_CST();
void    set_Input_MST();
void    set_Input_PST();
void    set_Input_AST();
void    set_Input_HST();
void    set_Input_NST();

void    set_Input_EDT();
void    set_Input_CDT();
void    set_Input_MDT();
void    set_Input_PDT();
void    set_Input_ADT();
void    set_Input_HDT();
void    set_Input_NDT();


void    set_Output_Z();
void    set_Output_EST();
void    set_Output_CST();
void    set_Output_MST();
void    set_Output_PST();
void    set_Output_AST();
void    set_Output_HST();
void    set_Output_NST();

void    set_Output_EDT();
void    set_Output_CDT();
void    set_Output_MDT();
void    set_Output_PDT();
void    set_Output_ADT();
void    set_Output_HDT();
void    set_Output_NDT();


void    set_Mods_Z();
void    set_Mods_EST();
void    set_Mods_CST();
void    set_Mods_MST();
void    set_Mods_PST();
void    set_Mods_AST();
void    set_Mods_HST();
void    set_Mods_NST();

void    set_Mods_EDT();
void    set_Mods_CDT();
void    set_Mods_MDT();
void    set_Mods_PDT();
void    set_Mods_ADT();
void    set_Mods_HDT();
void    set_Mods_NDT();




void    english_toggleButton_callback();
void    metric_toggleButton_callback();
void    general_english_toggleButton_callback();
void    general_metric_toggleButton_callback();
void    api_english_toggleButton_callback();
void    api_metric_toggleButton_callback();
void    sac_english_toggleButton_callback();
void    sac_metric_toggleButton_callback();
void    warning_callback();
void    futurePrecip_callback();


void    snow_on_cb();
void    snow_off_cb();
void    snow_no_change_cb();

void    frost_on_cb();
void    frost_off_cb();
void    frost_no_change_cb();

void    upsc_on_cb();
void    upsc_off_cb();
void    upsc_no_change_cb();

void    upwe_on_cb();
void    upwe_off_cb();
void    upwe_no_change_cb();

void    sac_snow_on_cb();
void    sac_snow_off_cb();
void    sac_snow_no_change_cb();

void    order_selected_string();
void    make_list_of_segment_names();

void    popup_NWSRFS_stopped_working();

int     mods_pending();

void    continue_with_requested_command_deleted();
void    continue_with_requested_command_saved();

#endif


