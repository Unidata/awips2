/* ifp_atoms.h */

#ifndef ifp_atoms_h
#define ifp_atoms_h

/*      Atoms used in the NWSRFS Interactive Forecast Program - 06/29/90        */

Atom            IFPA_run_start_date, IFPA_run_start_date_type;
Atom            IFPA_run_end_date, IFPA_run_end_date_type;
Atom            IFPA_run_end_obs_date, IFPA_run_end_obs_date_type;

Atom            IFPA_mods_start_date, IFPA_mods_start_date_type;
Atom            IFPA_mods_end_date, IFPA_mods_end_date_type;
Atom            IFPA_mods_end_obs_date, IFPA_mods_end_obs_date_type;

Atom            IFPA_display_start_date, IFPA_display_start_date_type;
Atom            IFPA_display_end_date, IFPA_display_end_date_type;
Atom            IFPA_display_end_obs_date, IFPA_display_end_obs_date_type;

Atom            IFPA_run_segments, IFPA_run_segments_type;

Atom            IFPA_seg_selected, IFPA_seg_selected_type;
Atom            IFPA_seg_deleted, IFPA_seg_deleted_type;

Atom            IFPA_display_selected, IFPA_display_selected_type;

Atom            IFPA_current_segment, IFPA_current_segment_type;
Atom            IFPA_segment_status, IFPA_segment_status_type;
Atom            IFPA_status_change, IFPA_status_change_type;

Atom            IFPA_forecast_group, IFPA_forecast_group_type;

Atom            IFPA_IFP_NWSRFS_is_running, IFPA_IFP_NWSRFS_is_running_type;
Atom            IFPA_Start_IFP_is_running, IFPA_Start_IFP_is_running_type;

Atom            IFPA_mod_limits_data, IFPA_mod_limits_data_type;
Atom            IFPA_mods_quit_ok, IFPA_mods_quit_ok_type;
Atom            IFPA_done_making_mods, IFPA_done_making_mods_type;

Atom            IFPA_univ_techniques, IFPA_univ_techniques_type;
Atom            IFPA_current_segment_non_univ_techs, IFPA_current_segment_non_univ_techs_type;

Atom            IFPA_time_zone_code, IFPA_time_zone_code_type;

Atom            IFPA_show_mods, IFPA_show_mods_type;
Atom            IFPA_show_tulsa_plot, IFPA_show_tulsa_plot_type;
Atom            IFPA_show_tables, IFPA_show_tables_type;
Atom            IFPA_show_operations_table, IFPA_show_operations_table_type;
Atom            IFPA_show_rating_curve, IFPA_show_rating_curve_type;
Atom            IFPA_rating_curve_available, IFPA_rating_curve_available_type;

Atom            IFPA_rerun_segment, IFPA_rerun_segment_type;
Atom            IFPA_next_segment, IFPA_next_segment_type;
Atom            IFPA_goto_upstream_segment, IFPA_goto_upstream_segment_type;
Atom            IFPA_goto_downstream_segment, IFPA_goto_downstream_segment_type;

Atom            IFPA_finished_making_mods, IFPA_finished_making_mods_type;

Atom            IFPA_ok_to_write_mods, IFPA_ok_to_write_mods_type;
Atom            IFPA_number_of_mods_to_write, IFPA_number_of_mods_to_write_type;

Atom            IFPA_general_units, IFPA_general_units_type;
Atom            IFPA_mods_general_units, IFPA_mods_general_units_type;
Atom            IFPA_mods_SAC_units, IFPA_mods_SAC_units_type;
Atom            IFPA_mods_API_units, IFPA_mods_API_units_type;

Atom            IFPA_number_of_TulPlots, IFPA_number_of_TulPlots_type;
Atom            IFPA_first_plot, IFPA_first_plot_type;
Atom            IFPA_tot_num_TulPlots, IFPA_tot_num_TulPlots_type;

Atom            IFPA_continue_to_next_operation, IFPA_continue_to_next_operation_type;

Atom            IFPA_begin_NWSRFS, IFPA_begin_NWSRFS_type;

Atom            IFPA_quit_NWSRFS, IFPA_quit_NWSRFS_type;

Atom            IFPA_entering_NWSRFS_eventLoop, IFPA_entering_NWSRFS_eventLoop_type;

Atom            IFPA_current_mod_saved, IFPA_current_mod_saved_type;

Atom            IFPA_no_Tulsa_plot, IFPA_no_Tulsa_plot_type;

Atom            IFPA_end_of_file_initializations, IFPA_end_of_file_initializations_type;

Atom            IFPA_num_mods_to_delete_fromFile, IFPA_num_mods_to_delete_fromFile_type;

Atom            IFPA_tschng_mod, IFPA_tschng_mod_type;

Atom            IFPA_mods_have_been_deleted,
		IFPA_mods_have_been_deleted_type;

Atom            IFPA_save_gif_file,
		IFPA_save_gif_file_type;

Atom            IFPA_save_gif_file_done, 
		IFPA_save_gif_file_done_type;
		
Atom            IFPA_mod_files_updated,
                IFPA_mod_files_updated_type;
                
Atom            IFPA_nts_mods_available,
                IFPA_nts_mods_available_type;
                		
Atom            IFPA_rerun_plot_num,
                IFPA_rerun_plot_num_type;
                		
Atom            IFPA_show_mods_viewer,
                IFPA_show_mods_viewer_type;

Atom            IFPA_activate_rerun,
                IFPA_activate_rerun_type;
                		
Atom            IFPA_FGmods_save,
                IFPA_FGmods_save_type;

Atom            IFPA_fgmod_files_updated,
                IFPA_fgmod_files_updated_type;

Atom            IFPA_rangemods_files_updated,
		IFPA_rangemods_files_updated_type;

Atom            IFPA_rangemods_saved,
                IFPA_rangemods_saved_type;

Atom            IFPA_show_plot_TS, IFPA_show_plot_TS_type;

Atom            IFPA_has_plot_TS, IFPA_has_plot_TS_type;
#endif
