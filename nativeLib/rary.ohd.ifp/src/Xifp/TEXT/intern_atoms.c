
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <X11/StringDefs.h>
#include "ifp_atoms.h"

/* ****************************************************************************

	 intern_the_atoms()
			intern the atoms for all interactive forecast
			program functions.

   **************************************************************************** */

void intern_the_atoms (toplevel)
	Widget          toplevel;
{
	Display         *display;

	display = XtDisplay(toplevel);


	IFPA_run_start_date = XInternAtom(display, "Run start date", False);
	IFPA_run_start_date_type = XInternAtom(display, "Run start date type", False);

	IFPA_run_end_date = XInternAtom(display, "Run end date", False);
	IFPA_run_end_date_type = XInternAtom(display, "Run end date type", False);

	IFPA_run_end_obs_date = XInternAtom(display, "Run end obs date", False);
	IFPA_run_end_obs_date_type = XInternAtom(display, "Run end obs date type", False);

	IFPA_mods_start_date = XInternAtom(display, "Mods start date", False);
	IFPA_mods_start_date_type = XInternAtom(display, "Mods start date type", False);

	IFPA_mods_end_date = XInternAtom(display, "Mods end date", False);
	IFPA_mods_end_date_type = XInternAtom(display, "Mods end date type", False);

	IFPA_mods_end_obs_date = XInternAtom(display, "Mods end obs date", False);
	IFPA_mods_end_obs_date_type = XInternAtom(display, "Mods end obs date type", False);

	IFPA_display_start_date = XInternAtom(display, "Display start date", False);
	IFPA_display_start_date_type = XInternAtom(display, "Display start date type", False);

	IFPA_display_end_date = XInternAtom(display, "Display end date", False);
	IFPA_display_end_date_type = XInternAtom(display, "Display end date type", False);

	IFPA_display_end_obs_date = XInternAtom(display, "Display end obs date", False);
	IFPA_display_end_obs_date_type = XInternAtom(display, "Display end obs date type", False);

	IFPA_run_segments = XInternAtom(display, "NWSRFS run segments", False);
	IFPA_run_segments_type = XInternAtom(display, "NWSRFS run segments type", False);

	IFPA_seg_selected = XInternAtom(display, "Segment selected", False);
	IFPA_seg_selected_type = XInternAtom(display, "Segment selected type", False);

	IFPA_seg_deleted = XInternAtom(display, "Segment deleted", False);
	IFPA_seg_deleted_type = XInternAtom(display, "Segment deleted type", False);

	IFPA_display_selected = XInternAtom(display, "Display selected", False);
	IFPA_display_selected_type = XInternAtom(display, "Display selected type", False);

	IFPA_current_segment = XInternAtom(display, "Current segment", False);
	IFPA_current_segment_type = XInternAtom(display, "Current segment type", False);

	IFPA_segment_status = XInternAtom(display, "Segment status", False);
	IFPA_segment_status_type = XInternAtom(display, "Segment status type", False);

	IFPA_status_change = XInternAtom(display, "Status change", False);
	IFPA_status_change_type = XInternAtom(display, "Status change type", False);

	IFPA_mod_limits_data = XInternAtom(display, "Mod limits data", False);
	IFPA_mod_limits_data_type = XInternAtom(display, "Mod limits data type", False);

	IFPA_forecast_group = XInternAtom(display, "Forecast group", False);
	IFPA_forecast_group_type = XInternAtom(display, "Forecast group type", False);

	IFPA_mods_quit_ok = XInternAtom(display, "Mods ok to quit", False);
	IFPA_mods_quit_ok_type = XInternAtom(display, "Mods ok to quit type", False);

	IFPA_done_making_mods = XInternAtom(display, "Done making mods", False);
	IFPA_done_making_mods_type = XInternAtom(display, "Done making mods type", False);

	IFPA_univ_techniques = XInternAtom(display, "Universal techniques", False);
	IFPA_univ_techniques_type = XInternAtom(display, "Universal techniques type", False);

	IFPA_current_segment_non_univ_techs = XInternAtom(display, "Non-universal techniques", False);
	IFPA_current_segment_non_univ_techs_type = XInternAtom(display, "Non-universal techniques type", False);

	IFPA_time_zone_code = XInternAtom(display, "Time zone code", False);
	IFPA_time_zone_code_type = XInternAtom(display, "Time zone code type", False);

	IFPA_rerun_segment = XInternAtom(display, "Rerun segment", False);
	IFPA_rerun_segment_type = XInternAtom(display, "Rerun segment type", False);

	IFPA_goto_upstream_segment = XInternAtom(display, "Goto upstream segment", False);
	IFPA_goto_upstream_segment_type = XInternAtom(display, "Goto upstream segment type", False);

	IFPA_goto_downstream_segment = XInternAtom(display, "Goto downstream segment", False);
	IFPA_goto_downstream_segment_type = XInternAtom(display, "Goto downstream segment type", False);

	IFPA_next_segment = XInternAtom(display, "Next segment", False);
	IFPA_next_segment_type = XInternAtom(display, "Next segment type", False);

	IFPA_show_mods = XInternAtom(display, "Show run-time mods", False);
	IFPA_show_mods_type = XInternAtom(display, "Show run-time mods type", False);

	IFPA_show_tulsa_plot = XInternAtom(display, "Show tulsa plot", False);
	IFPA_show_tulsa_plot_type = XInternAtom(display, "Show tulsa plot type", False);

	IFPA_show_tables = XInternAtom(display, "Show time-series tables", False);
	IFPA_show_tables_type = XInternAtom(display, "Show time-series tables type", False);

	IFPA_finished_making_mods = XInternAtom(display, "Finished making mods", False);
	IFPA_finished_making_mods_type = XInternAtom(display, "Finished making mods type", False);

	IFPA_IFP_NWSRFS_is_running = XInternAtom(display, "IFP-NWSRFS is running", False);
	IFPA_IFP_NWSRFS_is_running_type = XInternAtom(display, "IFP-NWSRFS is running type", False);

	IFPA_Start_IFP_is_running = XInternAtom(display, "Start IFP is running", False);
	IFPA_Start_IFP_is_running_type = XInternAtom(display, "Start IFP is running type", False);

	IFPA_ok_to_write_mods = XInternAtom(display, "OK to write mods", False);
	IFPA_ok_to_write_mods_type = XInternAtom(display, "OK to write mods type", False);

	IFPA_number_of_mods_to_write = XInternAtom(display, "Number of mods to write", False);
	IFPA_number_of_mods_to_write_type = XInternAtom(display, "Number of mods to write type", False);

	IFPA_general_units = XInternAtom(display, "NWSRFS general units", False);
	IFPA_general_units_type = XInternAtom(display, "NWSRFS general units type", False);

	IFPA_mods_general_units = XInternAtom(display, "Mods general units", False);
	IFPA_mods_general_units_type = XInternAtom(display, "Mods general units type", False);

	IFPA_mods_SAC_units = XInternAtom(display, "Mods SAC units", False);
	IFPA_mods_SAC_units_type = XInternAtom(display, "Mods SAC units type", False);

	IFPA_mods_API_units = XInternAtom(display, "Mods API units", False);
	IFPA_mods_API_units_type = XInternAtom(display, "Mods API units type", False);


	IFPA_number_of_TulPlots = XInternAtom(display, "Number of Tulsa Plots", False);
	IFPA_number_of_TulPlots_type = XInternAtom(display, "Number of Tulsa Plots type", False);


	IFPA_continue_to_next_operation = XInternAtom(display, "Continue to next operation", False);
	IFPA_continue_to_next_operation_type = XInternAtom(display, "Continue to next operation type", False);


	IFPA_begin_NWSRFS = XInternAtom(display, "Begin NWSRFS", False);
	IFPA_begin_NWSRFS_type = XInternAtom(display, "Begin NWSRFS type", False);

	IFPA_quit_NWSRFS = XInternAtom(display, "Quit NWSRFS", False);
	IFPA_quit_NWSRFS_type = XInternAtom(display, "Quit NWSRFS type", False);

	IFPA_entering_NWSRFS_eventLoop = XInternAtom(display, "Entering NWSRFS event loop", False);
	IFPA_entering_NWSRFS_eventLoop_type = XInternAtom(display, "Entering NWSRFS event loop type", False);

	IFPA_current_mod_saved = XInternAtom(display, "Current Mod is saved", False);
	IFPA_current_mod_saved_type = XInternAtom(display, "Current Mod is saved type", False);

	IFPA_no_Tulsa_plot = XInternAtom(display, "No Tulsa plot", False);
	IFPA_no_Tulsa_plot_type = XInternAtom(display, "No Tulsa plot type", False);

	IFPA_end_of_file_initializations = XInternAtom(display, "End file initializations", False);
	IFPA_end_of_file_initializations_type = XInternAtom(display, "End file initializations type", False);

	IFPA_show_operations_table = XInternAtom(display, "Show operations table", False);
	IFPA_show_operations_table_type = XInternAtom(display, "Show operations table type", False);

	IFPA_show_rating_curve = XInternAtom(display, "Show rating curve", False);
	IFPA_show_rating_curve_type = XInternAtom(display, "Show rating curve type", False);

	IFPA_rating_curve_available = XInternAtom(display, "Rating curve available", False);
	IFPA_rating_curve_available_type = XInternAtom(display, "Rating curve available type", False);

	IFPA_num_mods_to_delete_fromFile = XInternAtom(display, "Number mods to delete from File", False);
	IFPA_num_mods_to_delete_fromFile_type = XInternAtom(display, "Number mods to delete from File type", False);

	IFPA_tschng_mod = XInternAtom(display, "TSCHNG mod", FALSE);
	IFPA_tschng_mod_type = XInternAtom(display, "TSCHNG mod type", FALSE);

	IFPA_mods_have_been_deleted = XInternAtom(display, "Mods have been deleted", FALSE);
	IFPA_mods_have_been_deleted_type = XInternAtom(display, "Mods have been deleted type", FALSE);

	IFPA_save_gif_file = XInternAtom(display, "Save ifp gif file", FALSE);
	IFPA_save_gif_file_type = XInternAtom(display, "Save ifp gif file type", FALSE);

	IFPA_save_gif_file_done = XInternAtom(display, "Save ifp gif file done", FALSE);
	IFPA_save_gif_file_done_type = XInternAtom(display, "Save ifp gif file done type", FALSE);
	
	IFPA_first_plot = XInternAtom(display, "First plot in segment", FALSE);
	IFPA_first_plot_type = XInternAtom(display, "First plot in segment type", FALSE);

        IFPA_tot_num_TulPlots = XInternAtom(display, "Total Number of Tulsa Plots", False);
	IFPA_tot_num_TulPlots_type = XInternAtom(display, "Total Number of Tulsa Plots type", False);

        IFPA_mod_files_updated = XInternAtom(display, "Mod files updated", False);
	IFPA_mod_files_updated_type = XInternAtom(display, "Mod files updated type", False);
	
	IFPA_nts_mods_available = XInternAtom(display, "NTS mods available", False);
	IFPA_nts_mods_available_type = XInternAtom(display, "NTS mods available type", False);

	IFPA_rerun_plot_num = XInternAtom(display, "Rerun plot number", False);
	IFPA_rerun_plot_num_type = XInternAtom(display, "Rerun plot number type", False);

	IFPA_show_mods_viewer = XInternAtom(display, "Show mods viewer", False);
	IFPA_show_mods_viewer_type = XInternAtom(display, "Show mods viewer type", False);

	IFPA_activate_rerun = XInternAtom(display, "Activate Rerun", False);
	IFPA_activate_rerun_type = XInternAtom(display, "Activate Rerun type", False);

	IFPA_FGmods_save = XInternAtom(display, "FGmods Save", False);
	IFPA_FGmods_save_type = XInternAtom(display, "FGmods Save type", False);

        IFPA_fgmod_files_updated = XInternAtom(display, "fgMod files updated", False);
	IFPA_fgmod_files_updated_type = XInternAtom(display, "fgMod files updated type", False);

        IFPA_rangemods_files_updated = XInternAtom(display, "rangeMod files updated", False);
	IFPA_rangemods_files_updated_type = XInternAtom(display, "rangeMod files updated type", False);

	IFPA_rangemods_saved = XInternAtom(display, "rangemods Save", False);
	IFPA_rangemods_saved_type = XInternAtom(display, "rangemods Save type", False);

	IFPA_show_plot_TS = XInternAtom(display, "Show plot TS", False);
	IFPA_show_plot_TS_type = XInternAtom(display, "Show plot TS type", False);
	
	IFPA_has_plot_TS = XInternAtom(display, "Has plot TS", False);
	IFPA_has_plot_TS_type = XInternAtom(display, "Has plot TS type", False);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/intern_atoms.c,v $";
 static char rcs_id2[] = "$Id: intern_atoms.c,v 1.8 2002/05/15 17:54:05 dws Exp $";}
/*  ===================================================  */

}
