/*=========================================================================*/
/*                        FILE NAME:   post_functions.h                    */
/*                                                                         */
/*                        HEADER FILE FOR POST ANALYSIS PROGRAM            */
/*                                 CONTAINING:                             */
/*                             FUNCTION PROTOTYPES                         */
/*=========================================================================*/

 void   post_analysis();
 void   display_post();
 void   initialize_draw_data();
 void   create_legend();

 void   merge_data();
 void   quit();
 void   quit_and_save();
 void   quit_post_from_WM();
 void   zoom();
 void   time_lapse();
 void   zoom_sensitive();
 void   switch_sensitive();
 void   show_gage_table();
 void   remove_ap();
 void   close_shell();
 void   redo_gageonly();
 void   show_gages();
 void   show_values();

 void   Sum24();
 void   GetGageData();
 void   GetMisbin();
 void   GageOnly();
 float  WeightedValue(int, int);
 void   ReadParam();
 void   MergeData();
 void   timedist();

 void   fill_pixmap();
 void   copy_area();
 void   read_colors();
 void   draw_bound();

 void   display_merge();
 void   initialize_merged_data();

 Widget create_working_dialog();
 void   display_date_window();
 void   un_map_shell();
 void   post_analysis_pop_down();
 void   post_analysis_select_callback();
 void   ok_callback();
 void   popup_workingDialog();
 Widget create_working_dialog();
 void   set_duration();

 GC     xs_create_xor_gc();
 void   start_rubber_band();
 void   track_rubber_band();
 void   end_rubber_band();

 void   read_geo_data();
 void   post_analysis_add_overlays();
 void   show_states();
 void   show_rivers();
 void   show_basin_boundaries();
 void   show_cities_and_towns();
 void   show_county();

 void   create_gage_table();
 void   display_no_gage();
 void   sort();
 int    sort_by_gageval();
 int    sort_by_diff();
 int    sort_by_ratio();
 int    sort_by_gageid();
 void   change_rc();
 void   edit_gage_value();
 char  *table_data();

 void   show_single_gage();
 void   display_single_gage();
 void   display_gage();
 void   close_gage();

 void   locate_main();
 void   locate_merge();
 void   close_locate();
