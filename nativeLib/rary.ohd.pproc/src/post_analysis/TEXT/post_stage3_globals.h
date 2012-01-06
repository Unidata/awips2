/*=========================================================================*/
/*                    FILE PATH/NAME:   st3_include/stage3_globals.h       */
/*                                                                         */
/*                              HEADER FILE                                */
/*=========================================================================*/

void    change_mosaic();
void    create_legend();
void    rerun_stageii();
void    select_site();
void    show_single_gage();
void    switch_sensitive();
void    display_single_gage();
void    display_no_gage();
void    display_gage();
void    close_gage();
void    do_time_lapse_6();
void    do_time_lapse_12();
void    do_time_lapse_24();
void    do_time_lapse();
void    choose_color();
void    choose_delay();
void    read_delay();
void    read_tl_scale();
void    do_zoom();
void    show_states();
void    show_county();
void    show_cities_and_towns();
void    show_basin_boundaries();
void    show_radar_rings();
void    show_rivers();
void    show_gages();
void    show_values();
void    locate();
void    locate_ss();
void    close_locate();
void    unmanage();
void    popdown_shell();
void    popdown_viewer();
void    popdown_gagetable();
void    destroy_shell();
void    create_time_lapse_popup();
void    create_time_delay_popup();
void    slider_moved_callback();
void    create_stage3_interface();
void    missing_data();
void    label_radar_II();
void    label_radar_I();
void    label_multi();
void    label_gage();
void    popdown_singleSite();
void    ignore_radar();
void    switch_and_mosaic();
void    edit_multi_sensor_field();
void    edit_bias_value();
void    show_gage_table();

void    create_adapt_popup();
void    read_adapt_param();
char    *table_data();
void    create_edit_interface();
void    save_edit_ml_field();
//void    cancel_edit();
void    undo_edit();
void    edit_polygon();
void    select_poly();
void    select_edit_point();
void    track_edit_point();

int     get_pixel_by_name();

void    read_geo_data();
void    post_analysis_add_overlays();
void    draw_bound();
void    start_rubber_band();
void    track_rubber_band();
void    end_rubber_band();

void    popup_workingDialog();

void post_analysis_select_callback();
void ok_callback();
void post_analysis_pop_down();
void time_lapse();
void end_time_lapse();
void loop_callback();

void start_end_rubber_poly();

void create_gage_table();
void sort();
int sort_by_diff();
int sort_by_ratio();
int sort_by_radar();
int sort_by_gageid();
int sort_by_gageval();

void step_through_loop();
void next_step();
int InOut();
void write_bad_bins();

int get_reverse_color();
void zoom_sensitive();

void display_date_window();
/*-----------------------------*/
/* single site interface       */
/*-----------------------------*/

 void create_single_site_interface();
 void initialize_draw_data();
 void ignore_data();
 void switch_buttons();
 void create_legend_ss();
 void show_ss_gages();

/*-----------------------------*/
/* color preferences functions */
/*-----------------------------*/

void display_colors();
void read_color_list();
void save_user_colors();
void cancel_colors();
void change_color();
void select_color_to_change();
void color_select();
void end_colors();

/*-----------------------------*/
/* level preferences functions */
/*-----------------------------*/

void read_vip_levels();
void set_vip_levels();
void select_levels();
void SetNewValue();
void SetLevelToChange();
void write_new_levels();
void retrieve_old_levels();

/*---------------------------------------*/
/*   pseudo gages                        */
/*---------------------------------------*/
void add_pseudo();
void locate_pseudo();
void popdown_pseudo();
void create_pseudo_popup();
void read_pseudo_scale();
void write_pseudo();
void insert_pseudo(char *,char *,char *,float,float,float);
void read_numpseudo();

/*---------------------------------------*/
/*   edit_bias_value                     */
/*---------------------------------------*/
void popdown_editbias();
void create_editbias_popup();
void read_editbias_scale();
void write_editbias();

/*-------------------------------------------------*/
/*  functions to save output                       */
/*-------------------------------------------------*/
void save_stageiii();
void save_merged();

void ok_quit();
Widget create_save_data_dialog();
void quit_stageiii();
void quit_stageiii_from_WM();
void next_callback();
void clear_area();

void show_dates();
void newhour();
void clear_legend();
void restore_date();
void hour_sensitive();
