#ifndef GAGEQC_GUI_H
#define GAGEQC_GUI_H

/*******************************************************************************
* FILENAME:           gageqc_gui.h
* DESCRIPTION:        Contains the prototypes for the gageqc_gui routines.
*
* ORIGINAL AUTHOR:    Bryon Lawrence
* CREATION DATE:      January 9, 2006
* ORGANIZATION:       OHD-11 HSEB
* MACHINE:            Linux
* MODIFICATION HISTORY:
*   DATE            PROGRAMMER        DESCRIPTION/REASON
*   January 9, 2006 Bryon Lawrence    First coding
********************************************************************************
*/
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>

#include <gageqc_defs.h>
#include <gageqc_types.h>

#include "map_convert.h"
#include "map_defines.h"

#define MPE_LEGEND_HEIGHT 60
#define MPE_LEGEND_WIDTH 600

#define INITIAL_LEGEND_OFFSET 5
#define LEGEND_BOX_WIDTH 45
/*#define LEGEND_BOX_WIDTH 35 */
#define LEGEND_BOX_HEIGHT 15

#define NUM_LEGEND_COLORS 16
#define NUM_FREEZING_RANGES 1
#define NUM_TEMP_RANGES 3
#define NUM_PRECIP_RANGES 5
#define NUM_CLIMATE_SOURCES 2

#define SELECTION_LINE_WIDTH 5

#define DQC_PREPROCESSOR_BASETIME_SET_12Z 100
#define DQC_PREPROCESSOR_BASETIME_SET_18Z 101
#define DQC_PREPROCESSOR_BASETIME_SET_00Z 102
#define DQC_PREPROCESSOR_BASETIME_SET_06Z 103

struct legend
{
   enum MapState legend_state;
   enum MapState legend_filter_up_state;
   enum MapState legend_filter_down_state;
   int legend_filter_index;
   int filter_up_x;
   int filter_up_y;
   int filter_down_x;
   int filter_down_y;
   int filter_off_x;
   int filter_off_y;
   char * colors [ NUM_LEGEND_COLORS ];
   int legend_range;
};

typedef void ( * reset_function ) ( const struct _dqc_run_date * );

void change_abmode(Widget w,
		   XtPointer data,
		   XtPointer call_data );

void change_dcmode ( Widget w,
                     XtPointer data,
                     XtPointer call_data);

void change_character ( Widget w,
                        XtPointer data,
                        XtPointer call_data );

void change_pcpn_zoom_mode ( Widget w, 
                             XtPointer client_data,
		             XtPointer call_data );
void change_plot ( Widget w,
		   XtPointer client_data,
		   XtPointer call_data );

void change_pcpn_edit_mode ( Widget w,
                             XtPointer data,
                             XtPointer call_data );

void change_pcpn_time ( Widget w,
                        XtPointer data,
                        XtPointer call_data ) ;

void change_rsmode ( Widget w,
                     XtPointer data,
                     XtPointer call_data );

void change_tcmode ( Widget w,
                     XtPointer data,
                     XtPointer call_data );

void change_z_edit_mode(Widget w,
                        XtPointer data,
                        XtPointer call_data);

void change_z_time ( Widget w,
		     XtPointer data,
		     XtPointer call_data );

void change_maxmin_edit_mode ( Widget w,
                               XtPointer data,
                               XtPointer call_data );

void display_pcpn_options ( Widget w, 
                            XtPointer client_data, 
		            XtPointer call_data );

void edit_freezing_gages_callback ( Widget w,
                                    XtPointer client_data,
                                    XtPointer call_data );

void edit_precip_gages_callback ( Widget w,
                                  XtPointer client_data,
                                  XtPointer call_data );

void edit_temp_gages_callback ( Widget w,
                                XtPointer client_data,
                                XtPointer call_data );

void get_legend ( );

void new_dm ( Widget w, XtPointer data, XtPointer call_data );

void new_elevation_filter ( Widget w,
                            XtPointer client_data,
                            XtPointer call_data );

void new_filter ( Widget w, XtPointer data, XtPointer call_data );

void new_reverse_filter ( Widget w, XtPointer data, XtPointer call_data );

void temperature_value_filter ( Widget w, 
                                XtPointer client_data, 
                                XtPointer call_data );

void temperature_reverse_value_filter ( Widget w, 
                                        XtPointer client_data, 
                                        XtPointer call_data );

void freezing_value_filter ( Widget w, 
                             XtPointer client_data, 
                             XtPointer call_data );

void freezing_reverse_value_filter ( Widget w, 
                                     XtPointer client_data, 
                                     XtPointer call_data );

void other_pcpn_options ( Widget w, 
                          XtPointer client_data,
		          XtPointer call_data );

void screening_options ( Widget w,
		         XtPointer client_data,
			 XtPointer call_data );
void source_select ( Widget w,
                     XtPointer client_data, 
                     XtPointer call_data );

void quality_select ( Widget w,
                      XtPointer client_data,
                      XtPointer call_data );
void quality_select_temperature ( Widget w,
                                  XtPointer client_data,
                                  XtPointer call_data );

void plot_precip_stations(int type,
		          int map_number);
void plot_temperature_stations(int type,
		          int map_number);
void plot_freezing_stations(int type,
		          int map_number);

void _plotGageQCData();

char ** get_precip_colors ( );
char ** get_temperature_colors ( );
char ** get_freezing_colors ( );

enum MapState isThereDQCprecipData ();
enum MapState isThereDQCtempData ();
enum MapState isThereDQCfreezingData ();

char ** get_precip_base_colormap ( );
char ** get_temperature_base_colormap ( );
char ** get_freezing_base_colormap ( );
char ** get_color_map_a ( );
char ** get_color_map_n ( );

void draw_precip_legend (int map_number, int yoffset);
void draw_temperature_legend ( int map_number, int yoffset );
void draw_freezing_legend ( int map_number, int yoffset );
void dqc_legend_select_cb ( Widget w, XtPointer client_data, XEvent * event, 
                            Boolean * flag );
void temperature_legend_select_cb ( Widget w, XtPointer client_data,
                                    XEvent * event );
void send_expose ( );
void send_legend_expose ( );
void freezing_legend_select_cb ( Widget w, XtPointer client_data,
                                 XEvent * event );

void apply_group();

void apply_tgroup();

void group_edit_temperature_stations(int map_num, int win_x,int win_y);
void group_edit_precip_stations(int map_num, int win_x,int win_y);
void plot_mean_areal_precip (int h, int display_flag, int num);
void save_dbase ( Widget w, XtPointer client_data, XtPointer call_data);
void cancel_dbase ( Widget w, XtPointer client_data, XtPointer call_data);
void cancel_dbase2 (Widget w, XtPointer client_data, XtPointer call_data);
void ok_dbase ( Widget w, XtPointer client_data, XtPointer call_data);
void send_dbase ( Widget w, XtPointer client_data, XtPointer call_data);
int quit_all ( );
void plot_mean_area_temperature(int h, int display_flag,int num);
void plot_gridded_freezing (int h, int display_flag, char *file, int num);
void plot_gridded_temperature(int h, int display_flag,char *file,int num);
void plot_mean_areal_freezing (int h, 
                               int display_flag, 
                               int num);
void XmtWaitUntilMapped(Widget w);
void plot_maxmin(int isom,int h, int display_flag,int type);
void initialize_gageqc_gui ( );
const char * get_dqc_nodata_color ( );
int is_area_master ( );
int dqc_run_date_changed ( );
int dqc_run_area_changed ( );
void free_area_val_mem ( );
void init_dqc_local_date_and_area_values ( );
int load_new_data_set ( Widget w, 
                        const char * area_id,
                        int master_file_flag,
                        int begin_year,
                        int begin_month,
                        int begin_day,
                        XtCallbackProc qc_gui_function );
void set_dqc_area_and_date ( );
void get_precip_time_step_arrows ( Widget * up_arrow, Widget * down_arrow );
void set_precip_arrow_sensitivity ( );
void get_freezing_time_step_arrows ( Widget * up_arrow, Widget * down_arrow );
void set_freezing_arrow_sensitivity ( );
void reset_dqc_area_and_date ( );
void set_text_box_widget ( Widget * box );
Widget get_text_box_widget ( );

void set_reset_function ( reset_function function );
reset_function get_reset_function ( );

const char * getClimateSource ( const char * pParamCode );


#endif /* #ifndef GAGEQC_GUI_H */
