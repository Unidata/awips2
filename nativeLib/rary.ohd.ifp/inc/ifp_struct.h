/*  Structure definitions for the Tulsa Plot operation of the
IFP.
    Will also include mods in the future. (9 July 91) */
/*  AV 4/21/04 added varables to create resize_x_axis_max */

#ifndef ifp_struct_h
#define ifp_struct_h
#include <stdlib.h>

#ifndef ts_info_pm_h
#define ts_info_pm_h

typedef struct {
        int     ts_type, delta_t;
        char    ts_id[9], data_type[5];
        }       TS_INFO;

#endif

#define MAX_TS   20
#define MAX_TSMOD_OPER   20

typedef struct
{
   int       rc_num, loc_q, loc_stg, RC;
   float     flood_stg, warning_stg;
   float     flood_flow, warning_flow;
   float     *rating_curve_q, *rating_curve_stg;
   float     stg_mult_conver_factor;
   float     stg_add_constant;
   float     q_mult_conver_factor;
   float     q_add_constant;
   float     min_q, max_q, min_stg, max_stg;
   float     q_axis_max, stg_axis_max;
   int       origin_x, origin_y, end_x, end_y;
   Widget    form;
   Widget    rc_mouse;
   Pixmap    pix[5];
   GC        gc[5];
   Widget    drawing_area_widget[5];
   Widget    rc_id_widget;
   char      rc_id[9];
   int       form_width, form_height;
   Dimension drawing_area_4_width, drawing_area_4_height;
   GC        crosshairs_gc;
   XPoint    previous_mouse_position;
   float     max_record_q, max_record_stg;
   char      rc_station_name[21];
   Widget    rc_vert_scrollbar_widget;
   Widget    rc_horiz_scrollbar_widget;
   Widget    rc_vert_scale_widget;
   Widget    rc_horiz_scale_widget;
   int       height_scale;
   int       width_scale;
   float     min_stage_disp;
   float     max_stage_disp;
   float     min_q_disp;
   float     max_q_disp;
   int       rc_v_slider_size, rc_h_slider_size;
   int       interpol_method;
   int       axis_type;
   float     rc_q_increment;
   float     rc_stg_increment;
   caddr_t    *ptr5;
   caddr_t    *ptr6;
	     }   rc_struct;

typedef struct  {
		 Widget     toplevel;
		 Widget     tultable, drawa;
		 Widget     mainbb, v_scrollbar, h_scrollbar;
		 Widget     bb_label_gadget;
		 int        used_bb_names, total_bb_names;
		 Widget     *bb_name_frames, *bb_name_labels;
		 int        used_bb_dates, total_bb_dates;
		 Widget     *bb_date_frames, *bb_date_labels;
		 int        used_bb_data, total_bb_data;
		 Widget     *bb_data;
		 char       **ts_menu_names, **day_hour;
		 int        **changed;
		 float      **data_array;
		 int        first_row_visible, first_col_visible;
		 int        visible_columns, visible_rows;
		 int        maximum_columns, maximum_rows;
		 Position   popup_shell_x, popup_shell_y;
		 Dimension  child_max_width, child_max_height;
		 short      child_shadowThickness;
		 Pixel      bb_foreground;
		 int        num_ts_menus;
		 int        menu_index;
		 int        table_created;
                 int        end[MAX_TS];
                 int        delta_t[MAX_TS];
                 TS_INFO    *ts_info;
                 int        *list_ts_index;
		 caddr_t    *ptr4;
		 caddr_t    *ptr5;
		 caddr_t    *ptr6;
		 } tables_cb_struct;

typedef struct  {
		 Widget         main_plot_shell;
		 Pixmap         pix[8];
		 GC             gc[8];
		 Widget         drawing_area_widget[8];
		 Widget         px_label_da_widget;
		 Pixmap         px_label_pix;
		 GC             px_label_gc;
		 Widget         ro_label_da_widget;
		 Pixmap         ro_label_pix;
		 GC             ro_label_gc;
		 Widget         discharge_label_da_widget;
		 Pixmap         discharge_label_pix;
		 GC             discharge_label_gc;
		 Widget         stage_label_da_widget;
		 Pixmap         stage_label_pix;
		 GC             stage_label_gc;
		 Widget         horiz_scrollbar_widget;
		 Widget         vertical_scrollbar_widget;
		 Widget         horiz_scale_widget;
		 Widget         vertical_scale_widget;
		 Widget         legend_rc, *legend;
		 Widget         mouse_rc;
		 Widget         mouse_date, mouse_discharge, mouse_stage;
		 Widget         undo_widget;
		 Widget         month_year_widget;
		 Widget         seg_name_widget;
		 int            max_points;
		 int            width_scale, height_scale;
	int     num_ts;
	int     total_run_hours;
	int     *num_pts;
	int     *num_plotted_ts;
	float   *x, **y;
	float   *obs;
	float   *min_x, *max_x, *min_y, *max_y;
	float   *discharge_axis_max, *stage_axis_max;
	float   **ts_array, **orig_ts_array;
	float   **px, **ro;
	float   px_max, px_min, ro_max, ro_min;
	int     num_rr_oper, num_rr_pts;
	int     *plot_mask, *obs_mask;
	int     end_obs,end_obs_px_ro; 
	int     end[MAX_TS];
	char    **day_hrs;
	char    **ts_name;
	char    **ts_color;
	char    **ts_symbol;
	char    **px_id, **ro_id;
	GC      line_gc[MAX_TS];
	GC      rb_gc;
	GC      rb_gc_px;
	GC      rb_gc_ro;
	int     ipt, start_end_sw;
	int     *plot_index;
	int     origin_x, origin_y, end_x, end_y;
	int     ts_index, ts_change_flag;
	XPoint  lpt, cpt, rpt;
	int     lastx;
	int     v_slider_size, h_slider_size;
	float   min_discharge_disp, max_discharge_disp;
	float   min_stage_disp, max_stage_disp;
	float   min_time_disp, max_time_disp;
	rc_struct *rc_data;
	float   px_ro_y_axis_max;
	XPoint  previous_mouse_position;
	Dimension hydrograph_width, hydrograph_height;
	Dimension px_height, ro_height;
	Widget   px_ro_units_widget;
	char     disch_label_data_type[4];
	Widget   hide_ts_widget;
	char     tracker_label[5];
	Widget   change_vert_axis_max_widget;
	float    default_discharge_axis_max;
	Widget   crosshairs_event_widget;
	int      px_origin_y;
	int      ro_origin_y;
	float    discharge_increment;
        int      h_q_plot_indicator;
	int      plot_delta_t;
	caddr_t    *ptr3;
		} plot_cb_struct;

typedef struct
{
   char               *seg_name;
   tables_cb_struct   *tables;
   plot_cb_struct     *plot;
}   combined_struct;

typedef struct
{
   Widget         main_plot_shell;
   Widget         form;
   Widget         mp_mouse;
   Pixmap         pix[5];
   GC             gc[5];
   Widget         drawing_area_widget[5];
   Widget         legend_rc, *legend;
   Widget         undo_widget;
   Widget         done_widget;
   Widget         seg_name_widget;
   Widget         month_year_widget;
   Widget         plot_button_control;
   float          **ts_array, **orig_ts_array;
   float          min_x, max_x, min_y, max_y;
   float          y_axis_max;
   int            origin_x, origin_y, end_x, end_y;
   int            num_ts_sel;
   int            num_pts, end_obs;
   int            mod_type_sw;
   char           **day_hr;
   char           **ts_color;
   char           *op_name[MAX_TSMOD_OPER];
   int            form_width, form_height;
   int            unit_sw;
   XPoint         lpt, cpt, rpt;
   int            lastx;
   int            ipt, start_end_sw;
   GC             rb_gc;
   int            ts_index;
   int            bar_width;
   int            ts_change_flag;
   int            px_type_flag;
   char           *seg_name;
   char           *myt;
   int            delta_t;
   int            start_run, end_run, valid_run;
   XPoint  previous_mouse_position;
   Dimension drawing_area_2_width, drawing_area_2_height;
   char           mp_tracker_label[4];
   float          default_y_axis_max;
   Widget         change_y_axis_max_widget;
   float          mp_y_increment;
   int            num_ts_plotted;          
   Widget         mp_main_drawing_area;
   int            Orgnum_pts, Orgend_obs;
   float          Orgmax_x;
   int            horizontal_scale_value;
   Widget         change_horizontal_scale_widget;/*AV 4/21/04 */ 
   caddr_t    *ptr3;
   caddr_t    *ptr4;
   caddr_t    *ptr5;
   caddr_t    *ptr6;  
}  mods_plot_struct;

#endif
