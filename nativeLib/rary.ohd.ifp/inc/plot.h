/* plot.h */

#ifndef plot_h
#define plot_h


#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>

#include <math.h>

#define WIDTH_SCALE 1
#define HEIGHT_SCALE 1
#define MAX_POINTS  50
#define MAX_TS  20
#define PI          3.14159
#define NOPLOT 100
#define PLOT   200
#define START  100
#define DONE   200
#define SELECTED_LINE_COLOR  "white"
#define PX  10
#define RO  20
#define LOG    0
#define LINEAR 1

void resize_form_widget();

void copy_one_tulplot_drawing_area();
void help_event_handler();
void exit_tulplot_cb();

void resize_px_label();
void resize_px_y_axis();
void resize_px_x_axis();
void resize_ro_label();
void resize_ro_y_axis();
void resize_ro_x_axis();
void resize_x_axis();
void resize_discharge_label();
void resize_discharge_axis();
void resize_hydrograph();
void resize_stage_axis();
void resize_stage_label();

void plot_horiz_scrollbar_moved();
void plot_vertical_scrollbar_moved();
void scale_width_changed();
void scale_height_changed();
void change_vert_axis_max();

void    draw_axes();
void    draw_line();
void    draw_graph_line();
void    draw_lines();
int     val_to_pixel();
float   pixel_to_val();
void    select_ts();
void    change_ts();
void    undo_change_ts();
char    *xs_get_string_from_xmstring();
int     create_mouse_tracker();
void    clear_tracker();
void    track_mouse_position();
void    show_mouse_position();

void    IFP_Map_start_rubber_band();
void    start_rubber_band();
void    IFP_Map_track_rubber_band();
void    track_rubber_band();
void    IFP_Map_end_rubber_band();
void    end_rubber_band();

void    crosshairs_timer();
void    disable_crosshairs_timer();
void    start_crosshairs();
void    track_crosshairs();
void    end_crosshairs();
void    draw_crosshairs();
void    erase_crosshairs();

void    interpolate();
int     find_pt_index();
void    label_axis();
float   linear_int();
float   log_int();
void    update_table();
void    fill_tschng_keywords();
void    draw_rectangles();
void    scale_max_min();

void    save_ifp_gif_file();
int     get_save_gif_atom();
int     get_plot_num();

Widget  global_toplevel;

#endif
