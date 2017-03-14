/*  mods_plot.h:  header file for the functions associated with
		  the UHCHNG, ROCHNG, and RRICHNG mods.
*/
/*            AV 4/04  added routine change_horizontal_scale() */
#ifndef mods_plot_h
#define mods_plot_h

#include <math.h>
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
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "libXifp.h"
#include "mods.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "Mods_info.h"
#include "ifp_struct.h"


#define  UH 0
#define  RRICHNG 1
#define  ROCHNG 2
#define  UHD    3/*AV added for uhgcdate */
#define  ENGLISH 0
#define  METRIC  1
#define  MAX_TSMOD_OPER  20
#define  PX  10
#define  RO  20
#define  RB_LINE_COLOR "white"
#define  START 100
#define  DONE  200
#define  UNDO  300
#define  RAIM  15
#define  MAP   25
//#define MAX_OPERATIONS                  100

Widget  global_toplevel;

void resize_mp_form_widget();
void resize_mp_y_axis_label();
void resize_mp_y_axis();
void resize_mp_graph();
void resize_mp_x_axis_label();
void resize_mp_x_axis();
void copy_one_mp_drawing_area();

extern void plot_mod();
extern void mp_Create_ts_array();
extern void mp_draw_rectangles();
int     val_to_pixel();
float   pixel_to_val();
void    exit_mp_cb();
void    mp_date_hr();
void    mp_draw_x_axis();
void    mp_label_axis();
void    change_y_axis_max();
void    change_horizontal_scale(); /*AV added 4/21/04 -resize x-axis */

int     mp_create_mouse_tracker();      /* in mp_mousetracker.c */
void    mp_clear_tracker();             /* in mp_mousetracker.c */
void    mp_track_mouse_position();      /* in mp_mousetracker.c */
void    mp_show_mouse_position();       /* in mp_mousetracker.c */

void    mp_start_rubber_band();         /* in mp_rubberband.c */
void    mp_track_rubber_band();         /* in mp_rubberband.c */
void    mp_end_rubber_band();           /* in mp_rubberband.c */
void    mp_interpolate();               /* in mp_rubberband.c */
int     mp_find_pt_index();             /* in mp_rubberband.c */

void    mp_crosshairs_timer();
void    mp_disable_crosshairs_timer();
void    mp_start_crosshairs();
void    mp_track_crosshairs();
void    mp_end_crosshairs();
void    mp_draw_crosshairs();
void    mp_erase_crosshairs();

void    mp_change_ts();
void    mp_undo_change_ts();
void    mp_draw_end_obs_line();
void    mp_done();
void    mp_create_mods();
void    scale_max_min();


/*
 * Commented out by gfs 7/28/92 because of warnings from CodeCenter.
 *
 * static int               mdata_start = 1;
 * static int               mnew_start = 1;
 */

#endif
