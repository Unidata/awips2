/*  rating_curve.h:  header file for the functions associated with
		     the rating curve display
*/

#ifndef rating_curve_h
#define rating_curve_h

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

#include "ifp_struct.h"

void resize_rc_form_widget();
void resize_rc_y_axis();
void resize_rc_y_axis_label();
void resize_rc_x_axis();
void resize_rc_x_axis_label();
void resize_rc_graph();
void copy_one_rc_drawing_area();
void rc_label_axis();
void rc_draw_x_axis();
void rc_draw_graph_line();
int  rc_create_mouse_tracker();
int  val_to_pixel();
float pixel_to_val();

void    rc_crosshairs_timer();
void    rc_disable_crosshairs_timer();
void    rc_start_crosshairs();
void    rc_track_crosshairs();
void    rc_end_crosshairs();
void    rc_draw_crosshairs();
void    rc_erase_crosshairs();
void    rc_vert_scrollbar_moved();
void    rc_horiz_scrollbar_moved();
void    rc_scale_width_changed();
void    rc_scale_height_changed();
void    rc_draw_info_lines();

#endif
