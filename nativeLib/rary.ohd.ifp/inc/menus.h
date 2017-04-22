/* menus.h */

#ifndef menus_h
#define menus_h

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/BulletinB.h>
#include <Xm/ScrollBar.h>
#include <Xm/LabelG.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <stdlib.h>

#define DEFAULT_MAX_ROWS 51
#define DEFAULT_MAX_COLS 11
#define DEFAULT_VISIBLE_ROWS 13
#define DEFAULT_VISIBLE_COLS 3
#define DEFAULT_POPUP_SHELL_X 50
#define DEFAULT_POPUP_SHELL_Y 400
#define EDIT_TEXT_COLOR  "red"

void          resize_drawa();
void          fill_tables_data();
void          h_scrollbar_moved();
void          v_scrollbar_moved();
void          change_bb_data();
void          change_val();
int           is_valid_number();
void          update_plot();

Widget        global_toplevel;

#endif
