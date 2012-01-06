
/* ******************************************************************

	fgmap.h
		declarations and structure definitions for fgmap.c

   ****************************************************************** */

#ifndef fgmap_h
#define fgmap_h

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <X11/StringDefs.h>
#include <Xm/ScrolledW.h>

#include "tree.h"
/*
#include "libXs.h"
*/
#include "struct_defs.h"

int     num_segments;
int     list_position;

int     n_popup;
int     tree_mapped;
int     list_mapped;
char    prev_seg_selected[9];

#define UNKNOWN  0
#define NORMAL   1
#define ALERT    2
#define FLOOD    3

#define NO      0
#define YES     1

#define MWM_BORDER_WIDTH                11
#define MWM_MENU_BORDER_HEIGHT          34
#define RC_BORDER_WIDTH                 3


#define MAX_NUM_SUBGROUPS               30



static char * labels[] =
		{
		"Description:",
		"River name:",
		"Station name:",
		"Forecast group:",
		"Carryover group:",
		"Upstream segments:",
		"Downstream segments:",
		"Latitude (degrees):",
		"Longitude (degrees):",
		"Type of forecast point:",
		"Forecast point area (sq mi):",
		"Total area above forecast point (sq mi):",
		"Flood stage (ft):",
		"Flood flow (cfs):",
		"Secondary stage (ft):",
		"Warning stage (ft):",
		"Warning flow (cfs):",
		"Gage zero (ft):",
		"Record flood stage (ft):",
		"Record flood flow (cfs):",
		"Date of record flood:",
		"Comment about record flood:",
		"Upper limit of rating curve (ft):"
		};

/*#include "extra_e19.h"

typedef struct _e19
	{
	char            id[40];
	int             in_fgroup;
	char            name[9];
	char            description[21];
	extra_e19       extra;
	}       e19_data;
*//*dublicated in struct_defs.h---kwz[4/29/02]*/

typedef struct _node
	{
	int             status;
	struct  _e19    e19;
	struct  _node   *parent;
	struct  _node   *left;
	struct  _node   *mid_left;
	struct  _node   *center;
	struct  _node   *mid_right;
	struct  _node   *right;
	}       node;

typedef struct
	{
	Widget                  tree_widget;
	Widget                  list_widget;
	Widget                  select_button_widget;
	node                    *node_ptr;
	}       w_w_w_n_struct;


typedef struct
	{
	char            segment_name[9];
	int             status_id;
	}       seg_status;



typedef struct
	{
	Widget          list;
	Widget          selected_seg_widget;
	int             item_count;
	}       select_list_struct;


typedef struct
	{
	Widget          enable;
	Widget          popup;
	}       fgmap_data_struct;


node    *head[24];



node    *make_node();
node    *insert_node();
void    build_popup();
node    *find_it();
void    show_tree();
void    fill_list();
void    count_segments();
void    invert_segment();
void    create_help_Dialog();

void    post_change();
void    hilite_list_from_select();
void    pop_down();
void    unhilite();
void    change_to_sensitive();
void    post_change();

void    handle_seg_list_selection();
void    map_list_cb();
void    exit_pgm();

void    help_event_handler();
void    popup_segment_info();


static w_w_w_n_struct   w_w_w_node[100];


XmString        *xmstr;
XmString        prev_segment_selected;

FILE            *input_file;

#endif
