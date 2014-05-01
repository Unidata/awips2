/******************************************
	File:		crest_show.h
	Date:
	Author:		Dale Shelton
	
	Purpose:
	
******************************************/

#ifndef crest_show_h
#define crest_show_h

/********************
 Include header files  
 ********************/

#include <Xm/Xm.h>
#include "DbmsDefs.h"
#include "Crest.h"

/*********************************
 Definitions of symbolic constants
 *********************************/ 

#define	CREST_ALL		0
#define CREST_HI_WATER		1
#define CREST_LO_WATER		2

#define CREST_DATE              0
#define CREST_FLOW              1
#define CREST_STAGE             2

#define CREST_ROUND_FACTOR	1

#define CREST_NONFLOOD		0
#define CREST_ACTION		1
#define	CREST_FLOOD		2
#define CREST_MINOR		3
#define CREST_MODERATE		4
#define CREST_MAJOR		5
#define CREST_CAT_NUM		6

#define CREST_SCALE_INTERVALS	5

#define BOTTOM_Y_OFFSET		20
#define TOP_Y_OFFSET		10
#define TOTAL_Y_OFFSET		(BOTTOM_Y_OFFSET + TOP_Y_OFFSET)
#define LINE_OFFSET		6
#define LABEL_OFFSET		32
#define POINTS_OFFSET		15
#define CREST_LEFT_OFFSET	75
#define CREST_RIGHT_OFFSET	40

#define CREST_PT_X_OFFSET      -5
#define CREST_PT_Y_OFFSET	5

#define	GET_X 0
#define SET_X 1
#define GET_Y 2
#define SET_Y 3

#define OFFICIAL_CREST 0
#define RECORD_CREST   1
#define PRELIM_CREST   2

/*********************
 Variable Declarations
 *********************/

Crest  *crest;
char	crst_lid[LOC_ID_LEN + 1];
int	crst_state;

/*******************
 Function prototypes
 *******************/

void	crest_show(const Widget w, const char *lid, Boolean editable);
int	crest_load(Crest *crest);
void	crest_call_load();

void	crest_callbacks(void);
void	crest_add_textfilter_callbacks(void);
void	crest_remove_textfilter_callbacks(void);

void	crest_key(char *key, int *pos);
void	crest_stage_range(Crest *crest, double *max, double *min);
void	crest_date_range(Crest *crest, long *max, long *min);
void	crest_draw_cb(Widget w, XtPointer ptr, XtPointer cbs);
void	crest_draw(Widget w, Crest *crest);
void	crest_clear(void);
int	crest_save(Widget w);

/****************************
 Callback function prototypes 
 ****************************/

void	crest_import();
void	crest_ok();
void	crest_delete();
void	crest_del_conf();
void	crest_new();
void	crest_apply();
void	crest_close();

int	get_flood_cat_stages(const char *lid, double flood_stages[]);

long	get_year(long);

void	drawCrestUnitLabel(Display *display, Pixmap pm, GC gc,
			   const char *label);

void	draw_stage_axis(Widget scaleDA, double maxstage, double minstage, 
		        double flood_stages[], long num_points[],
		        char *colors[],
		        char *cat_names[]);

void	draw_year_axis(Widget w, Display *display, Pixmap pm, GC gc,
		       int height, int width,
		       long maxyear, long minyear);

void	draw_crest_points(Widget w, Display *display, Pixmap pm, GC gc,
		          int height, int width,
			  double maxstage, double minstage, 
			  long maxyear, long minyear,
			  Crest *crest,
			  double flood_stages[], long num_points[],
			  char *colors[]);

void	highlight_crest(Widget drawDA, Display *display, Pixmap pm, GC gc,
			int height, int width,
			double maxstage, double minstage, 
			long maxyear, long minyear);

void	select_crest(Widget w, XtPointer client_data, XEvent *event, Boolean *callem);

void	map_coord_bank(int action, double *min_data, double *max_data,
		       Position *min_win, Position *max_win);

void    set_x_coords(double min_data, double max_data,
		     Position min_win,	Position max_win);

void    set_y_coords(double min_data, double max_data,
		     Position min_win,	Position max_win);

void    get_x_coords(double *min_data, double *max_data,
		     Position *min_win,	Position *max_win);

void    get_y_coords(double *min_data, double *max_data,
		     Position *min_win,	Position *max_win);

long	find_closest_record(long year, double stage);

#endif



