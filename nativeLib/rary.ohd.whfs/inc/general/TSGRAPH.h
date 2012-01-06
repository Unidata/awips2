
/* ******************************************** */
/*        File:           TSGRAPH.h             */
/*        Date:           April 1999            */
/*        Author:         Sung Vo               */
/*        Purpose:        Provide support for   */
/*	  the Time Series Control Dialog and    */
/*        the Time Series Control Display       */
/* ******************************************** */

#ifndef	_TSGRAPH_h
#define _TSGRAPH_h

#include "TSdbx.h"


#define   MAX_POINTS 		5000
#define   MAX_TRACES 		30
#define   MAX_GRAPHS 		6
#define   MAX_PAGE_DATA 	1 
#define   MAX_PAGE_INFO 	30 
#define   MAX_GROUPS 		500
#define   MAX_FCST_TRACES	30

#define PER_CENT_XOFFSET 12/100
#define PER_CENT_YOFFSET 15/100
#define PER_CENT_WIDTH   78/100
#define PER_CENT_HEIGHT  75/100

#define MAX_COLORS     	40

#define DAYS_MAX     	90

#define NO_PC_TO_PP     0
#define INTERPOLATE	1
#define ASSIGN   	2

#define STAGE		0
#define DISCHARGE	1

#define GROUP		0
#define STATION		1	

#define RECTANGLE	0
#define CIRCLE		1
#define SYMBOLX		2

#define ADD		1
#define DELETE		2
#define	MOVE		3
#define SETMISSING      4

#define EDIT_START 	1
#define EDIT_ACTIVE	2
#define EDIT_RESET	3
#define EDIT_SAVE 	4
#define EDIT_COLOR	20

#define NO_DATA		0


/* *********************************************** */
/* Edit Colors  Structure for Timeseries Display   */
/* *********************************************** */
typedef struct _COLORS
{
	int 	fg_color;
	char	*colorname;

}COLORS; 


/* ********************************* */
/* Structure for Group information   */
/* ********************************* */
typedef struct _GROUP_INFO
{
	int    	page_count,
		grid_lines,
	    trace_mode,
		past_hours,
		future_hours,
		days_back,
		days_forward,
		current_page,
		GroupSelected;

	char	gridmode[2],
		tracemode[2],
		group_name[80],
		descript[80];
}GROUP_INFO;

/* ********************************* */
/* Structure for Trace information   */
/* ********************************* */
typedef struct _TRACE_INFO
{

	char	name[80];
	char    lid[LOC_ID_LEN + 1];
	char    pe[SHEF_PE_LEN + 1];
	int     dur;
	char    ts[SHEF_TS_LEN + 1];
	char    extremum[SHEF_EX_LEN + 1];
	char	cdur[2];

	char	color_name[20];

	Boolean isForecast;


} TRACE_INFO;


/* ********************************* */
/* Structure for Graph information   */
/* ********************************* */
typedef struct _GRAPH_INFO
{
	char 		title[80];
	int		num_traces,
			graph_pos, 
			xsize,
			ysize,
			yscale,
			ylinear,
			showcat, 
			derivepp,
			showpp,
			latestfcstonly;

	char		ylinearc[2],
			yscalec[2],
			showcatc[2],
			deriveppc[2],
			showppc[2],
			latestfcstonlyc[2];

	TRACE_INFO	traces[MAX_TRACES];

}GRAPH_INFO;

/* ********************************* */
/* Structure for Page  information   */
/* ********************************* */
typedef struct _PAGE_INFO
{
	char 	title[80];
	int     num_graphs;
	GRAPH_INFO graph[MAX_GRAPHS];

} PAGE_INFO;


/* **************************************** */
/* Structure for Page Manager information   */
/* **************************************** */
typedef struct _PAGE_MGR
{

	int    	active_graph,
		last_active_graph,
		active_trace,
	    	display_crosshairs,
		option_menuUp,
	       	zoom,
	       	reset,
	    	off_focus,
	       	start_x, 
		start_y,
	       	last_x, 
		last_y,
	    	x1,
		y1,
		x2,
		y2;

	int	Editmode,
	   	Editsave,
	   	Edit_active,
	   	Edit_count,
	   	edit_graph,
	   	edit_trace,
		standalone;

	int  	page_width, 
		page_height,
	    	symbol_type,
	    	error_report,
	    	ntraces_fromstn;

	char	traces_on_edit[40];


	time_t	BeginTime, 
		EndTime;

	long	pcAspp_intervals;


	XFontStruct	*display_fs;

	GC	gc_line; 
	GC	gc_point;
	GC	gc_reference;
	GC	gc_Xor;
	GC	gc_background;
	GC	gc_grid;
	GC	gc_header;

	Display	*display;
	Window	window;
	Widget	widget;

}PAGE_MGR;

/* ************************************ */
/* Structure for Point data in general  */
/* ************************************ */
typedef struct _POINT
{
	time_t 	x;
	float 	y;
	float	old_value;
	char 	mode;
	char	revision;
	int	quality_code;
	float	probability;
} POINT;


/* ******************************** */
/* Structure for Rating Trace data  */
/* ******************************** */
typedef struct _RATE_TRACE
{
	char		valid;
	char		StageOrDischarge;

} RATE_TRACE;

/* ************************************************* */
/* Structure for flood catgories for satge flow and  */
/* Stage discharge data                              */
/* ************************************************* */
typedef struct _HG_DATA
{
	char	valid;

	char	current_pe[4];

	float	flood_discharge,
	      	action_flow,
	     	flood_stage,
		action_stage,
		major,
		moderate,
		minor,
		ymax,
		ymin;

} HG_DATA;

/* *********************************** */
/* Structure for Trace data in general */
/* *********************************** */
typedef struct _TRACE_DATA
{

	TRACE_INFO	trace_info;

	int	npts,
	    	trace_color;

	float   ymin, 
		ymax,
		value_ymin,
		value_ymax;

	time_t  xmin, 
		xmax,
	        value_xmin,
		value_xmax,
		basistime;

	POINT  		TSdata[MAX_POINTS]; 

} TRACE_DATA;

/* *********************************** */
/* Structure for Graph data in general */
/* *********************************** */
typedef struct _GRAPH_DATA
{

	Dimension	x,
			y,
			w,
			h;

	int		num_traces,
	    		display_flow_unit;

	time_t		xmin,
			xmax,
			old_xmin,
	      		old_xmax,
			org_xmin,
			org_xmax;

	float		ymin,
			ymax,
			old_ymin,
			old_ymax,
			org_ymin,
			org_ymax;

	float		data_inc,
			old_data_inc,
			org_data_inc;

	int 		trace_on[MAX_TRACES];

	int 		trace_valid[MAX_TRACES];

	TRACE_DATA	traces[MAX_TRACES];

	RATE_TRACE	rdataPtr;
	HG_DATA		hdataPtr;

} GRAPH_DATA;

/* *********************************** */
/* Structure for Page  data in general */
/* *********************************** */
typedef struct _PAGE_DATA
{
	int  		num_graphs;

	GRAPH_DATA 	graph[MAX_GRAPHS];

} PAGE_DATA;


#endif

