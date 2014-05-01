/*
	File:		floodrept_show.h
	Date:		May 1997
	Author:		Paul Taylor

	Purpose:	Provide support for Flood Report DS.
*/


#ifndef floodrept_show_h
#define floodrept_show_h


#include "DbmsDefs.h"

#include "FloodTs.h"
#include "LoadUnique.h"

#include "floodrept_hairs.h"


/*
	Defines.
*/
#define	MSG		-999

#define	MAX_TEXT_LEN	50	/* used for aboveTE, crestTE, & belowTE */

#define	MAX_NUM_EVENTS	10000	/* LIMITATION: XmList ordering isn't
				   guaranteed past this many events.  This is
				   a workaround solution to LoadUnique()'s
				   inability to order data properly if a
				   "field" contains an integer column.
				   
				   (e.g. "lid||flood_event_id", where
				    flood_event_id is of type integer.) */

#define	INDEX_PT_A	0
#define	INDEX_PT_B	1
#define	INDEX_PT_C	2
#define	INDEX_PT_D	3
#define	INDEX_ANCHOR1	4
#define	INDEX_ANCHOR2	5	/* used for PtsMissing */

#define MAX_HSA_COUNT 125



/*
	Structures.
*/
typedef	struct _Month {
   long		month;		/* 1 thru 12, inclusive */
   long		days;		/* # of days in given month */
} MonthDays;

typedef	struct _PtsMissing {
   long		index;		/* e.g. INDEX_PT_A or INDEX_CREST */
   Boolean	state;
} PtsMissing;
   
   

typedef struct FloodReportWorkArea_st
{
   	GC		axisGC;			/* AXIS info */
	Pixmap		axisPM;
	Window		axisWindow;
	Widget		axisDA;
	Dimension	axisHeight,		/* (dimensions of axisDA) */
	   		axisWidth;

	double		min_y,
	   		max_y;
	int		interval;
	
	GC		mainGC;			/* MAIN info */
	Pixmap		mainPM;	
	Window		mainWindow;
	Widget		mainDA;
	Dimension	mainHeight,		/* (dimensions of mainDA) */
	   		mainWidth;
	
	time_t		beginTime,		/* for display window's x-axis*/
			endTime;

	UniqueList	*ulPtr;			/* for begin/end of Events */
	long		max_num_events;
	
	time_t		beginEventsTime,	/* for begin/end of Events */
			endEventsTime;
			

	FloodTs		*floodtsPtr;		/* for ACTIVE flood Event */
	double		fs;			/* for ACTIVE flood Event */
	
	char		hsa[HYD_SERV_LEN+1];	/* for hsa to use in filtering */		
	char 		availableHsaArray[MAX_HSA_COUNT][HYD_SERV_LEN+1];  /* possible HSAs */
	int             hsaCount;		/* count of actual HSAs*/
	
	
	Display		*display;		/* GENERAL info */
	XFontStruct	*finfo;
	CrossHairs	crosshairs;
} FloodReportWorkArea;




/*
	General functions/callbacks.
*/
void	floodrept_show		(Widget w);
void	floodrept_callbacks	(void);

void	floodrept_omCB		(Widget w, XtPointer ptr, XtPointer cbs);
void	floodrept_hsa_cbxCB    (Widget w, XtPointer ptr, XtPointer cbs);
void	floodrept_selectCB	(Widget w, XtPointer ptr, XtPointer cbs);
void	floodrept_closeCB	(Widget w, XtPointer ptr, XtPointer cbs);
void	floodrept_refreshCB	(Widget w, XtPointer ptr, XtPointer cbs);
void	floodrept_deleteCB	(Widget w, XtPointer ptr, XtPointer cbs);

void    insert_cresttable       (Widget w, XtPointer ptr, XtPointer cbs);
void    floodrept_insert        (Widget w, XtPointer ptr, XtPointer cbs);
void	floodrept_delete	(void);



/*
	Memory-Related functions.
*/
FloodReportWorkArea*	getFrWorkArea(void);
FloodReportWorkArea*	createFrWorkArea(void);



/*
	XmList-Related functions.
*/
void	floodrept_loadXmList(void);
			      
int	floodrept_ParseUnique(UniqueList 	*ulPtr, 
			      char 		*prev_lid,
			      char 		*lname, 
			      char 		*lid,
			      double 		*crest, 
			      dtime_t		*crest_dt,
			      double 		*fldstg);

void	floodrept_FreeUnique(void);

FloodTs	floodrept_findCrest(FloodTs *floodtsPtr, double fs,
			    FloodTs *last_crestPtr);

long	floodrept_computeMaxNumEvents(char *where);

void list_event_vals(FloodTs *floodtsEvent);



/*
	Time-Related functions.
*/
void	floodrept_setReportingPeriod(int menu_pos, time_t *beginEventsTime,
				     time_t *endEventsTime);


void	floodrept_loadSignificantTimes(void);

/*
	HSA-related functions.
*/
void floodrept_setHsa(int selectedPosition, char * hsa);
void floodrept_InitHsaArray(int * hsaCount);
void floodrept_fill_hsa_CBX();


int  get_passthru_times(FloodTs 	*floodtsPtr,
			float		fs,
			time_t		*above_timet,
			time_t		*below_timet);

void load_passthru_stages(FloodTs       *floodtsHead,
			  double	fldstage,
			  time_t	*above_time,
			  time_t	*below_time);

/* export related */

void export_floodts_file(Widget w, XtPointer ptr, XtPointer cbs);
void select_floodts_file();
void cancel_floodts_file();
void sort_events(UniqueList	*ulHead,
		 int		*sort_order);


#endif




