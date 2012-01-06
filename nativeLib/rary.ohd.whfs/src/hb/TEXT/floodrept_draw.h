/*
	File:		floodrept_draw.h
	Date:		May 1997
	Author:		Paul Taylor

	Purpose:	To provide drawing routines for Flood Report DS.
*/


#ifndef floodrept_draw_h
#define floodrept_draw_h


/*
	Includes.
*/
#include <Xm/Xm.h>
#include <time.h>
#include <stdlib.h>
#include "time_series.h"
#include "time_defs.h"
#include "FloodTs.h"
#include "LoadUnique.h"
#include "floodrept_hairs.h"
#include "floodrept_show.h"


/*
	Definitions.
*/
#define LEFT_OFFSET		0
#define RIGHT_OFFSET		0
#define BOTTOM_OFFSET		90 /* was 40, which caused VERY bad scaling */
#define TOP_OFFSET		10
#define LINE_OFFSET		6
#define LABEL_OFFSET		50
#define POINTS_OFFSET		15

#define PIXELS_PER_HOUR         10
#define MIN_MAINDA_WIDTH	570
#define MAX_MAINDA_WIDTH	19000

#define NUM_VERTICAL_INTERVALS  5


/*
	Drawing-Related functions/callbacks.
*/

Position getFrWinX	(double xdata,  FloodReportWorkArea *frwa);
Position getFrWinY	(double ydata,  FloodReportWorkArea *frwa);
double   getFrDataY	(Position ywin, FloodReportWorkArea *frwa);
double   getFrDataX	(Position xwin, FloodReportWorkArea *frwa);

void	floodrept_setYAxisMaxMin	(void);
void    floodrept_setBeginEndTimes	(void);

void    floodrept_getFloodEvent		(void);

void	floodrept_drawFloodEvent	(void);
void	floodrept_drawYAxisLabel	(void);
void	floodrept_drawPoints		(void);
void	floodrept_drawYAxis		(void); 
void	floodrept_drawTAxis		(void);
void	floodrept_drawFloodLine		(void);

void	floodrept_redrawAxis	(Widget w, XtPointer ptr, XtPointer cbs);
void	floodrept_redrawMain	(Widget w, XtPointer ptr, XtPointer cbs);



#endif




