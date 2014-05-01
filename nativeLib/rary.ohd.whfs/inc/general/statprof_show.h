#ifndef statprof_show_h
#define statprof_show_h

/*
	Includes.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>

#include "statprof.h"

#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "Xtools.h"
#include "time_convert.h"
#include "get_curobs_maxfcst.h"
#include "Report.h"
 
#include "Riverstat.h"
#include "StatProf.h"


/*
	Defines.
*/
#define ELEV_INCREMENTS		5
#define ELEV_TOP_OFFSET		5
#define ELEV_LOW_OFFSET		45
#define MIN_RIVER_MILES		400

#define SCALE_ROUNDING 5

#define LABEL_VERT_INCREMENT 15

#define PIXEL_USED 1
#define PIXEL_FREE 0
#define PROFILE_BUF_SIZE 128

#define PROFILE_TEXT_HEIGHT 10
#define PROFILE_TEXT_WIDTH 10

#define DOWN 1
#define UP -1

#define LABEL_Y_PAD 2

typedef struct LabelPosition 
{
   int lid_x;
   int lid_y;
   int elev_x;
   int elev_y;
   char lid_buf [ PROFILE_BUF_SIZE + 1 ];
   char elev_buf [ PROFILE_BUF_SIZE + 1 ];
} LabelPosition;

/*
	Function prototypes.
*/
void	statprof_show(Widget w, const char *lid);
void	statprof_close(Widget w, XtPointer ptr, XtPointer cbs);
void	statprof_load(Widget w, XtPointer ptr, XtPointer cbs);
void	statprof_callbacks(void);
void	statprof_draw();
void	statscale_draw();

void load_statprof_reports();


void	create_station_buttons(void);
void	draw_station_profile(Widget w);
void	draw_station_scale(Widget w);
float	get_max_elevation(StatProf *station);
float	get_min_elevation(StatProf *station);
float	get_max_mile(StatProf *station);
float	get_min_mile(StatProf *station);


#endif
