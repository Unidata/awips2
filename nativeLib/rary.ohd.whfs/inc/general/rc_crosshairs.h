/*
	File:		rc_crosshairs.c
	Date:		12/19/1994
	Author:		Chip Gobs
	
	Purpose:	Provide support for the Rating Curve DS.
	
*/

#ifndef rc_crosshairs_h
#define rc_crosshairs_h


/*
	Data definition.
*/
typedef struct _RcCrossHairs {
	int		startx;
	int		starty;
	int		endx;
	int		endy;
	GC		gc;
} RcCrossHairs;


/*
	Data declaration.
*/
RcCrossHairs	rc_ch_data;


/*
	Function prototypes.
*/
GC	rc_setup_crosshairs(Widget widget);
void	rc_start_crosshairs();
void	rc_track_crosshairs();
void	rc_stop_crosshairs();
int	rc_get_pixel_name(Widget widget, char *name);

#endif
