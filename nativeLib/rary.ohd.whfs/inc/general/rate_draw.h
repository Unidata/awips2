#ifndef rate_draw_h
#define rate_draw_h

/*
	Rating Curve defines.
*/
#define BORDER          50
#define R_BORDER        80
#define T_BORDER        (BORDER + R_BORDER)
#define MAX_CFS         100000.0
#define MAX_DIVISOR	100000
#define SMALLEST_MAX_FLOW 10000 /* smallest value _max_flow is allowed to be */ 

double	_max_flow;

float	maxstage,
	minstage;

#endif
