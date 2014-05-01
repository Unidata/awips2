#include "map_menubar_cb.h"

/****************************************************************************
 *
 * Header: map_bcdfile.h
 *
 * Description: contains prototype definations needed for map_bcdfile.c
 *
 ***************************************************************************/

/* prototype definations */

extern void _draw_points( Pixmap map , struct _Map_Segment * seg ,
                          Boolean fillarea ) ;
extern int _read_bcdfile( char * filename , struct _Map_Segment ** seg ) ;

extern void _load_bcd_segment( int num_pts, struct _Map_Segment ** seg ,
			       int * i , float * points ) ;
