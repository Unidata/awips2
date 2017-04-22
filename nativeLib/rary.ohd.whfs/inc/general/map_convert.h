/****************************************************************************
 *
 * Header: map_convert.h
 *
 * Description: contains prototype definations needed for map_convert.c 
 *
 ***************************************************************************/

#ifndef MAP_CONVERT_H
#define MAP_CONVERT_H

#include "map_defines.h"

#define POLAR_SCALE_MULTIPLE 45
#define HRAP_REFERENCE_LON -105.0 
#define NUM_NMI_PER_DEGREE 60
#define ZOOM_OUT_LIMIT_DEFAULT 20

/* This structure is used by the functionality which allows the drawing of
   of a rectangle which can be used for zooming. */
typedef struct {
                Boolean         zoom_state ;
                Boolean         rubber_band_zoom_mode ;
                Boolean         use_rectangle ; 
                int             start_x, start_y, last_x, last_y;
                GC              gc;
                Widget          w;
               } rubber_band_data;

/* prototype definitions */

extern void _set_map_projection(int);
extern void _init_map_variables ( float nmi ) ;
extern void _update_map_variables();
extern void _pan_map_up(Widget,XtPointer,XtPointer);
extern void _pan_map_down(Widget,XtPointer,XtPointer);
extern void _pan_map_left(Widget,XtPointer,XtPointer);
extern void _pan_map_right(Widget,XtPointer,XtPointer);
extern void _zoom_map_in(Widget,XtPointer,XtPointer);
extern void _zoom_map_out(Widget,XtPointer,XtPointer);
extern void mArealZoom ( Widget w , XtPointer clientdata ,
                         XtPointer calldata ) ;
extern int _get_map_projection();
extern float _get_center_lon();
extern float _get_center_lat();
extern float _get_left_lon();
extern float _get_top_lat();
extern int _get_offsetx();
extern int _get_offsety();
extern void mConvertLatLon2XY(float,float,int *,int *);
extern void mConvertXY2LatLon(int,int,float *,float *);
extern void mSaveOriginalCenterLatLon ( ) ;
extern void mSetCenterLatLon(float,float);
extern void mGetCenterLatLon ( float * , float * ) ;
extern void _get_region(float *,float *,float *,float *);
extern void mAddPanRoutine ( MotifCallback pan_function  ) ;
extern void mAddZoomRoutine ( MotifCallback zoom_function ) ;
extern void mOptimizeScaleFactor ( long previous_scale ,
                                   double lat , double lon , double lat1 ,
                                   double lon1 , int height , int width ,
                                   char direction ) ;
extern void mSetHrapArea ( int Hrap_XOR , int Hrap_YOR , int Hrap_MAXX ,
	                           int Hrap_MAXY ) ;

#endif /* #ifndef MAP_CONVERT_H */
