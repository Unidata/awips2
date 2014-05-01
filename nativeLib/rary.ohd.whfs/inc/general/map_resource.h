/****************************************************************************
 *
 * Header: map_resource.h
 *
 * Description: contains the prototypes for map_resource.c
 *
 ***************************************************************************/

/*-------------------------------------------------------------------------
 
 Prototypes definations

 --------------------------------------------------------------------------*/
#ifndef MAP_RESOURCE_H
#define MAP_RESOURCE_H

#include "map_defines.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void _create_gc(Widget);
extern void _create_pixmaps ( int index, int width, int height );
extern unsigned long _get_color(char *name);
extern XtAppContext _get_map_context ( );
extern Display *_get_map_display();
extern GC _get_map_gc();
extern void _get_map_resources(Widget);
extern Widget _get_top_level ( ) ;
extern void mSetCursor(int);
extern void _set_foreground(char *);
extern void mSetGC ( enum MapGCtype gc_type , char * color ) ;
extern void _create_2nd_map ( );

extern Screen *_get_screen();
extern void mSetPDSize(int);

unsigned int FCST_POINT_SIZE;
float FCST_POINT_OFFSET_X;
int FCST_POINT_OFFSET_Y;
unsigned int RIVER_GAGE_SIZE;
int RESERVOIR_OFFSET_X;
int RESERVOIR_OFFSET_Y;
unsigned int RESERVOIR_WIDTH;
float X_OTHER_OFFSET;
int Y_OTHER_OFFSET;
int GENERAL_STATION_SIZE;

int set_pdsize_flag;
void define_pdsize(int set_pdsize_flag);

#ifdef __cplusplus
}
#endif
#endif /* #ifndef MAP_RESOURCE_H */
