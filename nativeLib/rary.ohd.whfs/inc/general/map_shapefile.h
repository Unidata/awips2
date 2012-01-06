/****************************************************************************
 *
 * Header: map_shapefile.h
 *
 * Description: contains prototype definations needed for map_shapefile.c
 *
 ***************************************************************************/
#ifndef MAP_SHAPEFILE_H
#define MAP_SHAPEFILE_H

#include <Xm/Xm.h>
#include "map_defines.h"
#include "map_menubar_cb.h"

/* Manifest constant definitions */
#define SHAPEFILE_HEADER_LENGTH 100
#define SHAPEFILE_POINT_RECORD_LENGTH 28
#define SHAPEFILE_POLYGON_NUMPARTS_POS 36 
#define SHAPEFILE_POLYGON_NUMPOINTS_POS 40 
#define SHAPEFILE_POLYGON_PARTS_POS 44

/* Type defintions. */

/* prototype definations */

extern void _draw_shapefile ( char * filename ,
                              struct _Map_Segment ** seg ,
                              Pixmap map ,
                              int store_in_memory ,
                              enum ShapeFileType * shape_type ,
                              Boolean fillarea ) ;

void _draw_shapefile_data ( enum ShapeFileType type , 
                            Pixmap map ,
                            struct _Map_Segment * seg ,
                            Boolean fillarea ) ;

extern int _get_shapefile_point ( int fp ,
                                  double * lat ,
                                  double * lon ) ;

#endif /* #ifndef MAP_SHAPEFILE_H */
