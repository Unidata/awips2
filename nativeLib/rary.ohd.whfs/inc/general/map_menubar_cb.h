#ifndef MAP_MENUBAR_CB_H
#define MAP_MENUBAR_CB_H

#include <Xm/Xm.h> 

#include "map_defines.h"

/****************************************************************************
 *
 * Header: map_menubar_cb.h
 *
 * Description: contains the prototypes for map_menubar_cb.c
 *
 ***************************************************************************/

/*-------------------------------------------------------------------------

 User-defined constants.

 --------------------------------------------------------------------------*/
#define OVERLAY_PATH_LEN 300 
#define OVERLAY_FILENAME_LEN 50
#define MAX_NUMBER_OF_FILES 3
#define COLOR_NAME_LENGTH 30
#define MAPLIB_DEFAULT_LINE_WIDTH 1
#define MAX_MAP_STRING_SIZE 1024

/*-------------------------------------------------------------------------

 User-defined types.

 --------------------------------------------------------------------------*/

typedef struct _Map_Point {
  float lat;
  float lon;
} M_MapPoint ;

struct _Map_Segment {
  int num_points;
  struct _Map_Point *points;
  struct _Map_Segment *next;
};

typedef struct _M_XYPoint {
   long x;
   long y;
} M_XYPoint ;

typedef struct _M_IntXYPoint {
   int x ;
   int y ;
} M_IntXYPoint ;

/* The user has the option of supplying a routine to calculate a
   map overlay.  This routine must take two arguments.  The index
   number of the map and the pixmap to do the actual drawing on. */
typedef void ( * OverlayCalculatorRoutine ) ( int index ,
                                              void * pOverlay ,
                                              Pixmap map ,
                                              void * pUserData ) ;

/* The user has the option of supplying a routine to read
   geographical information from a database table. */
typedef void ( * ExternalOverlayRoutine ) ( void * pData ,
				            int item ,
                                            struct _Map_Segment ** seg ,
                                            enum ShapeFileType * type ,
                                            Boolean * fillarea ) ;

struct _Overlay {
  int  status; /* Indicates whether or not to draw the overlay. */
  char color [ COLOR_NAME_LENGTH ]; /* The color of the overlay. */
  char filepath [ MAX_NUMBER_OF_FILES ]
                    [ OVERLAY_PATH_LEN ] ; /* The path to the bcd file(s). */
  char filename [ MAX_NUMBER_OF_FILES ]    /* The name(s) of the bcd */
                    [ OVERLAY_FILENAME_LEN ] ; /* file(s). */
  int n_files ; /* The number of shape files for this overlay. */
  enum MapOverlayTypes type ; /* The type of the file. */
  enum ShapeFileType shape_type ; /* The type of the shapefile. */  
                                  /*    for bcd files. */
  int store_in_memory ; /* A flag indicating whether or not to store the
                           geographic item in memory. This will save from
                           having to read the data source every time, but it
                           could be expensive in terms of memory. */
  struct _Map_Segment * seg [ MAX_NUMBER_OF_FILES ] ; /* If the user wants to
                                                         keep the geographic
                                                         information in
                                                         memory for quicker
                                                         access, it is kept
                                                         here. */
  OverlayCalculatorRoutine pCalculateOverlay ;
  ExternalOverlayRoutine pExternalOverlay ;
  Boolean fillarea ;
  int line_width ;

} ;

struct _RIVERS
{
  char wfo[4];
  char rfc[4][6];
  int num_of_rfcs;
  char bcd_filepath   [ OVERLAY_PATH_LEN ] ;
  char bcd_filename   [ OVERLAY_FILENAME_LEN ] ;
  char shape_filepath [ OVERLAY_PATH_LEN ] ;
  char shape_filename [ OVERLAY_FILENAME_LEN ] ;
  int type ;
  int store_in_memory ;
  struct _Map_Segment *seg;
} ;

/*-------------------------------------------------------------------------
 
 Prototypes definations

 --------------------------------------------------------------------------*/

void set_user_overlays_counter_and_names();

extern void _draw_map_overlays ( int ) ;

extern void _set_overlay(Widget,XtPointer,XtPointer);

extern void _set_projection(Widget,XtPointer,XtPointer);

extern void _init_overlays();

extern void _delete_overlays ( ) ;

extern int _get_points_in_segment();

extern void _get_point_coordinate(int,float *,float *);

extern int _allocate_segment(int);

extern int _allocate_first_segment( int , struct _Map_Segment ** seg ) ;

extern void _deallocate_segments ( struct _Map_Segment ** seg ) ;

extern void _load_points ( float * points ) ;
extern void _load_coords ( double * lat , int negate_lat ,
                           double * lon , int negate_lon ) ;

extern void _set_first_segment ( struct _Map_Segment * seg ) ;

extern struct _Map_Segment * _next_segment ( );

extern void _close_map_screen(Widget,XtPointer,XtPointer);

extern void _save_image(Widget,XtPointer,XtPointer);

extern void _change_projection(Widget,XtPointer,XtPointer);

extern void _print_image(Widget,XtPointer,XtPointer);

extern void _print_image_reverse(Widget,XtPointer,XtPointer);

extern void _show_legend(Widget,XtPointer,XtPointer);

extern void _show_toolbar(Widget,XtPointer,XtPointer);

extern void _about_cb ( Widget , XtPointer , XtPointer ) ;

extern void _recenter_cb(Widget,XtPointer,XtPointer);

extern void _set_recenter_flag(int);

extern int _get_recenter_flag();

extern int _get_overlay_status ( int item ) ;

extern void _set_overlay_status ( int item , 
                                  enum MapState state ) ;
				  
extern void _draw_bcdfile_overlay ( char * color , 
		                    int line_width ,
		                    const char * fpath ,
                                    const char * fname ,
                                    struct _Map_Segment ** seg ,
                                    Pixmap map ,
                                    enum MapState store_in_memory ,
                                    Boolean fillarea ) ;
extern void _set_map_font ( Widget wid , XtPointer clientdata ,
                            XtPointer calldata ) ;
extern void _draw_topography_overlay ( int index ) ;

extern void _set_map_pdsize ( Widget wid , XtPointer clientdata ,
                            XtPointer calldata ) ;

#endif /* #ifndef MAP_MENUBAR_CB_H */
