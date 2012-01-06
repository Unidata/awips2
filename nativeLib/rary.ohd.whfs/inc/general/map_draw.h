/****************************************************************************
 *
 * Header: map_draw.h
 *
 * Description: contains the prototypes for map_draw.c
 *
 ***************************************************************************/

#define MAPLIB_MAX_COLORS 2
#define MAX_PLANES 1 
#define MAX_CELLS 4
#define CANNOT_OVERLAY 0
#define CAN_OVERLAY 1

/* Constants */
#ifdef _MAPLIB_SYMBOL_ROUTINES

static const unsigned int FCST_POINT_SIZE = 3 ;
static const int FCST_POINT_OFFSET_X = -2 ;
static const int FCST_POINT_OFFSET_Y = -7 ;

static const unsigned int RIVER_GAGE_SIZE = 4 ;
static const int RIVER_GAGE_X = 0 ;
static const int RIVER_GAGE_Y = 0 ;

static const int RESERVOIR_OFFSET_X  = -3 ;
static const int RESERVOIR_OFFSET_Y  = 6 ;
static const unsigned int RESERVOIR_HEIGHT = 2 ;
static const unsigned int RESERVOIR_WIDTH  = 7 ;

static const int X_OTHER_OFFSET = -2 ;
static const int Y_OTHER_OFFSET = -1 ;
static const int GENERAL_STATION_SIZE = 3 ;

#endif

/*-------------------------------------------------------------------------
 
 Prototypes definations

 --------------------------------------------------------------------------*/

extern void _area_draw_text(Pixmap,int,int,char *);
extern void _area_draw_circle(Pixmap,int,int,int,int);
extern void _area_draw_fill_circle(Pixmap,int,int,int,int);
extern void _area_fill_circle(Pixmap,int,int,int,int);
extern void _area_draw_line(Pixmap,int,int,int,int);
extern void _area_draw_lines(Pixmap,XPoint*,int);
extern void _area_draw_box(Pixmap,int,int,int,int);
extern void _area_fill_box(Pixmap,int,int,int,int);
extern void _area_draw_fill_box ( Pixmap map , int x , int y , int width ,
                                  int height );
extern void _area_draw_fill_polygon ( Pixmap map , XPoint * pts , int num ,
                                      int shape , int mode ) ;
extern int _area_draw_planes ();
void _area_draw_fill_half_circle(Pixmap map,int x,int y,int width,int height);
