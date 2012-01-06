/* structures.h */

#ifndef structures_h
#define structures_h

#include <X11/Intrinsic.h>

typedef struct HRAP {
   float x,y;} HRAP;

typedef struct {
   int x,y;
   } point;

typedef struct {
    char id[8];
    int npts,order;
    HRAP *hrap;
    } overlay_struct;

typedef struct {
    char id[20];
    int npts;
    HRAP *hrap;
    } county_struct;

typedef struct {
    char id[4];
    float xlat;
    float xlong;
    int   on;
    HRAP  ctr;
    int   ngrd;
	} nex_struct;


typedef struct
	{
	    short int    **data_array;
	    int    maximum_columns, maximum_rows;
	    int    num_levels;
	    int    *levels;
	    int    states_on;
	    int    county_on;
	    int    rivers_on;
	     int    basins_on;
	    int    rings_on;
	    int    cities_on;
	    int   ssnum;
	    point  origin;
	    GC     *gc;
	    Pixmap pix;
	    Pixmap pixbase;
	    Widget w;
	  } draw_struct;

#endif
