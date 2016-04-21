/*=========================================================================*/
/*         FILE PATH/NAME:   st3_includes/stageiii_structures.h            */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*=========================================================================*/

#include <X11/Intrinsic.h>
#include "hrap.h"

typedef struct {
    char id[4];
    float xlat;
    float xlong;
    int   on;
    point  ctr;
    int   ngrd;
    int   ngage;
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

