/*******************************************************************************
* FILENAME:             polygon_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototype for the 
*                       start_end_rubber_poly_RFCW routine 
*                       in the polygon_RFCW.c source file.
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 14, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                   PROGRAMMER       DESCRIPTION/REASON
* February 14, 2002     Moria Shebsovich    Original Coding 
********************************************************************************
*/

#ifndef POLYGON_RFCW_H
#define POLYGON_RFCW_H

#include "display_field_data_RFCW.h"
#include "List.h"
#include "stage3.h"

#define M_INDEX 0
#define MAX_POINTS_IN_POLYGON 20
#define POLY_ACTION_LENGTH 20
#define POLY_VALUE_LENGTH 20
#define POLY_FILENAME_LENGTH 120
#define REGION_SCALE_FACTOR 10

/* Define the rubber poly data structure.  This contains information
   about polygons drawn on the display by the user for the purpose of
   editing precipitation data.  This used to live in the drawa.h 
   header file in the mpe_util library.  It was moved on June 26, 2003. */

typedef struct
{
         Node   node;
         int    polygon_number ;
         int    npoints;
         int    max_points;
         HRAP * hrap; 
         GC     gc;
         Widget shell;
         int    isite;
         int    xpoly,xpt;
         int    close;
         float draw_precip_value ;
         Boolean snow_flag ;
         Boolean set_flag ;
         Boolean raise_flag ;
         Boolean lower_flag ;
         Boolean scale_flag ;
         Boolean sub_flag ;
         enum DisplayFieldData draw_source ;
         Boolean visible;
         Boolean persistent;
	 float minx;
         float maxx;
         float miny;
         float maxy;
         List   list;
} rubber_poly_data;

/* Function prototypes. */
void start_end_rubber_poly_RFCW ( Widget w , XtPointer clientdata, 
                                  XEvent * event , 
                                  Boolean * continue_to_dispatch_return ) ;

#endif /* #ifndef POLYGON_RFCW_H */
