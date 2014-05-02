/*=========================================================================*/
/*                           FILE NAME:   drawa.h                          */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*=========================================================================*/

#ifndef DRAWA_H
#define DRAWA_H

#include <Xm/Xm.h>

#include "List.h"
#include "mpe_field_names.h"
#include "stage3.h"

#define NUM_MAP_SCREENS 2

void  fill_pixmap ( Pixmap user_pixmap ) ;

int get_vip_level(int num_levels, int *levels, int value);
int get_vip_level_allow_negative(int num_levels, int *levels, int value);

void  MPEUtil_copy_area ( Widget w , XtPointer client_data , XEvent * calldata ,
                  Boolean * flag ) ;

typedef struct  {
		 int    **data_array;
		 int    maximum_columns, maximum_rows;
		 int    num_levels;
		 int    *levels;
		 int    rfc_on;
		 int    states_on;
		 int    county_on;
		 int    rivers_on;
                 int    basins_on;
		 int    rings_on;
		 int    cities_on;
		 int    gages_on;
		 int    ssnum;
		 point  origin;
		 GC     *gc;
		 Pixmap pix;
		 Pixmap pixbase;
		 Widget w;
                 enum DisplayFieldData field_type;
                 int first_display;
                 int previous_num_levels;
                 char cv_use[16];
                 int cv_duration;
		 } draw_struct;
		 
 draw_struct rad_data [ NUM_MAP_SCREENS ];
draw_struct      *msdata, *ggdata, *st1idata, *st1iidata;
draw_struct      eddata;
draw_struct      zoom_data;
draw_struct      tl_draw_data; 
int        **data_array_tmp;

Dimension        screenWidth;
Dimension        mapWidth, mapHeight;
int              rectangleWidth;

typedef struct
	{
	 Widget    w;
	 int       nhrs;
	 draw_struct draw;
	} time_lapse_struct;

typedef struct {
   Widget scale1;
   Widget scale2;
   Widget level[17];
   Widget button[17];
   int    num;
   }  level_struct;

#endif /* #ifndef DRAWA_H */
