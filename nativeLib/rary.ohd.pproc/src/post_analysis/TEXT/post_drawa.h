/*=========================================================================*/
/*                           FILE NAME:   drawa.h                          */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*=========================================================================*/
#ifndef POST_DRAWA_H
#define POST_DRAWA_H

#include "post_stage3.h"

void    fill_pixmap();
void    fill_pixmap_radar_site();
int     get_vip_level();
void    copy_area();

typedef struct  {
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
		 
draw_struct      *msdata, *ggdata, *st1idata, *st1iidata;
draw_struct      rad_data;
draw_struct      eddata;
draw_struct      zoom_data;
draw_struct      tl_draw_data;
short int        **data_array_tmp;
Dimension        screenWidth;
Dimension        mapWidth, mapHeight;
int              rectangleWidth;

typedef struct
	{
	 Widget    w;
	 int       nhrs;
	 draw_struct draw;
	} time_lapse_struct;

#define MAXPOLY 10
#define MAX_POINTS_IN_POLYGON  20

typedef struct
	{
	 int    npoly;
	 int    npoints[MAXPOLY];
	 XPoint points[MAXPOLY][MAX_POINTS_IN_POLYGON];
	 GC     gc;
	 Widget shell;
	 int    isite;
	 int    xpoly,xpt;
	 int    close;
	} rubber_poly_data;

typedef struct {
   Widget scale1;
   Widget scale2;
   Widget level[17];
   Widget button[17];
   int    num;
   }  level_struct;



time_lapse_struct       tldata;

rubber_poly_data        polydata,polydata_orig;

#endif
