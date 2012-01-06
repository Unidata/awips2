#ifndef MAP_BACKGROUNDS_H
#define MAP_BACKGROUNDS_H


/***********************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/types.h>

#include <Xm/Xm.h>

#include "City.h"
#include "RadarLoc.h"
#include "cvt_latlon.h"
#include "HvDisplayControl.h"
#include "HvColorList.h"
#include "geoutil.h"
#include "drawStations.h" /* for X and Y LETTER_OFFSET */
#include "convert_hrap.h"
/***********************************************************************************/

/*
	Radar defines

*/
#define MAX_RADARS 133
#define MAX_X 1000
#define MAX_Y 1000

#ifndef PI
#define PI 3.14159
#endif 

#define RAD_PER_DEG (PI/180)	/* radians/degree */
#define RADAR_COVER_M 230000.0  /* radar coverage radius in meters */
#define M_PER_NMILE 1852.0	/* meters/nautical mile */
#define NMILE_PER_DEG_LAT 60    /* nautical miles/degree latitude */
				/* 
				   nautical miles/degree longitude is
				   60*cosine_in_degrees_of_the_latitude
				*/
				

#define MAX_RECORD_LEN 80

#define AREA_ID_X_OFFSET 0*X_LETTER_OFFSET
#define AREA_ID_Y_OFFSET 0*Y_LETTER_OFFSET
#define AREA_NAME_X_OFFSET 0*X_LETTER_OFFSET
#define AREA_NAME_Y_OFFSET 2*Y_LETTER_OFFSET


#define	AREA_VALUE1_X_OFFSET 0*X_LETTER_OFFSET
#define AREA_VALUE1_Y_OFFSET 1*Y_LETTER_OFFSET
#define	AREA_VALUE2_X_OFFSET 0*X_LETTER_OFFSET
#define AREA_VALUE2_Y_OFFSET 2*Y_LETTER_OFFSET





/***********************************************************************************/

/***********************************************************************************/

/*
	prototypes
*/



char * get_name(char *buf);

/*
	radar 
*/
extern M_XYPoint M_lonlat2xy(M_MapPoint pt);
int inRangeCorner(M_XYPoint pt);
void printM_XYPoint(M_XYPoint point);

void  drawMapBackground ( HvDisplayControl * hdc , HvColorList * hcl ) ;

void	readMapAreasFromFile(MapArea **area_ptr, long *num_ptr, char *fileName);
void	readMapLinesFromFile(MapLine **line_ptr, long *num_ptr, char *fileName);
void	readMapPointsFromFile(MapPoint **point_ptr, long *num_ptr, char *fileName);

void	readMapAreasFromDb(MapArea **area_ptr, long *num_ptr, char * boundaryType);
void    readMapLinesFromDb(MapLine **lines_ptr, long *num_ptr, char *vectorType, long feature_rank);
void	readMapPointsFromDb(MapPoint **point_ptr, long *num_ptr,
			    char * boundaryType);



void	read_cities(MapPoint **cityPts_ptr, long *num_ptr,
		    long disp_precedence);

void	read_radars(MapPoint **radarPts_ptr, long *num_ptr);

void	drawMapAreas(MapArea *areas, long numAreas,
		     char * boundaryColor,
		     char * fillColor,
		     Boolean fillArea,
		     Boolean showLabel,
		     HvDisplayControl *hdc,
		     GC gc, Display *dpy, Pixmap pix);

void	drawMapArea(MapArea *area, 
		    char * boundaryColor,
		    char * fillColor,
		    Boolean drawArea,
		    Boolean fillArea,
		    Boolean showId,
		    Boolean showName,
		    Boolean showValue1,
		    Boolean showValue2,
		    GC gc, Display *dpy, Pixmap pix);

void	drawMapLines(MapLine *lines, long numLines, char * lineColor,
		    Boolean showLabel, GC gc, Display *dpy, Pixmap pix);
void	drawMapPoints(MapPoint *points, long numPoints, char * pointColor,
		    Boolean showLabel, GC gc, Display *dpy, Pixmap pix);

void	draw_radars(MapPoint *radarPts, long num_radars,
		    char * radar_color,
		    GC gc, Display *dpy, Pixmap pix);

void    drawRadarRing(MapPoint radarPt,
		    char * radar_color,
		    GC gc, Display *dpy, Pixmap pix);
#endif
