#ifndef COLORED_POINT_H
#define COLORED_POINT_H

#include "ColorThreshold.h"
#include "HvColorList.h" /* Included for "MAX_COLOR_NAME_LENGTH" . */
#include "ToolDefs.h"  /* for MISSING */

typedef struct _ColoredPoint
{
	double lat,
	       lon;
	
	double row,
	       col;
	       
	double value;	

	/*Color */
        char color [ MAX_COLOR_NAME_LENGTH ] ;
     
} ColoredPoint;


typedef struct _ColoredPointArray
{
	ColoredPoint *points;
	long length;
   
} ColoredPointArray;
   

int allocateColoredPoints(ColoredPointArray *pointArray, long numPoints);
void freeColoredPointArray(ColoredPointArray *pointArray);
void  printColoredPointArray(ColoredPointArray *pointArray, char *fileName);

void printPointArrayGrid(ColoredPointArray *pointArray,
			    int numRows, int numColumns,
			    char *fileName);
void initColoredPoint(ColoredPoint *point);

#endif
