
#ifndef GRID_H
#define GRID_H

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "GridDef.h"
#include "HvDisplayControl.h"
#include "ColoredPoint.h"
#include "ColorThreshold.h"
#include "convert_hrap.h"
#include "DPARadar.h"
//#include "Stage2Result.h"
#include "RadarLoc.h"




void initGrid(Grid *grid, const ArealProduct *product,
	      FfmComparisonType comparisonType);

	
void freeGrid(Grid *grid);  
   
void colorizeGrid(ColoredPointArray *pointArray,
		   const ColorThresholdArray *colorArray);



void drawGrid(Grid *grid);

     void drawHrapBin(ColoredPoint point);
  
     
void fillHrapPointArray(ColoredPointArray *cpArray, float *sourceArray,
		        long rows, long cols,
		        double radarLat, double radarLon);


/*
	Determine what color a point should be
*/

void determineAllPointColors(ColoredPointArray *pointArray,
		                 const ColorThresholdArray *colorArray);



/*
	utility functions
*/
float * compareGridArray(float *ffgGrid,
			 float *precipGrid,
			 long gridSize,
			 FfmComparisonType comparisonType);

void printFloatArray(float *floatArray,
		     int numRows, int numColumns, char *fileName);  
void printShortArray(short *shortArray,
		     int numRows, int numColumns, char *fileName);


int getRadarLatLon(char *radarId, double *radLat, double *radLon);
void getCorners(ColoredPoint point, XPoint corners[]);
void HrapToXPoint(double row, double col, XPoint *xPoint);

void printXPoint(FILE *fp, XPoint point, char *string);


#endif
