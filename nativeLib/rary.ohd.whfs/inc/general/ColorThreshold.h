#ifndef COLOR_THRESHOLD_H
#define COLOR_THRESHOLD_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "Xm/Xm.h"
#include "Xtools.h"  /* should be GeneralUtil.h eventually!!!! */
#include "DbmsDefs.h"
#include "ColorValue.h"
#include "LoadUnique.h"

typedef struct _ColorThreshold
{
        /*
		<value> is the threshold for the location
		to be allowed to be colored according to <color>
	*/  
   	double value;
	char colorName [ COLOR_NAME_LEN+1 ] ;
	   
} ColorThreshold;


typedef struct _ColorThresholdArray
{	
	char missingColorName[COLOR_NAME_LEN+1];
	char defaultColorName[COLOR_NAME_LEN+1];
	ColorThreshold *thresholds;
   	long length;

} ColorThresholdArray;

/*
	prototypes
*/
        

int loadColorThresholdArrayFromFile(ColorThresholdArray *ctArray,
				     FILE *fp, Widget widget);

const char * determineColorByThreshold(double value, double missingValue,
			               const ColorThresholdArray *colorArray);

void copyColorThresholdArray(ColorThresholdArray *dest,
			     const ColorThresholdArray *source);

void freeColorThresholdArray(ColorThresholdArray *ctArray);

void printColorThresholdArray(const ColorThresholdArray *colorArray);

void loadDefaultColorThresholdArray(ColorThresholdArray *ctArray,
				    Widget widget);


#endif
