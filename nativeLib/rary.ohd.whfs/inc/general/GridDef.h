#ifndef GRID_DEF_H
#define GRID_DEF_H

#define GRID_SOURCE_ID_LEN 80
#define GRID_FILENAME_LEN 80
#define COLOR_THRESHOLD_SET_NAME_LEN 80

#include "ArealProductType.h"
#include "ColoredPoint.h"
#include "ColorThreshold.h"
#include "TimePeriod.h"
#include "datetime.h"

typedef struct _Grid
{
     ArealProductType productType;
   
     char sourceId[GRID_SOURCE_ID_LEN];
     
     char calculated[2]; /* n, c, z */
     char fileName[GRID_FILENAME_LEN];
     
     char colorThresholdSetName[COLOR_THRESHOLD_SET_NAME_LEN];
     
     
     dtime_t		 obstime;
     
     TimePeriod          timePeriod;
     
     
     ColoredPointArray   pointArray;
     long	numRows;
     long	numColumns;
     
     ColorThresholdArray thresholdArray;
     
} Grid;

#endif
