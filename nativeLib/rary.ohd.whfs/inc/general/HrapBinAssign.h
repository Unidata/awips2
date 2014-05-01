#ifndef HRAP_BIN_ASSIGN_H
#define HRAP_BIN_ASSIGN_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "LineSegment.h"

#include "linesegs_defs.h"

#include "geoutil.h"                   /* utilities */
#include "convert_hrap.h"


/* prototypes */

void getHrapBinListFromSegments(const LineSegment segments[],
			        long numSegments,
			        HrapBinList *binList);

double getHrapBinArea(double row, double col);
   
void printHrapBinList(const HrapBinList *binList);
   
#endif
