#ifndef CREATE_OFS_LINESEGS_H
#define CREATE_OFS_LINESEGS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "LineSegment.h"               /* database */

#include "geoutil.h"                   /* utilities */
#include "convert_hrap.h"

#include "HrapBinAssign.h"



/* prototypes */

void getHrapBinList_OFSArea(long numPoints,
			    double latArray[], 
			    double lonArray[],
			    long rows[],
			    long beginCols[],
			    long endCols[],
			    long *numRows,
			    long *numBins,
			    double *area);

#endif
