#ifndef CREATE_GEOAREA_LINESEGS_H
#define CREATE_GEOAREA_LINESEGS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "LineSegment.h"               /* database */

#include "geoutil.h"                   /* utilities */
#include "convert_hrap.h"

#include "HrapBinAssign.h"
#include "write_lineseg.h"
#include "geo_header_file.h"

/* definitions */


/* prototypes */

void createLinesegsForAreas(FILE * infile , char * geotype , int rank ,
                            int geotable , int verbose, int printOnly);
void createLinesegsForArea(GeoAreaData *gPtr, int verbose, int printOnly);

void getHrapBinListForArea(GeoAreaData *gPtr, HrapBinList *binList);

Point * getPointsFromArea(GeoAreaData *gPtr, long *numPoints); 

#endif
