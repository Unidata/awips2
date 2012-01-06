#ifndef WRITE_LINESEGS_H
#define WRITE_LINESEGS_H


#include "DbmsUtils.h"      /* database functions */
#include "DbmsDefs.h"
#include "LineSegs.h"
#include "HrapBinAssign.h"

/* prototypes */

void write_lineseg(const char *area_id, const HrapBinList *binList); 

#endif
