#ifndef PRECIP_TOTAL_H
#define PRECIP_TOTAL_H

#include <time.h>
#include "DbmsDefs.h"


/* for a given duration, a value is missing if no data are found;
   it is rejected if less than some threshold percent of data
   is found; it is ignored if the existing data covered more of
   the duration that the duration covered by the data just analyzed. */

#define MISSING_PRECIP  -9999.

#define LARGE_DIFF 10.0

#define OK_CHAR ' '
#define MISSING_CHAR  'm'
#define REJECTED_CHAR 'r'


/* error conditions */

typedef struct
{ 
   int negval;
   int negdiff;
   int largediff;
} data_err_struct;
 
#endif
