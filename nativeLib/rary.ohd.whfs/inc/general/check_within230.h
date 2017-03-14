#ifndef CHECK_WITHIN230_H
#define CHECK_WITHIN230_H

#define PI 3.1415926


/* need definition of the HRAP_ROWS and COLS */

#include "convert_hrap.h"


/* protos */

int compute_numbins230(double lat,
                       int *numbins_230);
                       
int incircle_check(int          numbins_230,
                   int          row,
                   int          col);

#endif
