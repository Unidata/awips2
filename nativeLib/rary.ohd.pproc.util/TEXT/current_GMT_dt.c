#include <time.h>
#include "time_convert.h"

/*----------------------------------------------------------------------------*/
/*   function to return current time (GMT) in dtime_t format                  */
/*----------------------------------------------------------------------------*/

int current_GMT_dt(dtime_t *GMTtime_dt)

{

time_t GMTtime_t=0;
int ret;

time(&GMTtime_t);
ret = timet_to_yearsec_dt(GMTtime_t, GMTtime_dt);

return ret;

}
