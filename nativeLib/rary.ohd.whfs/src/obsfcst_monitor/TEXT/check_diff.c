#include "obsfcst_monitor.h"
#include "get_limits.h"
#include "DataLimits.h"
#include "LocDataLimits.h"

/*** return result = 1 if alarm
            result = 2 if alert 
            result = -1 if limits missing **/

int check_diff(FILE* logfp,
               Forecast* fcst, 
               Observation* obs, 
               struct obsfcst_options obsfcst_opts)
{
  int    result = 0;
  DataLimits   *limitsPtr = NULL;
  int          limits_found;

  limitsPtr = get_limits(obs->lid, obs->pe, obs->dur, obs->obstime, &limits_found);

  if(limits_found)
  {
       if(limitsPtr->alarm_diff_limit == MISSING_VAL && 
          limitsPtr->alert_diff_limit == MISSING_VAL)
	   result = -1;
       else if(fabs(fcst->value - obs->value)>= limitsPtr->alarm_diff_limit)
           result = 1;
       else if(fabs(fcst->value - obs->value)>= limitsPtr->alert_diff_limit)
           result = 2;
  }
  else
  {
       result = -1; /*** limits missing **/
  }

 return result;
}
