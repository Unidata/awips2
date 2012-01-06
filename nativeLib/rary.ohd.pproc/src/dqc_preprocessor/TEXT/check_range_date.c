#include <time.h>
#include "time_convert.h"

/*******************************************************************

  Check if the data value's time is within the
  given day-of-the-year window.  [sql: extend(xxx, year to second)]

   ******************************************************************/
int  check_range_date(time_t    data_timet,
                      char *    start_ansi_monthday,
                      char *    end_ansi_monthday)
{

   int returnValue = 0;

   monthday_t data_monthday_t = timet_to_monthday_t(data_timet);

   monthday_t start_monthday_t = 0;
   monthday_t end_monthday_t = 0;

   ansi_monthday_to_monthday_t(start_ansi_monthday, &start_monthday_t);
   ansi_monthday_to_monthday_t(end_ansi_monthday, &end_monthday_t);

   if ((data_monthday_t >= start_monthday_t) &&
       (data_monthday_t <= end_monthday_t))
   {
       returnValue = 1;
   }
   else
   {
       returnValue = 0;
   }

   return (returnValue);
}
