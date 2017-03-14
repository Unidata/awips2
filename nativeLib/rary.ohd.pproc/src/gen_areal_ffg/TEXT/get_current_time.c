
#include <time.h>

void get_current_time(int *iyear, int *imonth, int *iday,
                      int *ihour, int *imin)
{

/*-------------------------------------------------------------*/
/*  this routine generates the current date and time in GMT    */
/*  the current date and time are used with the lookback time  */
/*    to determine if the gridded FFG files are too old        */
/*-------------------------------------------------------------*/

   struct tm    *t_gmt;
   time_t       tnow;

   time(&tnow);
   t_gmt = gmtime(&tnow);

   *iyear = t_gmt->tm_year + 1900;
   *imonth = t_gmt->tm_mon + 1;
   *iday = t_gmt->tm_mday;
   *ihour = t_gmt->tm_hour;
   *imin = 0;

}
