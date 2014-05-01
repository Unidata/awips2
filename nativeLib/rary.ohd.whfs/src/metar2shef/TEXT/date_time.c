#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "mtr.h"
#include "global_external.h"

void date_time(year,mon,day,hour,min,sec)
short int *mon, *day, *year, *hour, *min, *sec;
{
  time_t cal_time;
  struct tm *bd_time;
  cal_time = time(NULL);
  bd_time = gmtime(&cal_time);

/*  
   fprintf(stdout,"%02d %02d %02d %02d %02d\n"
        ,bd_time->tm_mon+1
        ,bd_time->tm_mday
        ,bd_time->tm_year
        ,bd_time->tm_hour
        ,bd_time->tm_min);
*/

 *year =   bd_time->tm_year + 1900; 
 *mon  =   bd_time->tm_mon +1;
 *day  =   bd_time->tm_mday;
 *hour =   bd_time->tm_hour;
 *min  =   bd_time->tm_min; 
 *mon  =   bd_time->tm_mon+1;
 *sec  =   bd_time->tm_sec; 
 
 buffer_.idate[0] = *year;
 buffer_.idate[1] = *mon;
 buffer_.idate[2] = *day;
 buffer_.idate[3] = *hour;
 buffer_.idate[4] = *min;
 buffer_.idate[5] = *sec;

 }
