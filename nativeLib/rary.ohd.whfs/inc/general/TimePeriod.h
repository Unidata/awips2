
#ifndef TIME_PERIOD_H
#define TIME_PERIOD_H

#include <time.h>
#include "time_defs.h"

typedef struct _TimePeriod
{
   time_t  beginTime;
   time_t  endTime;
   
   
} TimePeriod;


/*

*/

int isGoodTimePeriod(TimePeriod *timePeriod);
time_t getTimePeriodDurationInHours(TimePeriod *timePeriod);



#endif
