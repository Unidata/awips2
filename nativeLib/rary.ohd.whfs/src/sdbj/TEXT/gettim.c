#include <time.h>
#include "create_fortran_link.h"

create_fortran_link( int, gettim, (int *hours, int *minutes, int *seconds, int *millisec), (hours, minutes, seconds, millisec))
{
	time_t time_in_ticks = time(NULL);
 	struct tm *time_tm = gmtime(&time_in_ticks);
	*hours = time_tm -> tm_hour;
	*minutes = time_tm -> tm_min;
	*seconds = time_tm -> tm_sec;
	*millisec = 0;
        return 0;
}
