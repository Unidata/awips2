#ifndef GET_LIMITS_H
#define GET_LIMITS_H 

#include <time.h>

#include "DbmsUtils.h"

#define MISSING_VAL -9999.

#include "DataLimits.h"
#include "LocDataLimits.h"

#include "time_convert.h"

DataLimits * get_limits(char 	*lid,
			char 	*pe,
			int  	dur,
			time_t	obstime,
			int     *limits_found);

int  check_date_range(time_t	data_timet,
		      char *	start_ansi_monthday,
		      char *	end_ansi_monthday);

#endif
