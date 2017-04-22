/*********************************************************************
   
   set_timevals.h
      
   ******************************************************************/

#ifndef SET_TIMEVALS_H
#define SET_TIEMVALS_H


#include "RpfParams.h"                 /* database table */
#include "GeneralUtil.h"               /* for get_apps_defaults */

void set_timevals(const	time_t	current_time,
		  	time_t	*obs_btime,
			time_t	*fcst_etime,
			time_t	*basis_btime);

void get_hrvals(time_t	*obshrs,
		time_t	*fcsthrs,
		time_t	*basishrs);
		
#endif
