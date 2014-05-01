/************************************************************************
   county_struct.h
   
   PURPOSE
   Contains information specific to a group of forecast points.
   
   NOTES
   The county information is always defined after the forecast
   point info, since the county information is somewhat dependent
   upon the forecast point groupings.

  ***********************************************************************/

#ifndef COUNTY_STRUCT_H
#define COUNTY_STRUCT_H

#include "DbmsDefs.h"

typedef struct
{  
   /* static info. loaded in by load_cntydata() in get_fp_grp_county.c */
   
   char	                county[COUNTY_LEN + 1];
   char	                state[STATE_LEN + 1];
   char                 countynum[UGC_LEN + 1];
   int			numfps;
   int 			*fpindex;
   
   
   /* dynamic info, determined in compute_grp_county_info() in get_stages.c */
   
   int  	max_curobs_cat;
   time_t	max_curobs_time;
   
   int  	max_maxfcst_cat;
   time_t	max_maxfcst_time;
   
   int  	max_omf_cat;
   time_t	max_omf_time;
   
   
   /* dynamic info, needs whole time series to be determined.
      used for VTEC 2nd line */
   
   time_t 	obs_fallbelow_time;
   time_t 	obs_riseabove_time;
   
   time_t 	fcst_fallbelow_time;
   time_t 	fcst_riseabove_time;
   
   time_t	fallbelow_time;
   time_t	riseabove_time;

} county_struct;
 

#endif
