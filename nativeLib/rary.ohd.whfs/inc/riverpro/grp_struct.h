/************************************************************************
   grp_struct.h
   
   PURPOSE
   Contains information specific to a group of forecast points.
   
   NOTES
   The group information is always defined after the forecast
   point info, since the group information is somewhat dependent
   upon the forecast point groupings.
   Add new field rec_allpts_in_group  11/2002
  ***********************************************************************/

#ifndef GRP_STRUCT_H
#define GRP_STRUCT_H

#include "DbmsDefs.h"

typedef struct
{  
   /* static info. loaded in by load_grpdata() in get_fp_grp.c */
   
   char			id[LOC_ID_LEN + 1];
   char			name[GROUP_NAME_LEN + 1];
   int			numfps;
   int 			*fpindex;
   char                 rec_allpts_in_group[BOOL_LEN + 1];  


   /* dynamic info. determined from omf data.
      determined in compute_grp_county_info() in get_stages.c */
      
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
      
} grp_struct;
 

#endif
