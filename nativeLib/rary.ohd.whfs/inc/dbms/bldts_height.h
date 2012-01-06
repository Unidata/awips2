#ifndef BLDTS_HEIGHT_H
#define BLDTS_HEIGHT_H

#include "Report.h"
#include "LoadUnique.h"
#include "FcstHeight.h"


void bldts_obsriv(char 		*lid,
		  char		*pe,
		  char		*ts_filter,
		  int		use_qcfilter,
		  time_t	obs_btime,
		  time_t	obs_etime,
		  Report	**obs,
		  int		*num_obs);

void bldts_fcstriv(char		*lid,
		   char		*pe,
		   char 	*ts_filter,
		   int		use_qcfilter,
		   int		use_latest,
		   time_t	basis_btime,
		   Report 	**fcst,
		   int 		*num_fcst);

void set_fcst_keep(UniqueList	*ulHead,
		   FcstHeight	*fhHead,
		   int		*doKeep);

void adjust_startend(int	ul_count,
		     time_t	*basis_time,
		     time_t	*start_valid_time,
		     time_t	*end_valid_time);

void find_maxfcst(Report	*fcst_ts,
		  int		fcst_ts_cnt,
		  Report	*max_record);

#endif
