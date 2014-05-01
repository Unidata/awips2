#ifndef GET_CUROBS_MAXFCST_H
#define GET_CUROBS_MAXFCST_H 

#include <time.h>

#include "Report.h"

void get_curobs_maxfcst(char		*lid,
			char 		*pe,
			int		obs_hours_ago,
			int		fcst_hours_ahead,
			int		fcst_basis_hours_ago,
			int		*obs_found,
			int		*fcst_found,
			struct Report	*obsReport,
			struct Report	*fcstReport);


#endif
