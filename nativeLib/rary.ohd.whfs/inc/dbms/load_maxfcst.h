#ifndef LOAD_MAXFCST_H
#define LOAD_MAXFCST_H 

#include <time.h>

#include "Report.h"
#include "set_timevals.h"


void load_maxfcst();

void load_max_fcstdata(char *tablename);

void load_max_fcstdata_lidpe(char 	*tablename,
			     char	*lid,
			     char	*pe);

void load_maxfcst_item(char 	*lid, 
		       char	*pe,
		       char	*ts);

void load_riverstatus(char	*lid,
		      Report 	report_record);

#endif
