#ifndef ROC_CHECKER_H
#define ROC_CHECKER_H


#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "time_convert.h"
#include "alert_util.h"

#include "Observation.h"
#include "AlertAlarmVal.h"
#include "LocDataLimits.h"
#include "DataLimits.h"

#include "QualityCode.h"

#define MISSING_VAL -9999.
#define DEFAULT_HOURS 6

#define ROC_OKAY                      0
#define ROC_ALERT                     1
#define ROC_BAD                       2
#define ROC_ALARM                     3

#define USE_GOOD_DATA                 1
#define USE_GOOD_QUESTIONABLE_DATA    2


/************************************
  Declaration of function prototypes 
 ************************************/

void get_check_options(int		argc,
		       char		**argv,
                       char             *dbname,
		       char		*tablename,
		       time_t		*end_timet,     
		       int		*numhrs,
		       int		*show_errs_only,
                       int              *usedata_mode,
		       char		*lid,
		       char		*pe);

void get_data_limits(LocDataLimits      *LocDataLimitsHead,
                     DataLimits         *DataLimitsHead, 
                     Observation        *obsHead, 
                     double             *threshold_roc_max,
                     double             *threshold_alarm_roc_limit,
                     double             *threshold_alert_roc_limit); 



void get_timestamp(char *timestamp);

void do_roc_checker(char 	*tabname,
		    time_t	 endtime,
		    int		 numhrs,
		    char 	*lid,
		    char        *pe,
		    int		 show_errs_only, 
                    int         *usedata_mode, 
		    time_t	*tnow,
                    FILE        *logfilePtr); 

Observation * get_review_data(char 	*tabname,
			      time_t	 end_timet,
			      int	 numhrs,
                              int       *usedata_mode, 
			      char 	*lid,
			      char   	*pe, 
                              FILE      *logfilePtr); 

void perform_roc_checker(char  	        *tabname,
                         Observation 	*obsHead,
			 LocDataLimits	*LocDataLimitsHead,
			 DataLimits	*DataLimitsHead,
			 int		 show_errs_only, 
			 time_t	        *tnow,
                         FILE           *logfilePtr); 

Observation * process_lidpe_data(char 	        *tabname,
                                 Observation 	*obsHead,
				 LocDataLimits  *LocDataLimitsHead,
				 DataLimits     *DataLimitsHead,
				 int		 show_errs_only,
				 time_t		*tnow,
                                 FILE           *logfilePtr); 

void Write_AlertAlarmVal(Observation 	*obsPtr,
                         double		roc_value,
			 int 		 roc_status, 
			 time_t		*tnow,
			 FILE 		*logfilePtr);

#endif
