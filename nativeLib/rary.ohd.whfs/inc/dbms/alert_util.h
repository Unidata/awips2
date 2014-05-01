#ifndef ALERT_UTIL_H
#define ALERT_UTIL_H 

#include <time.h>

#define UPPER_CHECKSTR "upper"
#define LOWER_CHECKSTR "lower"
#define DIFF_CHECKSTR "diff"
#define ROC_CHECKSTR   "roc"

#define ALERT_CATEGSTR "alert"
#define ALARM_CATEGSTR "alarm"

int purge_fcst_alerts(char *aa_check,
		      char *err_msg);


#endif
